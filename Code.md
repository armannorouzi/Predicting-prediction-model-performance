#### import library
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from tableone import TableOne
from sklearn.utils import resample
import random
import statistics as s

#### importing data set from https://doi.org/10.5061/dryad.d22q6vh with pandas in python
df = pd.read_excel("https://datadryad.org/stash/downloads/file_stream/30857")
# df = df.dropna()

df.rename(columns={'resp_rate': 'Respiratory rate (per min)',
                   'BPS': 'Systolic blood pressure (mm Hg)',
                   'HR': 'Pulse (bpm)',
                   'temp': 'Temperature (°C)',
                   'SpO2': 'SpO2 (%)',
                   'confusion': 'Confusion',
                   'gender': 'Gender',
                   'age': 'Age',
                   'ICU': 'ICU admission'},
          inplace=True)

### renaming index f to female, m to male
df['Gender'] = df['Gender'].replace({'f': 'female', 'm': 'male'})

### creating table of characterisitcs (we can remove p-vals and missing values if we want (just letting it be as it is right now)
columns = ['ICU admission', 'Age', 'Gender', 'Systolic blood pressure (mm Hg)', 'Confusion', 'Pulse (bpm)',
           'Respiratory rate (per min)', 'SpO2 (%)', 'Temperature (°C)']
groupby = 'country'
nonnormal = ['Respiratory rate (per min)', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)', 'Temperature (°C)',
             'SpO2 (%)']
mytable = TableOne(df, columns=columns, groupby=groupby, nonnormal=nonnormal, pval=True, pval_test_name=True)

#### Splitting data based on country of origin
df_USA = df[df['country'] == 'USA']
df_France = df[df['country'] == 'France']
df_Switzerland = df[df['country'] == 'Switzerland']

### assigning development sample and validation sample
devsample = df_France
valsample = df_USA

### list of accuracys

# 1: accuracy in development sample
list_acc_dev = []
# 2: true accuracy in validation sample
list_acc_tval = []
# 3: predicted accuracy in validation sample with segment
list_acc_pval = []
# 4: accuracy difference between 2 and 1 (Naive approach)
list_acc_diff_tval_dev = []
# 5: accuracy difference between 3 and 1 (Segmented approach)
list_acc_diff_pval_dev = []
# 6: accuracy difference between 5 and 4 (Approach difference)
list_acc_diff_diff = []

### assigning X and y values
feature_cols = ['Respiratory rate (per min)', 'Confusion', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)',
                'Temperature (°C)', 'SpO2 (%)']
Xd = devsample[feature_cols]
yd = devsample['ICU admission']

### resample with replacement from devsample and valsample and do the exact processes bootstrapped amount of times to develop 95% confidence intervalls
### assigning bootstrap numbers
bootstrap = 1000

### while looping everything to be able to boostrap confidence intervalls
z = 0
while z < bootstrap:
    # assigning random interger to resample seed
    print(z)
    randint = random.randint(1, 1000000)

    # creating new resamples of development sample and validation sample
    devsamp = resample(devsample, n_samples=len(devsample), replace=True, random_state=randint)
    valsamp = resample(valsample, n_samples=len(valsample), replace=True, random_state=randint)

    # assigning independent variables and dependent variables to the development sample
    feature_cols = ['Respiratory rate (per min)', 'Confusion', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)',
                    'Temperature (°C)', 'SpO2 (%)']
    Xd = devsamp[feature_cols]
    yd = devsamp['ICU admission']

    # developing prediction model with the development sample (If error, pass and restart iteration) 
    # error = value error, due to risk for only 0:s or 1:s as y values during resampling(outcomes) = cant fit prediction model)
    try:
        logreg = LogisticRegression().fit(Xd, yd)
    except:
        pass
    else:
        # assigning independet variables and dependent variables to the validation sample
        Xv = valsamp[feature_cols]
        yv = valsamp['ICU admission']

        # assigning "country of origin index" devsamp = 1, valsamp = 0.
        devsamp['devval'] = 1
        valsamp['devval'] = 0

        # pooling devsamp and valsamp
        pooled = pd.concat([devsamp, valsamp])

        # assigning independent variables and dependent variables in the pooled sample
        Xp = pooled[feature_cols]
        yp = pooled['devval']

        # developing propensity model based on variables in pooled sample (If error, pass and restart iteration) 
        # error = value error, due to risk for only 0:s or 1:s as y values during resampling (outcomes) = cant fit propensity model = error
        try:
            propensity = LogisticRegression().fit(Xp, yp)
        except:
            pass
        else:
            # predicting origin of data from pooled sample and creating a list
            yp_pred = propensity.predict(Xp).tolist()

            # creating the true origin of data from pooled sample as a list
            yp_true = pooled['devval'].tolist()

            # comparing predicted and true origin of data lists in order to identify missmatched development sample data in list: missmatch
            missmatch = []
            listn = list(range(0, len(devsamp)))

            for i in listn:
                if yp_pred[i] == 0:
                    missmatch.append(i)
                    
            
            # if 0 missmatched development sample = error during score calculation => passes and restarts iteration if 0 missmatches
            if len(missmatch) == 0:
                pass
            else:
                # making new segment with only missmatched development samples
                df_segment = devsamp.iloc[missmatch]

                # assinging independent and dependent variables in segmented samples
                Xt = df_segment[feature_cols]
                yt = df_segment['ICU admission']

                # Predicting performance in development sample and storing accuracy in: list_acc_dev
                list_acc_dev.append(logreg.score(Xd, yd) * 100)

                # "true" performance predicted in validation sample and storing accuracy in: list_acc_tval
                list_acc_tval.append(logreg.score(Xv, yv) * 100)

                # "predicted" performance of prediction model in validation sample based on prediction made on segmented sample and storing accuracy in: list_acc_pval
                list_acc_pval.append(logreg.score(Xt, yt) * 100)

                # rerunning itterations untill satistifed with bootstrap
                z += 1

# calculating difference between true performance accuracy and development accuracy (naive apporach) as: list_acc_diff_tval_dev
# calculating difference between predicted performance accuracy and development accuracy (segmented approach) as: list_acc_diff_pval_dev
k = 0
for i in list_acc_dev:
    list_acc_diff_tval_dev.append(list_acc_tval[k] - i)
    list_acc_diff_pval_dev.append(list_acc_pval[k] - i)
    k += 1

# calculating difference between differences (segmented approach - naive approach) as: list_acc_diff_pval_dev
k = 0
for i in list_acc_diff_pval_dev:
    list_acc_diff_diff.append(i - list_acc_diff_tval_dev[k])
    k += 1

# sorting accuracys in order to draw confidence intervals
list_acc_dev.sort()
list_acc_tval.sort()
list_acc_pval.sort()
list_acc_diff_tval_dev.sort()
list_acc_diff_pval_dev.sort()
list_acc_diff_diff.sort()

# identifying upper and lower index for confidence intervals
ucii = int((len(list_acc_dev)) * 0.975 - 1)
lcii = int((len(list_acc_dev)) * 0.025)

# assigning confidence intervals for the accuracys and differences

# development sample accuracy upper and lower confidence intervals (u = upper (97,5%), l = lower (2,5%)
ci_dev_u = list_acc_dev[ucii]
ci_dev_l = list_acc_dev[lcii]
# true validation sample accuracy upper and lower confidence intervals (u = upper (97,5%), l = lower (2,5%)
ci_tval_u = list_acc_tval[ucii]
ci_tval_l = list_acc_tval[lcii]
# predicted validation sample accuracy upper and lower confidence intervals (u = upper (97,5%), l = lower (2,5%)
ci_pval_u = list_acc_pval[ucii]
ci_pval_l = list_acc_pval[lcii]
# naive apporach accuracy difference upper and lower confidence intervals (u = upper (97,5%), l = lower (2,5%)
ci_tval_dev_u = list_acc_diff_tval_dev[ucii]
ci_tval_dev_l = list_acc_diff_tval_dev[lcii]
# segmented approach accuracy difference upper and lower confidence intervals (u = upper (97,5%), l = lower (2,5%)
ci_pval_dev_u = list_acc_diff_pval_dev[ucii]
ci_pval_dev_l = list_acc_diff_pval_dev[lcii]
# difference between semgented approach difference and naive approach difference upper and lower confidence intervals (u = upper (97,5%), l = lower (2,5%)
ci_diff_diff_u = list_acc_diff_diff[ucii]
ci_diff_diff_l = list_acc_diff_diff[lcii]

# developing dataframe of confidence intervals and means in order to make markdown table with them
data = {'Mean': [str(s.mean(list_acc_dev)), str(s.mean(list_acc_tval)), str(s.mean(list_acc_pval)),
                 str(s.mean(list_acc_diff_tval_dev)),
                 str(s.mean(list_acc_diff_pval_dev)), str(s.mean(list_acc_diff_diff))],
        '2,5%': [str(ci_dev_l), str(ci_tval_l), str(ci_pval_l), str(ci_tval_dev_l), str(ci_pval_dev_l),
                 str(ci_diff_diff_l)],
        '97,5%': [str(ci_dev_u), str(ci_tval_u), str(ci_pval_u), str(ci_tval_dev_u), str(ci_pval_dev_u),
                  str(ci_diff_diff_u)]}

dfci = pd.DataFrame(data, columns=['Mean', '2,5%', '97,5%'],
                    index=['1: Prediction model accuracy in development sample',
                           '2: Prediction model accuracy in validation sample',
                           '3: Prediction model accuracy in segmented sample',
                           '4: Accuracy difference between 2 and 1 (naive approach)',
                           '5: Accuracy difference between 3 and 1 (segmented approach)',
                           '6: Accuracy difference between 5 and 4 (approach difference)'])

# printing characteristic table as markdown format
print(mytable.tabulate(tablefmt="markdown"))
# printing confidence intervals and mean? (didn't really know what to use here maybe median is better idk..)
print(print(dfci.to_markdown()))
