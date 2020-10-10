```python
# import library
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from tableone import TableOne
from sklearn.utils import resample
import random
import statistics as s

# Importing data set from https://doi.org/10.5061/dryad.d22q6vh with pandas in python
df = pd.read_excel("https://datadryad.org/stash/downloads/file_stream/30857")

# Renaming columns to be more representative names
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

# Renaming indexes: f to Female, m to Male, ICU 1 to Admission, ICU 0 to no admission. confusion 1 to Confusion, confusion 0 to no confusion.
df['Gender'] = df['Gender'].replace({'f': 'Female', 'm': 'Male'})
df['ICU admission'] = df['ICU admission'].replace({1: 'Admission', 0: 'No admission'})
df['Confusion'] = df['Confusion'].replace({1: 'Confusion', 0: 'No confusion'})

# Creating table of characterisitcs (we can remove p-vals and missing values if we want (just letting it be as it is right now)
columns = ['ICU admission', 'Age', 'Gender', 'Systolic blood pressure (mm Hg)', 'Confusion', 'Pulse (bpm)',
           'Respiratory rate (per min)', 'SpO2 (%)', 'Temperature (°C)']
groupby = 'country'
nonnormal = ['Respiratory rate (per min)', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)', 'Temperature (°C)',
             'SpO2 (%)']

# Table of characteristics presented as: pctable
pctable = TableOne(df, columns=columns, groupby=groupby, nonnormal=nonnormal, pval=True, htest_name=True)

# Renaming back indexes to original values just because my code is writton on that...
df['Gender'] = df['Gender'].replace({'Female': 'f', 'Male': 'm'})
df['ICU admission'] = df['ICU admission'].replace({'Admission': 1, 'No admission': 0})
df['Confusion'] = df['Confusion'].replace({'Confusion': 1, 'No confusion': 0})

# Splitting data based on country of origin
df_USA = df[df['country'] == 'USA']
df_France = df[df['country'] == 'France']
df_Switzerland = df[df['country'] == 'Switzerland']

# Assigning development sample and validation sample
combinations = [[df_USA, df_France], [df_USA, df_Switzerland], [df_France, df_USA], [df_Switzerland, df_USA], [df_Switzerland, df_France]]
name = ['USA_to_France', 'USA_to_Switzerland', 'France_to_USA', 'Switzerland_to_USA', 'Switzerland_to_France']

u = 0
while u < len(combinations):
    devsample = combinations[u][0]
    valsample = combinations[u][1]

    feature_cols = ['Respiratory rate (per min)', 'Confusion', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)',
                    'Temperature (°C)', 'SpO2 (%)']
    Xd = devsample[feature_cols]
    yd = devsample['ICU admission']

    # Developing prediction model with the development sample
    logreg = LogisticRegression().fit(Xd, yd)

    # Assigning independet variables and dependent variables to the validation sample
    Xv = valsample[feature_cols]
    yv = valsample['ICU admission']

    # Assigning "country of origin index" devsamp = 1, valsample = 0.
    devsample['devval'] = 1
    valsample['devval'] = 0

    # Pooling devsample and valsample
    pooled = pd.concat([devsample, valsample])

    # Assigning independent variables and dependent variables in the pooled sample
    Xp = pooled[feature_cols]
    yp = pooled['devval']

    # Developing propensity model based on variables in pooled sample
    propensity = LogisticRegression().fit(Xp, yp)

    # Predicting origin of data from pooled sample and creating a list
    yp_pred = propensity.predict(Xp).tolist()

    # Creating the true origin of data from pooled sample as a list
    yp_true = pooled['devval'].tolist()

    # Comparing predicted and true origin of data lists in order to identify missmatched development sample data in list: missmatch
    missmatch = []
    listn = list(range(0, len(devsample)))

    for i in listn:
        if yp_pred[i] == 0:
            missmatch.append(i)

    # Making new segment with only missmatched development samples
    df_segment = devsample.iloc[missmatch]

    # Assinging independent and dependent variables in segmented samples
    Xt = df_segment[feature_cols]
    yt = df_segment['ICU admission']

    # Creating new lists for point estimates (PE):
    # PE in development sample
    pe_acc_dev = []
    # PE in true validation sample
    pe_acc_tval = []
    # PE in predicted validation sample
    pe_acc_pval = []
    # PE in difference between true val and dev
    pe_tval_dev = []
    # PE in difference between predicted val and dev
    pe_pval_dev = []
    # PE in difference between the differences pred val dev true val dev.
    pe_diff_diff = []

    # PE accuracy in development sample added to PE list
    pe_acc_dev.append(logreg.score(Xd, yd) * 100)
    # PE accuracy in validation sample added to PE list
    pe_acc_tval.append(logreg.score(Xv, yv) * 100)
    # PE prediction of true accuracy in validation sample added to PE list
    pe_acc_pval.append(logreg.score(Xt, yt) * 100)
    # PE difference accuracy between true val and dev added to PE list
    pe_tval_dev.append(pe_acc_tval[0] - pe_acc_dev[0])
    # PE difference accuracy between pred val and dev added to PE list
    pe_pval_dev.append(pe_acc_pval[0] - pe_acc_dev[0])
    # PE difference accuracy between differences added to PE list
    pe_diff_diff.append(pe_pval_dev[0] - pe_tval_dev[0])

    # list of accuracys and differences that will be calculated from the bootstrapping
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

    # resample with replacement from devsample and valsample and do the exact same processes as before but with bootstrapped amount of times to develop 95% confidence intervalls
    # assigning amount of bootstraps performed (we are doing 1000 in our study, can be changed in order to just see if it works)
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
        feature_cols = ['Respiratory rate (per min)', 'Confusion', 'Systolic blood pressure (mm Hg)',
                        'Pulse (bpm)', 'Temperature (°C)', 'SpO2 (%)']
        Xd = devsamp[feature_cols]
        yd = devsamp['ICU admission']

        # Developing prediction model with the development sample
        # If error occurs, restarts iteration, if not it will continue with the process
        # Error is a value error, happens due to risk for only 1:s or 0:s as outcome value => cannot fit logistic regression with that.
        try:
            logreg = LogisticRegression().fit(Xd, yd)
        except:
            pass
        else:
            # Assigning independet variables and dependent variables to the validation sample
            Xv = valsamp[feature_cols]
            yv = valsamp['ICU admission']

            # Assigning "country of origin index" devsamp = 1, valsamp = 0.
            devsamp['devval'] = 1
            valsamp['devval'] = 0

            # Pooling devsamp and valsamp
            pooled = pd.concat([devsamp, valsamp])

            # Assigning independent variables and dependent variables in the pooled sample
            Xp = pooled[feature_cols]
            yp = pooled['devval']

            # Developing propensity model based on variables in pooled sample
            # If error occurs, restarts iteration, if not it will continue with the process
            # Error is a value error, happens due to risk for only 1:s or 0:s as outcome values => cannot fit logistic regression with that
            try:
                propensity = LogisticRegression().fit(Xp, yp)
            except:
                pass
            else:
                # Predicting origin of data from pooled sample and creating a list
                yp_pred = propensity.predict(Xp).tolist()

                # Creating the true origin of data from pooled sample as a list
                yp_true = pooled['devval'].tolist()

                # Comparing predicted and true origin of data lists in order to identify missmatched development sample data in list: missmatch
                missmatch = []
                listn = list(range(0, len(devsamp)))

                for i in listn:
                    if yp_pred[i] == 0:
                        missmatch.append(i)

                # If 0 missmatched development sample are identified an error will occur during score counting, will therefore
                if len(missmatch) == 0:
                    pass
                else:
                    # Making new segment with only missmatched development samples
                    df_segment = devsamp.iloc[missmatch]

                    # Assinging independent and dependent variables in segmented samples
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

    # Calculating difference between true performance accuracy and development accuracy (naive apporach) for each bootstrap and adding to: list_acc_diff_tval_dev
    # Calculating difference between predicted performance accuracy and development accuracy (segmented approach) for each bootstrap and adding to: list_acc_diff_pval_dev
    k = 0
    while k < bootstrap:
        list_acc_diff_tval_dev.append(list_acc_tval[k] - list_acc_dev[k])
        list_acc_diff_pval_dev.append(list_acc_pval[k] - list_acc_dev[k])
        list_acc_diff_diff.append(list_acc_diff_pval_dev[k] - list_acc_diff_tval_dev[k])
        k += 1

    # Creating lists for empirical bootstrap that we are going to choose 97,5 percentile and 2,5 percentile from
    # List for difference between PE dev and bootstrapped devs
    dev = []
    # List for difference between PE tval and bootstrapped tvals
    tval = []
    # List for difference between PE pval and bootstrapped pvals
    pval = []
    # List for difference between PE tval minus dev and bootstrapped tval minus devs (naive approach empirical difference)
    tvaldev = []
    # List for difference between PE pval minus dev and bootstrapped pval minus devs (segmented approach empirical difference)
    pvaldev = []
    # List for difference between segmented approach empirical difference and naive approach empirical difference
    diffdiff = []

    # Adding values to lists with differences between point estimates and bootstraps
    k = 0
    while k < bootstrap:
        dev.append(list_acc_dev[k] - pe_acc_dev[0])
        tval.append(list_acc_tval[k] - pe_acc_tval[0])
        pval.append(list_acc_pval[k] - pe_acc_pval[0])
        tvaldev.append(list_acc_diff_tval_dev[k] - pe_tval_dev[0])
        pvaldev.append(list_acc_diff_pval_dev[k] - pe_pval_dev[0])
        diffdiff.append(list_acc_diff_diff[k] - pe_diff_diff[0])
        k += 1

    # Sorting lists with differences between point estimates and bootstraps
    dev.sort()
    tval.sort()
    pval.sort()
    tvaldev.sort()
    pvaldev.sort()
    diffdiff.sort()

    # Assigning 97,5th percentile index
    percentile975 = int((len(list_acc_dev)) * 0.975 - 1)
    # Assigning 2,5th percentile index
    percentile25 = int((len(list_acc_dev)) * 0.025)

    # Assinging upper and lower confidence intervals for development sample accuracy (u = upper, l = lower)
    ci_dev_u = round(pe_acc_dev[0] - dev[percentile25], 2)
    ci_dev_l = round(pe_acc_dev[0] - dev[percentile975], 2)
    # Assinging upper and lower confidence intervals for true validation sample accuracy (u = upper, l = lower)
    ci_tval_u = round(pe_acc_tval[0] - tval[percentile25], 2)
    ci_tval_l = round(pe_acc_tval[0] - tval[percentile975], 2)
    # Assinging upper and lower confidence intervals for predicted validation sample accuracy (u = upper, l = lower)
    ci_pval_u = round(pe_acc_pval[0] - pval[percentile25], 2)
    ci_pval_l = round(pe_acc_pval[0] - pval[percentile975], 2)
    # Assinging upper and lower confidence intervals for difference between true validation accuracy and development sample accuracy (u = upper, l = lower)
    ci_tval_dev_u = round(pe_tval_dev[0] - tvaldev[percentile25], 2)
    ci_tval_dev_l = round(pe_tval_dev[0] - tvaldev[percentile975], 2)
    # Assinging upper and lower confidence intervals for difference between predicted validation accuracy and development sample accuracy (u = upper, l = lower)
    ci_pval_dev_u = round(pe_pval_dev[0] - pvaldev[percentile25], 2)
    ci_pval_dev_l = round(pe_pval_dev[0] - pvaldev[percentile975], 2)
    # Assinging upper and lower confidence intervals for difference between the 2 previous differences described (u = upper, l = lower)
    ci_diff_diff_u = round(pe_diff_diff[0] - diffdiff[percentile25], 2)
    ci_diff_diff_l = round(pe_diff_diff[0] - diffdiff[percentile975], 2)

    # developing dataframe of confidence intervals and means in order to make markdown table with them
    data = {
        'Point estimate': [str(round(pe_acc_dev[0], 2)), str(round(pe_acc_tval[0], 2)), str(round(pe_acc_pval[0], 2)),
                           str(round(pe_tval_dev[0], 2)), str(round(pe_pval_dev[0], 2)),
                           str(round(pe_diff_diff[0], 2))],
        '95% Confidence Interval': ['[' + str(ci_dev_l) + ', ' + str(ci_dev_u) + ']',
                                    '[' + str(ci_tval_l) + ', ' + str(ci_tval_u) + ']',
                                    '[' + str(ci_pval_l) + ', ' + str(ci_pval_u) + ']',
                                    '[' + str(ci_tval_dev_l) + ', ' + str(ci_tval_dev_u) + ']',
                                    '[' + str(ci_pval_dev_l) + ', ' + str(ci_pval_dev_u) + ']',
                                    '[' + str(ci_diff_diff_l) + ', ' + str(ci_diff_diff_u) + ']']}

    dfci = pd.DataFrame(data,
                        columns=['Point estimate', '95% Confidence Interval'],
                        index=['1: Prediction model accuracy in development sample',
                               '2: Prediction model accuracy in validation sample',
                               '3: Prediction model accuracy in segmented sample',
                               '4: Accuracy difference between 2 and 1 (naive approach)',
                               '5: Accuracy difference between 3 and 1 (segmented approach)',
                               '6: Accuracy difference between 5 and 4 (approach difference)'])

    vars()[name[u]] = dfci

    u += 1

# printing patient characterisitcs
print('Patient Characteristics')
print(pctable.tabulate(tablefmt="Markdown"))
print(' ')
# printing USA transfer to France
print(name[0])
print(USA_to_France.to_markdown())
print(' ')
# printing USA transfer to Switzerland
print(name[1])
print(USA_to_Switzerland.to_markdown())
print(' ')
# printing France to USA
print(name[2])
print(France_to_USA.to_markdown())
print(' ')
# printing Switzerland to USA
print(name[3])
print(Switzerland_to_USA.to_markdown())
print(' ')
# printing Switzerland to France
print(name[4])
print(Switzerland_to_France.to_markdown())

