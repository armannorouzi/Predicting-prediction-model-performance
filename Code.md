# All code written in python, IDE pycharm. Open in raw format to access the code.

### import library
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression

### importing data set from https://doi.org/10.5061/dryad.d22q6vh with pandas in python
df = pd.read_excel("https://datadryad.org/stash/downloads/file_stream/30857")
df = df.dropna()

### Splitting data based on country of origin
df_USA = df[df['country'] == 'USA']
df_France = df[df['country'] == 'France']
df_Switzerland = df[df['country'] == 'Switzerland']

### Assigning which data is development setting 'dsetting' and which data is transfer setting = 'tsetting'
dsetting = df_France
tsetting = df_USA

### Developing prediction model with logistic regression in the development setting, predictor = feature_cols, outcome = ICU
feature_cols = ['resp_rate', 'confusion', 'BPS', 'HR', 'temp', 'SpO2']
Xd = dsetting[feature_cols]
yd = dsetting['ICU']

logreg = LogisticRegression().fit(Xd, yd)

### Calculating accuracy in development setting with developed prediction model (as 'scored')
scored = logreg.score(Xd, yd)

### Calculating true performance of model in transfer setting (as 'scoret')
Xt = tsetting[feature_cols]
yt = tsetting['ICU']

scoret = logreg.score(Xt, yt)

### Developing propensity model that will find missmatched development setting participants
### 1) assigning devval index 1 to development setting, and devval index 0 to transfer setting
dsetting['devval'] = 1
tsetting['devval'] = 0

### 2) pooling data from development setting and transfer setting
df_pooled = pd.concat([dsetting, tsetting])

### 3) Creating propensity model to distinguish which sample is from which setting
Xc = df_pooled[feature_cols]
yc = df_pooled['devval']
propensitymodel = LogisticRegression().fit(Xc, yc)

### 4) running propensity model to create predictions of which sample the pooled prediction variables belong to (as 'yc_pred')
yc_pred = propensitymodel.predict(Xc).tolist()

### 5) comparing predicted setting to real setting and returning the iloc index for that missmatched development setting participant (as 'listmissmatch')
yc_true = df_pooled['devval'].tolist()

listn = list(range(0, len(dsetting)))
listmissmatch = []

for i in listn:
    if yc_pred[i] == 0:
        listmissmatch.append(i)

### segmenting the missmatched development setting participants
df_segment = dsetting.iloc[listmissmatch]

### estimating true performance in transfer setting by utilizing logreg model previously developed and the segment that has been created in the step above (as 'predict_scoret')
Xs = df_segment[feature_cols]
ys = df_segment['ICU']

predict_scoret = logreg.score(Xs, ys)

### scored = prediction model performance in development setting
### scoret = prediction model performance in transfer setting with outcome data
### predict_socret = predicted performance in transfer setting with only predictor data

print(scored)
print(scoret)
print(predict_scoret)






