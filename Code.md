---
title: Code
---
# All code is written in python with IDE Pycharm.

## Week 1  
#### importing data set as dataframe to python  
import pandas as pd  
df = pd.read_excel(r'C:\Users\Arman\Downloads\NEWS_datafile.xls')  

#### Removing NAN values from dataframe
df = df.dropna()

### adding NEWS value 
sample = df  
listNEWSvalue = []  
i = 0  
f = 0  
while i < len(sample):
    if sample['resp_rate'].iloc[i] <= 8:
        f += 3
    elif sample['resp_rate'].iloc[i] <= 11:
        f += 1
    elif sample['resp_rate'].iloc[i] <= 20:
        f += 0
    elif sample['resp_rate'].iloc[i] <= 24:
        f += 2
    elif sample['resp_rate'].iloc[i] >= 25:
        f += 3

    if sample['SpO2'].iloc[i] <= 91:
        f += 3
    elif sample['SpO2'].iloc[i] <= 93:
        f += 2
    elif sample['SpO2'].iloc[i] <= 95:
        f += 1
    elif sample['SpO2'].iloc[i] >= 96:
        f += 0


    if sample['BPS'].iloc[i] <= 90:
        f += 3
    elif sample['BPS'].iloc[i] <= 100:
        f += 2
    elif sample['BPS'].iloc[i] <= 110:
        f += 1
    elif sample['BPS'].iloc[i] <= 219:
        f += 0
    elif sample['BPS'].iloc[i] >= 220:
        f += 3

    if sample['HR'].iloc[i] <= 40:
        f += 3
    elif sample['HR'].iloc[i] <= 50:
        f += 1
    elif sample['HR'].iloc[i] <= 90:
        f += 0
    elif sample['HR'].iloc[i] <= 110:
        f += 1
    elif sample['HR'].iloc[i] <= 130:
        f += 2
    elif sample['HR'].iloc[i] >= 131:
        f += 3

    if sample['confusion'].iloc[i] == 1:
        f += 3
    else:
        f += 0

    if sample['temp'].iloc[i] <= 35:
        f += 3
    elif sample['temp'].iloc[i] <= 36:
        f += 1
    elif sample['temp'].iloc[i] <= 38:
        f += 0
    elif sample['temp'].iloc[i] <= 39:
        f += 1
    elif sample['temp'].iloc[i] >= 39.1:
        f += 2

    if (sample['resp_rate'].iloc[i] <= 8 or sample['resp_rate'].iloc[i] >= 25 or sample['SpO2'].iloc[i] <= 91 or sample['BPS'].iloc[i] <= 90 or sample['BPS'].iloc[i] >= 220 or sample['HR'].iloc[i] <= 40 or sample['HR'].iloc[i] >= 131 or sample['confusion'].iloc[i] == 1 or sample['temp'].iloc[i] <= 35) and f < 7:
        f = 6

    listNEWSvalue.append(f)
    f = 0
    i += 1

df['NEWS'] = listNEWSvalue

### turning NEWS value to dummies of high, moderate and low
listNEWS = []
for i in listNEWSvalue:
    if i <= 4:
        listNEWS.append('Low')
    elif i <= 6:
        listNEWS.append('Moderate')
    elif i >= 7:
        listNEWS.append('High')

df['NEWS-S'] = listNEWS

dummy = pd.get_dummies(df['NEWS-S'])
df = df.merge(dummy, left_index=True, right_index=True)
        

#### splitting dataframe based on country where the participant was registered  
df_USA = df[df['country'] == 'USA']  
df_France = df[df['country'] == 'France']  
df_Switzerland = df[df['country'] == 'Switzerland']  

## Week 2




## Week 3
### Specifying X and y value  
feature_cols = ['resp_rate', 'confusion', 'BPS', 'HR', 'temp', 'SpO2', 'LOS', 'PCT', 'MR-proADM']  
X = df_USA[feature_cols]  
y = df_USA['ICU']  

