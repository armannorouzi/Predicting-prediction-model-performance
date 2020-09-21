---
title: Code
---
# All code is written in python

## Week 1 
### importing data set as dataframe to python
import pandas as pd
df = pd.read_excel(r'C:\Users\Arman\Downloads\NEWS_datafile.xls')

### splitting dataframe based on country where the participant was registered
df_USA = df[df['country'] == 'USA']
df_France = df[df['country'] == 'France']
df_Switzerland = df[df['country'] == 'Switzerland']



### Specifying X and y value
feature_cols = ['resp_rate', 'confusion', 'BPS', 'HR', 'temp', 'SpO2', 'LOS', 'PCT', 'MR-proADM']
X = df_USA[feature_cols]
y = df_USA['ICU']

