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

