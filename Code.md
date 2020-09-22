# All code written in python with IDE pycharm. Open raw format of this md file in order to see code more clearly...

#### import library
import numpy as np
import pandas as pd

#### importing data set from https://doi.org/10.5061/dryad.d22q6vh with pandas in python
df = pd.read_excel(r'C:\Users\Arman\Downloads\NEWS_datafile.xls')
df = df.dropna()

#### Splitting data based on country of origin
df_USA = df[df['country'] == 'USA']
df_France = df[df['country'] == 'France']
df_Switzerland = df[df['country'] == 'Switzerland']

