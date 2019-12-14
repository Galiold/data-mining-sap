# %% import libraries
from subprocess import check_output
import pandas as pd
import numpy as nd
import matplotlib.pyplot as plt
import seaborn as sns

sns.set_style('whitegrid')

print(check_output(["ls", "Data"]).decode('utf8'))

# %% read data
data = pd.read_csv('Data/SAP_normal.csv')
data.head()
data.info()

# %%
# fig = plt.subplots(figsize=(10, 5))
sns.regplot(x='StudentAbsenceDays', y='Class', data=data)
# sns.regplot(x='AnnouncementsView', y='Discussion', data=data, ax=axis2)
# plt.show()


#%%
import pywt

# %%
a, b = pywt.dwt([1, 3, 5, 11, 12, 13, 0, 1], wavelet='haar')
print(a)
