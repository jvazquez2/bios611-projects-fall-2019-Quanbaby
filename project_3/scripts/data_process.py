# Packages required

import pandas as pd
import numpy as np
import math
from datetime import datetime
import statsmodels.api as sm
import seaborn as sns
from pylab import savefig
import matplotlib.pyplot as plt
plt.switch_backend('agg')

# Read the dataset
# Read the dataset abount client summary

client1 = pd.read_csv("../data/CLIENT_191102.tsv", sep="\t")
client1.head()

# Read another datasets where exact entry and exit dates were recorded

client2 = pd.read_csv("../data/ENTRY_EXIT_191102.tsv", sep="\t", na_values=" ")
client2.head()

# Read dataset about income at entry and exit

income_entry = pd.read_csv("../data/INCOME_ENTRY_191102.tsv", sep="\t", na_values=" ")
income_exit = pd.read_csv("../data/INCOME_EXIT_191102.tsv", sep="\t", na_values=" ")

# Read dataset about insurance at entry and exit

insurance_entry = pd.read_csv("../data/HEALTH_INS_ENTRY_191102.tsv", sep="\t", na_values=" ")
insurance_exit = pd.read_csv("../data/HEALTH_INS_EXIT_191102.tsv", sep="\t", na_values=" ")

# Read dataset about non-cash resources at entry and exit

noncash_entry = pd.read_csv("../data/NONCASH_ENTRY_191102.tsv", sep="\t", na_values=" ")
noncash_exit = pd.read_csv("../data/NONCASH_EXIT_191102.tsv", sep="\t", na_values=" ")

# Read dataset about disabilities at entry and exit

disability_entry = pd.read_csv("../data/DISABILITY_ENTRY_191102.tsv", sep="\t", na_values=" ")
disability_exit = pd.read_csv("../data/DISABILITY_EXIT_191102.tsv", sep="\t", na_values=" ")

# Pre-process the data

# Create another variable to calculate the roughly time spent in the shelter according to their entry and exit ages
# The data is catogorical

a=[]
for x in range(1, client1.shape[0]+1):
    if math.isnan(client1['Client Age at Exit'][x-1] - client1['Client Age at Entry'][x-1]) == True: # na values
        a = a + ['nan']
    else:
        if client1['Client Age at Exit'][x-1] - client1['Client Age at Entry'][x-1] == float(0):
            a = a + ['<1']
        else:
            a = a + [str(int(client1['Client Age at Exit'][x-1] - client1['Client Age at Entry'][x-1])) + '+' ]
client1['Time Spent (year)'] = a

# Create another variable to calculate days spent in the shelter using entry and exit dates

a=[]
for x in range(1,client2.shape[0]+1):
    if type(client2['Exit Date'][x-1]) != str or type(client2['Entry Date'][x-1]) != str: # na values
        a = a + ['nan']
    else:
        a = a + [(datetime.strptime(client2['Exit Date'][x-1], '%m/%d/%Y') - datetime.strptime(client2['Entry Date'][x-1], '%m/%d/%Y')).days]
client2['Time Spent (days)'] = a

# Calculate the total income at entry and exit for each client

income_entry_sum = income_entry.groupby('Client ID')['Monthly Amount (Entry)'].apply(lambda x: x.sum()).reset_index(name='Monthly Amount (Entry)')
income_entry_sum.fillna(0, inplace = True)
income_exit_sum = income_exit.groupby('Client ID')['Monthly Amount (Exit)'].apply(lambda x: x.sum()).reset_index(name='Monthly Amount (Exit)')
income_exit_sum.fillna(0, inplace = True)

# Calculate numbers of insurances each client had at entry and exit

insurance_entry_sum = insurance_entry.groupby('Client ID')['Covered (Entry)'].apply(lambda x: (x=='Yes').sum()).reset_index(name='Insurance (Entry)')
insurance_exit_sum = insurance_exit.groupby('Client ID')['Covered (Exit)'].apply(lambda x: (x=='Yes').sum()).reset_index(name='Insurance (Exit)')

# Calculate numbers of noncash resources each client had at entry and exit

noncash_entry_sum = noncash_entry.groupby('Client ID')['Receiving Benefit (Entry)'].apply(lambda x: (x=='Yes').sum()).reset_index(name='Noncash (Entry)')
noncash_exit_sum = noncash_exit.groupby('Client ID')['Receiving Benefit (Exit)'].apply(lambda x: (x=='Yes').sum()).reset_index(name='Noncash (Exit)')

# Calculate numbers of disabilities each client had at entry and exit

disability_entry_sum = disability_entry.groupby('Client ID')['Disability Determination (Entry)'].apply(lambda x: (x=='Yes').sum()).reset_index(name='Disability (Entry)')
disability_exit_sum = disability_exit.groupby('Client ID')['Disability Determination (Exit)'].apply(lambda x: (x=='Yes').sum()).reset_index(name='Disability (Exit)')



# Merge the final dataset we will work with

client_v1 = pd.merge(client1, client2[['Client ID', 'Time Spent (days)']], on = ['Client ID'], how = 'left')
client_v2 = pd.merge(client_v1, income_entry_sum, on = ['Client ID'], how = 'left')
client_v3 = pd.merge(client_v2, income_exit_sum, on = ['Client ID'], how = 'left')
client_v4 = pd.merge(client_v3, insurance_entry_sum, on = ['Client ID'], how = 'left')
client_v5 = pd.merge(client_v4, insurance_exit_sum, on = ['Client ID'],  how = 'left')
client_v6 = pd.merge(client_v5, noncash_entry_sum, on = ['Client ID'],  how = 'left')
client_v7 = pd.merge(client_v6, noncash_exit_sum, on = ['Client ID'],  how = 'left')
client_v8 = pd.merge(client_v7, disability_entry_sum, on = ['Client ID'],  how = 'left')
client_final = pd.merge(client_v8, disability_exit_sum, on = ['Client ID'], how = 'left')

client_final.to_csv('../data/clients_final.tsv', sep = '\t')

# Some summary statistics

# A total summary about number of clients for each catogory

client1.groupby(['EE Provider ID', 'Client Gender', 'Client Primary Race']).count()

# Summary about number of clients with each EE provider

client1.groupby(['EE Provider ID']).count()['Client Unique ID']

# Summary about number of clients with each gender

client1.groupby(['Client Gender']).count()['Client Unique ID']

# Summary about number of clients with each race

client1.groupby(['Client Primary Race']).count()['Client Unique ID']

# Summary about number of clients with each ethnicity

client1.groupby(['Client Ethnicity']).count()['Client Unique ID']

# Summary about number of clients with veteran status

client1.groupby(['Client Veteran Status']).count()['Client Unique ID']

# Summary about time spent catogory for each client 

client1.groupby(['Time Spent (year)']).count()['Client Unique ID']

# Reasons that people left

client2.groupby(['Destination']).count()['Client ID'].sort_values(ascending = False)


# Specific analysis to individual client
# Who came most often
# Groupby client ID and then count the numbers of each group

client_freq = client1.groupby(['Client ID']).count()['Client Unique ID'].sort_values(ascending = False)
client_freq.head()

# Who stayed the longest
# Find non-NA's in the 'Time Spent (days)' column and then sort the values

a = list()
for x in range(0,client2.shape[0]):
    if type(client2['Time Spent (days)'][x])==int:
        a = a + [x] # find non-NA's
client_stay = client2.iloc[a].sort_values(by = ['Time Spent (days)'], ascending = False)[['Client ID','Time Spent (days)']]
client_stay.head()

# Relationship between staying days and other factors
# Fit a regression model to decide which variables are significant to staying days
# The explanotory variables are: age at Entry, gender, race, veteran status, income at entry, insurance at entry, 
#   non cash services at entry, disabilities at entry

# Drop all the NA values
final_nona = client_final[['Time Spent (days)', 'Client Age at Entry', 'Client Gender', 'Client Primary Race', 
                                'Client Veteran Status', 'Monthly Amount (Entry)','Insurance (Entry)', 
                           'Noncash (Entry)', 'Disability (Entry)']].dropna()

# Create dummy variables and replace string with the values
# For gender:
final_nona.loc[final_nona['Client Gender'] == 'Female', 'Client Gender'] = 0
final_nona.loc[final_nona['Client Gender'] == 'Male', 'Client Gender'] = 1
final_nona.loc[final_nona['Client Gender'] == 'Trans Female (MTF or Male to Female)', 'Client Gender'] = 0.5

# For race:
# The main races are white and black, and the numbers of other races are very small. 
# Other races are in one group.
# Create 2 dummy variables for race.

final_nona['Client Primary Race2'] = 0

final_nona.loc[final_nona['Client Primary Race'] == 'White (HUD)', 'Client Primary Race2'] = 0
final_nona.loc[final_nona['Client Primary Race'] == 'White (HUD)', 'Client Primary Race'] = 0

final_nona.loc[final_nona['Client Primary Race'] == 'Black or African American (HUD)', 'Client Primary Race2'] = 1
final_nona.loc[final_nona['Client Primary Race'] == 'Black or African American (HUD)', 'Client Primary Race'] = 0

final_nona.loc[final_nona['Client Primary Race'] != 0, 'Client Primary Race2'] = 0
final_nona.loc[final_nona['Client Primary Race'] != 0, 'Client Primary Race'] = 1


# For veteran status:
final_nona.loc[final_nona['Client Veteran Status'] == 'No (HUD)', 'Client Veteran Status'] = 0
final_nona.loc[final_nona['Client Veteran Status'] == 'Yes (HUD)', 'Client Veteran Status'] = 1
final_nona.drop(final_nona[final_nona['Client Veteran Status'] == 'Data not collected (HUD)'].index, inplace = True) 

# Replace INF to NA and then drop NA
final_nona = final_nona.apply(pd.to_numeric, args=('coerce',))
final_nona.replace([np.inf, -np.inf], np.nan)
final_nona.dropna(inplace = True)

# Choose variables by calculating the pearson correlation matrix
correlation = final_nona.corr(method='pearson')
columns = correlation.nlargest(7, 'Time Spent (days)').index
columns

# Plot the pearson correlation matrix
correlation_map = np.corrcoef(final_nona[columns].values.T)
sns.set(font_scale=1.0)
heatmap = sns.heatmap(correlation_map, cbar=True, annot=True, square=True, fmt='.2f', yticklabels=columns.values, xticklabels=columns.values)

figure = heatmap.get_figure()
figure.set_size_inches(15, 15)
figure.savefig('../results/heatmap.png')

# Specify our x and y
X = final_nona[['Client Age at Entry', 'Noncash (Entry)',
       'Client Primary Race2', 'Client Primary Race', 'Client Veteran Status',
                'Client Gender']]
Y = final_nona['Time Spent (days)']

# Train linear regression model
model_LR = sm.OLS(Y, X).fit()
model_LR.summary()

# Creat variables to compare changes
# Create another dataframe

client_diff = client_final.drop(['EE Provider ID', 'Client Unique ID', 'Client Gender',
                                         'Client Primary Race', 'Client Ethnicity', 
                                         'Client Veteran Status','Time Spent (year)'], axis=1)

# Create variabeles recording the difference between exit and entry

client_diff['Income diff'] = client_diff['Monthly Amount (Exit)'] - client_diff['Monthly Amount (Entry)']
client_diff['Insurance diff'] = client_diff['Insurance (Exit)'] - client_diff['Insurance (Entry)']
client_diff['Noncash diff'] = client_diff['Noncash (Exit)'] - client_diff['Noncash (Entry)']
client_diff['Disability diff'] = client_diff['Disability (Exit)'] - client_diff['Disability (Entry)']


# Create a weighted score represents whether client's life was improved at exit
# Variables: Income diff, Insurance diff, Noncash diff, Disability diff
# Formula: 0.0004*Income diff + 0.4*Insurance diff + 0.4*Noncash diff - 0.2*Disability diff

client_diff['Score'] = 0.0004*client_diff['Income diff'] + 0.4*client_diff['Insurance diff'] + 0.4*client_diff['Noncash diff'] - 0.2*client_diff['Disability diff']

client_diff.to_csv('../data/clients_diff.tsv', sep = '\t')

# Analyze how many clients had improved
client_diff[client_diff['Score'] > 0].count()[1]

# Mean scores for all the clients
client_diff['Score'].mean()










