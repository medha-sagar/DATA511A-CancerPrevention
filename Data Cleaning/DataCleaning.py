# -*- coding: utf-8 -*-
#@author : Sagar Medha
import pandas as pd

#Load the 
df = pd.read_csv('~/Downloads/SA10+_ABC_Proteomics_AfterNorm_M_noCVFilter.csv')

#Drop Abnova and Aviva Field as it is not necessary
df = df.drop(['ab.name'], axis = 1)

# Transpose the data to get protiens as columns and patients as rows
transpose_df = df.transpose()

# Make the first row as column names
new_header = transpose_df.iloc[0] #grab the first row for the header
transpose_df = transpose_df[1:] #take the data less the header row
transpose_df.columns = new_header

# Crete three columns [Patient Id, TX and Metabol]
# We require this to merge the data later to the metadata 
patient_id = [0,0]
tx = [0,0]
metabol = [0,0]

for i in range(2,len(transpose_df.index)):
    metabol_val = str(transpose_df.index[i].split('_')[1]).replace('p','')
    tx_val = int(transpose_df.index[i].split('_')[0]) % 2
    if tx_val == 1:
        tx.append(0)
        metabol.append(-1*int(metabol_val))
    elif tx_val == 0:
        tx.append(1)
        metabol.append(int(metabol_val))
    patient_id.append(int(transpose_df.index[i].split('_')[2]))
   
transpose_df['PPTID'] = patient_id
transpose_df['TX'] = tx
transpose_df['Metabol'] = metabol

#save the transposed data
transpose_df.to_csv('transposed_protoemics.csv')

# Read the Metadata
df_meta = pd.read_excel('~/Downloads/Aspirin vs Placebo metadata.xlsx')

#Merge the proteomics and metadata files on ppt id and tx 
merged_df = pd.merge(transpose_df, df_meta, how ='inner', on=['PPTID','TX'])

#Save the merged file
merged_df.to_csv('protoemics_with_meta_data.csv')



## HANDLE NULL VALUES
df = merged_df
total = df.isnull().sum().sort_values(ascending=False)
percent = (df.isnull().sum()/ df.isnull().count()).sort_values(ascending=False)
missing_df = pd.concat([total, percent], axis = 1, keys=['Total Missing', 'Percent Missing'])
print(missing_df.head(300))

#Remove columns with more than 20% missing data
#column wise data cleaning
proteins_drop = list 
for i in range(1, 401):
    merged_df = merged_df.drop(missing_df.index[i], axis =1)
    
merged_df.to_csv('cleaned_master_data.csv')

#Drop columns with NANs
merged_df = merged_df.dropna(axis = 1)

#Save the cleaned file
merged_df.to_csv('imputed_master_data.csv')