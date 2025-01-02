import pandas as pd
import numpy as np

def unprocessed (csv_file):
    df = pd.read_csv(csv_file)
    return df
def load_and_process (csv_file1,csv_file2):
    df1 = pd.read_csv(csv_file1)
    df2 = pd.read_csv(csv_file2)
    dfmerge = df1.merge(df2)
    dfreplace = (
        dfmerge.replace('Backstreet Boys, The','Backstreet Boys')
        .replace('2 Pac','2Pac')
        .replace('C and C Music Factory','C+C Music Factory')
        .replace('Janet','Janet Jackson')
        .replace('Proclaimers, The','The Proclaimers')
        .replace('Notorious B.I.G., The','The Notorious B.I.G')
        .replace('Spice Girls, The','The Spice Girls')
        .replace("B-52's, The","The B-52's")
        .replace('Heights, The','The Heights')
        #replacing artist names into more appropriate formats (ex. "The Proclaimers" instead of "Proclaimers, The")
        #replacing artist names so that multiple/duplicate artist names are consistent (ex. 2 Pac is the same as 2Pac)
        .drop(columns=['years_pre_birth_1','years_pre_birth_2','years_pre_birth_3',
               'years_pre_birth_4','years_pre_birth_5','years_pre_birth_6',
               'years_pre_birth_7','years_pre_birth_8','years_pre_birth_9',
               'years_pre_birth_10',])
               #dropping 'years_pre_birth_x' columns due to a lot of missing/insufficient values
        .drop([42,75,228,238,320]) #dropping duets to focus on solo singles (easier to analyze the artist category)
        .rename(columns={'years_old_13':'13 Years Old',
                          'years_old_12':'12 Years Old',
                          'years_old_11':'11 Years Old',
                          'years_old_10':'10 Years Old',
                          'years_old_9':'9 Years Old',
                          'years_old_8':'8 Years Old',
                          'years_old_7':'7 Years Old',
                          'years_old_6':'6 Years Old',
                          'years_old_5':'5 Years Old',
                          'years_old_4':'4 Years Old',
                          'years_old_3':'3 Years Old',
                          'years_old_2':'2 Years Old',
                          'years_old_1':'1 Years Old',
                          'years_old_0':'Year Born',
                          'mean_millennial_recognition':'Recognition by Millennials',
                          'mean_gen_z_recognition':'Recognition by Gen-Zs'})
                          #renaming columns into more appropriate names
    )
    return dfreplace
def by_artist (df):#grouping songs by artists, and use the grouped data by evaluating the highest data points of mean recognizability (max), indicating most popular songs of the artist
    dfgroup = df.groupby('artist').max().reset_index()
    dfgroupf = dfgroup.drop(columns=['song'])
    return dfgroupf
def sort_recog (df):#sort the recognizability by the mean of Millennial and Gen-Z recognizability
    df['mean_all'] = (1/2)*(df['Recognition by Millennials']+df['Recognition by Gen-Zs'])
    sortdf = df.sort_values(by=['mean_all'],ascending=False)
    finaldf = sortdf.drop(columns=['mean_all'])
    fdf = finaldf[finaldf['No. of Songs'] >= 3]#Filter out artists with less than 2 songs
    return fdf