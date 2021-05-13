def colops(df):
    """This columnOps.py module contains columnOps function to display the count of unique values, null values 
        and the operation (drop/impute) to perform on the column. It takes only one argument which is your 
        dataset to be analysed"""
    import pandas as pd
    a=pd.Series(df.nunique(),name='Unique_Count')
    b=pd.Series(df.isnull().sum(),name='Null_Count')
    info_df=[a,b]
    data_details=pd.concat(info_df,axis=1)
    data_details['%age_of_Data']=(data_details['Null_Count']/len(df))*100
    
    def what2do(num):
        if num>=30:
             return ('Drop')
        else:
             return ('Impute')
    
    data_details['Operation']=data_details['%age_of_Data'].apply(what2do)
    return data_details

def impute(df,con,cat):
    for each_con_col in con:
        df[each_con_col].fillna(df[each_con_col].median(),inplace=True)
    for each_cat_col in cat:
        df[each_cat_col].fillna(df[each_cat_col].mode()[0],inplace=True)