import pandas as pd
import pymysql
from sqlalchemy import create_engine as ce

# please replace the ****** with your password, plus, you might also need replace the database name, in my case it's MH6142
sqlengine = ce('mysql+pymysql://root:******@localhost:3306/MH6142')

# in my MH6142 database, there is already a test_abc table, below is just to retrieve it back as dataframe
sql = '''
    select * from test_abc;
    '''
dataframe_test_abc = pd.read_sql_query(sql, sqlengine)
print(dataframe_test_abc)


# write some dataframe to database table test_def, replace it if it's already there

dataframe_test_def = pd.DataFrame({'id':[1,2,3,4], 'att':['d', 'e', 'f', 'g']})
dataframe_test_def.to_sql('test_def', sqlengine, index=False, if_exists='replace')

# read the new test_def we just inserted back to another dataframe test_def1
sql1 = '''
    select * from test_def
    '''
dataframe_test_def1 = pd.read_sql_query(sql1, sqlengine)
print(dataframe_test_def1)

# below is to list all the tables in database
sql2 = '''
    show tables
    '''

dataframe_tables = pd.read_sql_query(sql2, sqlengine)
print(dataframe_tables)

# read global_superstore.xlsx into db
dataframe_superstore = pd.read_excel("global_superstore.xlsx", "Orders",)

print(dataframe_superstore)

dataframe_superstore.to_sql('global_superstore', sqlengine, index=False, if_exists='fail')

# check from mysql side to see whether table's created

