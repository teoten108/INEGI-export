import pandas as pd
import numpy as np
from plotnine import *
import matplotlib.pyplot as plt
import seaborn as sb
plt.style.use('ggplot')

ex_rows = pd.read_csv("./exportations_activity_rows.csv")
ex_cols = pd.read_csv("./exportations_activity_cols.csv")

## Spanish names in a vector
categorias = pd.Series(pd.read_csv("./exportations_activity_cols.csv")
                       .columns.values[2:27])

## Shorter equivalents in English
activities_en = pd.Series(["Total", "Food", "Drinks and tobacco",
                           "Textiles", "Textile products", "Tailoring",
                           "Paper", "Chemistry", "Plastic",
                           "Minerals based", "Metal industry",
                           "Metal products","Machinery", "Electronics",
                           "Transport equipment", "Furniture",
                           "Other manufactures","Not specified", "Mining",
                           "Leather", "Wood","Printing","Electricity",
                           "Petroleum", "Petroleum products"])

## Change column names 2:27
ex_cols.columns = pd.Series(['state','year']).append(activities_en)
list(ex_cols.columns.values)

def equivalent(expression_es):
    position = list(categorias[categorias == expression_es].index)
    ## Match expression_es in the list categorias
    expression_en = activities_en[position[0]]
    return(expression_en)

def translate (vector_es):
    vector_en = []
    for i in range(len(vector_es)):
        vector_en.append(equivalent(vector_es[i]))
        ## Need to define equivalent 
    return(vector_en)

ex_rows = ex_rows.assign(Activity = translate(ex_rows['Descripción']))

ex_rows.query('Activity == "Total"').groupby('state')
.agg({'USD':'sum'}).sort_values('USD', ascending=False)
