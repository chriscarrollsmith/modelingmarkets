#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Import libraries for scraping and machine learning
import requests
import pandas as pd
import matplotlib.pyplot as plt

# Scrape economic dataset structure dictionary from IMF RESTful API
# (API Instructions: http://www.bd-econ.com/imfapi1.html)
url = 'http://dataservices.imf.org/REST/SDMX_JSON.svc/'
key = 'Dataflow'  # Method with series information
search_term = 'Commodity Price'  # Term to find in series names
series_list = requests.get(f'{url}{key}').json()\
    ['Structure']['Dataflows']['Dataflow']
            
# Use dictionary keys to navigate through results and fetch the key for the 
# Primary Commodity Price System series:
for series in series_list:
    if search_term in series['Name']['#text']:
        series_key = f"{series['KeyFamilyRef']['KeyFamilyID']}"

#Use the Primary Commodity Price System key to get dimensions of that series
dimension_list = requests.get(f'{url}DataStructure/{series_key}').json()\
    ['Structure']['KeyFamilies']['KeyFamily']\
    ['Components']['Dimension'] # Returns a list of four tuples
for n, dimension in enumerate(dimension_list):
    print(f"{n} : {dimension['@codelist']}")

# Get codes for all dimensions in the series
key = [f"CodeList/{dimension_list[i]['@codelist']}" for i in range(len(dimension_list))]
code_list = [requests.get(f'{url}{k}').json()['Structure']['CodeLists']['CodeList']['Code'] for k in key[1:len(dimension_list)]]

#Save geographical area names and codes as a dictionary, keyed to the names
areas = {}
areas[f"{code_list[0]['Description']['#text']}"] = f"{code_list[0]['@value']}"

#Save series names and codes as a dictionary, keyed to the names
indexes = dict(zip([f"{code_list[1][i]['Description']['#text']}" for i in range(len(code_list[1]))],\
    [f"{code_list[1][i]['@value']}" for i in range(len(code_list[1]))]))
    
#Save value variable names and codes as a dictionary, keyed to the names
variables =  dict(zip([f"{code_list[2][i]['Description']['#text']}" for i in range(len(code_list[2]))],\
    [f"{code_list[2][i]['@value']}" for i in range(len(code_list[2]))]))

#Define url and key for API request
url = 'http://dataservices.imf.org/REST/SDMX_JSON.svc/'
key = 'CompactData/PCPS/'
# key = 'CompactData/PCPS/M.W00.PLITH.IX' # adjust codes here, using dot for frequency variable

# Download series JSON data from the API
data = (requests.get(f'{url}{key}').json()['CompactData']['DataSet']['Series'])

#Get lists of all observations in the dataset
data_list = []
for obs in data:
    for elem in obs['Obs']:
        data_list.append([elem['@TIME_PERIOD'],elem['@OBS_VALUE'],obs['@COMMODITY'],obs['@UNIT_MEASURE'],obs['@FREQ']])

# Create pandas dataframe from the observations
df = pd.DataFrame(data_list, columns=['date', 'value', 'commodity', 'unit', 'frequency'])

#Convert code dictionaries to dataframes for joining
commodities = pd.DataFrame.from_dict(data=indexes,orient='index',columns=['code'])
commodities.index.name = 'series'
commodities.reset_index(inplace=True)
measures = pd.DataFrame.from_dict(data=variables,orient='index',columns=['code'])
measures.index.name = 'measure'
measures.reset_index(inplace=True)

# Save cleaned dataframe as an Excel file with multiple worksheets
with pd.ExcelWriter("IMFcommodities.xlsx") as writer:
    df.to_excel(writer, sheet_name="Dataset", index=False)
    commodities.to_excel(writer, sheet_name="Commodities", index=False)
    measures.to_excel(writer, sheet_name="Measures", index=False)

# Basic plot of agricultural raw materials prices
ser = df[(df.frequency=='M') & (df.commodity=='PAGRI') & (df.unit=='IX')].copy()
ser.loc[:,'value'] = ser['value'].astype("float")
ser.loc[:,'date'] = pd.to_datetime(ser['date'])
plt.plot('date','value',data=ser)
plt.xlabel('Date')
plt.ylabel('Index value')
plt.suptitle("Global agricultural raw materials prices")
plt.show()
