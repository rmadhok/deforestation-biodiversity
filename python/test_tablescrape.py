import requests
from   bs4 import BeautifulSoup
import pandas as pd
import os

dir = '/Users/rmadhok/Documents/ubc/research/consecon/data/'
page = requests.get('http://forestsclearance.nic.in/admin/useragency_report_w.aspx?pid=FP/RJ/MIN/2644/2018')
soup = BeautifulSoup(page.content, 'html.parser')

table = soup.findAll('table', {'class': 'ez1'})[0]

keys = []
headers = table.findAll('th')
prefix = headers[0].text
for header in headers[1:]:
	keys.append(prefix + header.text)

rows = table.findAll('tr')
#Scrape table into dataframe
table_data = [[td.text for td in rows[i].findAll('td')] for i in range(len(rows))]

#Convert to dataframe
df = pd.DataFrame(table_data, columns=keys)
df = df.iloc[2:].reset_index(drop=True)

#reshape wide
df = df.unstack().to_frame().sort_index(level=1).T

df.columns = df.columns.map('{0[0]}_{0[1]}'.format)
dicti = df.to_dict('record')
print dicti

#export
#os.chdir(dir)
#df.to_csv('reshape_test.csv', encoding = 'utf-8')


