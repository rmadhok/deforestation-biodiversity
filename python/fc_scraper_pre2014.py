#----------------------
# Settings
#----------------------

# Libraries
import requests
from   bs4 import BeautifulSoup
import os
import pandas as pd
import time
import re
from func_timeout import func_timeout, FunctionTimedOut


# Parameters
url = 'http://forestsclearance.nic.in/'         
dir = '/Volumes/Backup Plus/research/data/def_biodiv/parivesh/'
os.chdir(dir)

#-----------------------
# Landing Page
#-----------------------

# Session object(persist parameters across request)
s = requests.Session()

#Open Search Page
s.headers = {
    'Connection': 'keep-alive',
    'Cache-Control': 'no-cache',
    'Upgrade-Insecure-Requests': '1',
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
    'Accept-Encoding': 'gzip, deflate',
    'Accept-Language': 'en-GB,en-US;q=0.9,en;q=0.8',
}

r = s.get(url + 'search.aspx')

# Soupify
soup = BeautifulSoup(r.content, 'html.parser')

# Set post parameters
data = {
    'ctl00$ScriptManager1': 'ctl00$ContentPlaceHolder1$UpdatePanel1|ctl00$ContentPlaceHolder1$Button1',
    '__EVENTARGUMENT': '',
    '__EVENTTARGET': '',
    '__VIEWSTATEENCRYPTED': '',
    'ctl00$ContentPlaceHolder1$ddlyear': '-All Years-',
    'ctl00$ContentPlaceHolder1$ddl3': 'Select',
    'ctl00$ContentPlaceHolder1$ddlcategory': '-Select All-',
    'ctl00$ContentPlaceHolder1$DropDownList1': 'Approved',
    'ctl00$ContentPlaceHolder1$txtsearch': '',
    '__ASYNCPOST': 'true',
    'ctl00$ContentPlaceHolder1$Button1': 'SEARCH',
}

# Add dynamic args
data['__VIEWSTATE']          = soup.find('input', {'id': '__VIEWSTATE'         })['value']
data['__VIEWSTATEGENERATOR'] = soup.find('input', {'id': '__VIEWSTATEGENERATOR'})['value']
data['__EVENTVALIDATION']    = soup.find('input', {'id': '__EVENTVALIDATION'   })['value']

#Post to Page 1
r = s.post(
    url + 'search.aspx',
    data = data
)

#--------------------
# Scrape Page 1
#--------------------

#Initiate master data list
master = []

#Soupify
soup = BeautifulSoup(r.content, 'lxml')
table = soup.find('table', {'id' : 'ctl00_ContentPlaceHolder1_grdevents'})
page_no = int(table.find('tr', {'class': 'pagi'}).span.text)
rows = table.findAll('tr')

print('Scraping projects from page 1...')
for row in rows[1:len(rows)-2]:

    #print('Scraping Project ' + str(rows.index(row)) + ' From Page 1...')

    #Create data containers
    project_data = []
    my_dict = {}

    cols = row.findAll('td')

    my_dict['page_no'] = page_no
    my_dict['state'] = cols[1].text
    my_dict['prop_no'] = cols[2].text
    my_dict['prop_name'] = cols[4].text
    my_dict['proj_cat'] = cols[5].text
    my_dict['proj_area_forest'] = cols[7].text
    my_dict['prop_status'] = cols[8].text
    my_dict['date_submit'] = cols[9].text
    my_dict['date_rec'] = cols[10].text
    
    project_data.append(my_dict)
    master += project_data

#------------------------
# Scrape Remaining Pages
#------------------------

# update post parameters
del data['ctl00$ContentPlaceHolder1$Button1']
data['ctl00$ScriptManager1'] = 'ctl00$ContentPlaceHolder1$rr|ctl00$ContentPlaceHolder1$grdevents'
data['__EVENTTARGET']        = 'ctl00$ContentPlaceHolder1$grdevents'

# 782 pages total
pageDelay = list(range(50,782,50))
lastPage = 782
for page in range(2, lastPage+1):

    print('Scraping projects from page ' + str(page) + '...')

    try:
        
        if page % 5 == 0:

            #Export to CSV
            df = pd.DataFrame(master)
            df.to_excel('fc_pre2014_raw.xlsx', encoding = 'utf-8')

        if page in pageDelay:
            print("Sleeping for 5 mins...")
            time.sleep(300)

        # Update post parameters
        data['__VIEWSTATE']          = re.search(r'__VIEWSTATE\|([^|]+)', r.text)[1]
        data['__VIEWSTATEGENERATOR'] = re.search(r'__VIEWSTATEGENERATOR\|([^|]+)', r.text)[1]
        data['__EVENTVALIDATION']    = re.search(r'__EVENTVALIDATION\|([^|]+)', r.text)[1]
        data['__EVENTARGUMENT']      = 'Page${}'.format(page)
        
        # Post to page
        r = func_timeout(600,
            s.post,
            args = (
                url + 'search.aspx', 
                data
            )
        )

        #Soupify
        soup = BeautifulSoup(r.content, 'lxml')
        table = soup.find('table', {'id' : 'ctl00_ContentPlaceHolder1_grdevents'})
        page_no = int(table.find('tr', {'class': 'pagi'}).span.text)
        rows = table.findAll('tr')

        for row in rows[1:len(rows)-2]:

            #Create data containers
            project_data = []
            my_dict = {}

            cols = row.findAll('td')

            my_dict['page_no'] = page_no
            my_dict['state'] = cols[1].text
            my_dict['prop_no'] = cols[2].text
            my_dict['prop_name'] = cols[4].text
            my_dict['proj_cat'] = cols[5].text
            my_dict['proj_area_forest'] = cols[7].text
            my_dict['prop_status'] = cols[8].text
            my_dict['date_submit'] = cols[9].text
            my_dict['date_rec'] = cols[10].text

            project_data.append(my_dict)
            master += project_data

    except FunctionTimedOut:
        print("Page was hanging. Moving to next one...")
        time.sleep(200)
        continue

    except:
        print("Could not reach page. Trying next one...")
        time.sleep(300)
        continue

# Write to Excel
df = pd.DataFrame(master)
df.to_excel('fc_pre2014_raw.xlsx', encoding = 'utf-8')

