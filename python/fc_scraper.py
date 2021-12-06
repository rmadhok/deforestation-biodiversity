"""
=== Web Scraper for pulling Forest clearance data (2014-present) ===
The scraper cycles through every page on the start url
and builds a useable dataset. 
"""
#----------------------
# Settings
#----------------------

# META
__author__ = "Raahil Madhok"
__copyright__ = "Copyright 2021"
__version__ = "1.0"
__maintainer__ = "Raahil Madhok"
__email__ = "madhok.raahil@gmail.com"
__status__ = "Production"


# Import Libraries
import requests
from   bs4 import BeautifulSoup
import os
import pandas as pd
import urllib
import time
import re
from func_timeout import func_timeout, FunctionTimedOut
from scrape_functions import *

## Set Parameters
url = 'http://forestsclearance.nic.in/'         
#dir = '/Volumes/Backup Plus/research/data/def_biodiv/parivesh/'
dir = '/Users/rmadhok/Dropbox/def_biodiv/data/raw/'
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

r = s.get(url + 'Online_Status.aspx')

# Soupify
soup = BeautifulSoup(r.content, 'html.parser')

# Set post parameters
headers = {
    'Connection': 'keep-alive',
    'Cache-Control': 'no-cache',
    'Upgrade-Insecure-Requests': '1',
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
    'Accept-Encoding': 'gzip, deflate',
    'Accept-Language': 'en-GB,en-US;q=0.9,en;q=0.8',
}

data = {
    '__EVENTTARGET': '',
    '__EVENTARGUMENT': '',
    '__VIEWSTATEENCRYPTED': '',
	'ctl00$ContentPlaceHolder1$RadioButtonList1': 'New',
    'ctl00$ContentPlaceHolder1$ddlyear': '-All Years-',
    'ctl00$ContentPlaceHolder1$ddl1': 'Select',
    'ctl00$ContentPlaceHolder1$ddl3': 'Select',
    'ctl00$ContentPlaceHolder1$ddlcategory': '-Select All-',
    'ctl00$ContentPlaceHolder1$DropDownList1': '-Select All-',
    'ctl00$ContentPlaceHolder1$txtsearch': '',
    'ctl00$ContentPlaceHolder1$Button1': 'SEARCH',
    'ctl00$ContentPlaceHolder1$hdstatus': 'New',
    '__ASYNCPOST': 'true',
}

# Add dynamic args
data['__VIEWSTATE']          = soup.find('input', {'id': '__VIEWSTATE'         })['value']
data['__VIEWSTATEGENERATOR'] = soup.find('input', {'id': '__VIEWSTATEGENERATOR'})['value']
data['__EVENTVALIDATION']    = soup.find('input', {'id': '__EVENTVALIDATION'   })['value']

#Post to Page 1
r = s.post(url + 'Online_Status.aspx', data = data)

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

# Scrape
for row in rows[1:len(rows)-2]:

	print('Scraping Project ' + str(rows.index(row)) + ' From Page 1...')
	
	#Create data containers
	project_data = []
	item = {}

	#Scrape report
	item['page_no'] = page_no
	cols = row.findAll('td')
	report_url = cols[10].find('a')['href']
	if report_url.startswith('viewreport'):
		
		#Scrape Report Data
		report_page = s.get(url + report_url)
		item.update(scrapeReport(report_page.content))
		item.update(scrapeTables(report_page.content))

	#Scrape Timeline
	#timeline_url = cols[10].find('a')['href']
	#if timeline_url.startswith('Timeline'):
	#	timeline_page = requests.get(url + timeline_url)
	#	item.update(scrapeTimeline(timeline_page.content))

	project_data.append(item)
	master += project_data

#------------------------
# Scrape Remaining Pages
#------------------------

# update post parameters
del data['ctl00$ContentPlaceHolder1$Button1']
data['ctl00$ScriptManager1'] = 'ctl00$ContentPlaceHolder1$rr|ctl00$ContentPlaceHolder1$grdevents'
data['__EVENTTARGET']        = 'ctl00$ContentPlaceHolder1$grdevents'

# 964 pages total.
lastPage = 976
pageDelay = list(range(50,lastPage+1,50))
pageRange = list(range(2, lastPage+1))
broken = {120, 129, 499, 529, 583, 856, 892, 915, 925} 
pageRange = [e for e in pageRange if e not in broken]
for page in pageRange:
	
	try:
		
		if page % 5 == 0:

			#Export to CSV
			df = pd.DataFrame(master)
			df.to_excel('fc_raw3.xlsx', encoding = 'utf-8')

		if page in pageDelay:
			print("Sleeping for 2 mins...")
			time.sleep(120)

		#update post parameters
		data['__VIEWSTATE']          = re.search(r'__VIEWSTATE\|([^|]+)', r.text)[1]
		data['__VIEWSTATEGENERATOR'] = re.search(r'__VIEWSTATEGENERATOR\|([^|]+)', r.text)[1]
		data['__EVENTVALIDATION']    = re.search(r'__EVENTVALIDATION\|([^|]+)', r.text)[1]
		data['__EVENTARGUMENT']      = 'Page${}'.format(page)

		# Post to page
		r = func_timeout(600, s.post, args = (url+'Online_Status.aspx', data))
		
		#Soupify
		soup = BeautifulSoup(r.content, 'lxml')
		table = soup.find('table', {'id' : 'ctl00_ContentPlaceHolder1_grdevents'})
		page_no = int(table.find('tr', {'class': 'pagi'}).span.text)
		rows = table.findAll('tr')
		for row in rows[1:len(rows)-2]:

			try:
				
				print('Scraping Project ' + str(rows.index(row)) + ' From Page ' + str(page) + '...')
				
				#Create data containers
				project_data = []
				item = {}

				#Scrape report
				item['page_no'] = page_no
				cols = row.findAll('td')
				report_url = cols[10].find('a')['href']
				if report_url.startswith('viewreport'):
					
					# Clink report link
					report_page = s.get(url + report_url)
					#report_page = func_timeout(600, s.get, args=(url+report_url))
					
					#Scrape report data
					item.update(scrapeReport(report_page.content))
					item.update(scrapeTables(report_page.content))
				
				#Scrape Timeline
				#timeline_url = cols[10].find('a')['href']
				#if timeline_url.startswith('Timeline'):
				#	timeline_page = requests.get(url + timeline_url)
				#	item.update(scrapeTimeline(timeline_page.content))
				
				project_data.append(item)
				master += project_data

			#except FunctionTimedOut:
			#	print("Project Link was Hanging. Moving to next one...")
			#	time.sleep(200)
			#	continue

			except:
				print("Could not reach project link. Moving to next project...")
				time.sleep(120)
				continue

	except FunctionTimedOut:
		print("Page was hanging. Moving to next one...")
		time.sleep(120)
		continue

	except:
		print("Could not reach page. Trying next one...")
		time.sleep(120)
		continue

# Write to CSV
df = pd.DataFrame(master)
df.to_excel('fc_raw3.xlsx', encoding = 'utf-8')
