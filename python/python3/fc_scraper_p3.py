"""
=== Web Scraper for pulling Forest clearance data (2014-present) ===
The scraper cycles through every page on the start url
and builds a useable dataset. 
"""
# META
__author__ = "Raahil Madhok"
__copyright__ = "Copyright 2018"
__version__ = "1.0"
__maintainer__ = "Raahil Madhok"
__email__ = "madhok.raahil@gmail.com"
__status__ = "Production"


# Import Libraries
import requests
from   bs4 import BeautifulSoup
import os
import pandas as pd
import urllib.request, urllib.parse, urllib.error
import mechanize
import time
from scrape_functions import *

## Set Parameters
# Set Directory for Writing Data
dir = '/Users/rmadhok/Documents/ubc/research/def_biodiv/data/csv'
# Set top URL to follow and scrape
url = 'http://forestsclearance.nic.in/'            

#Open Search Page
r = requests.get(url + 'Online_Status.aspx')

#Set Post Parameters
VIEWSTATE, GENERATOR, VALIDATION = getFormData(r.content)
cookies = {
    'ASP.NET_SessionId': 'kaqs1jzegnfn4zxpwio4jthl',
    'countrytabs': '0',
    'countrytabs1': '0',
    'acopendivids': 'Omfc,Email,Campa,support,livestat,commitee,Links',
    'acgroupswithpersist': 'nada',
}
headers = {
    'Connection': 'keep-alive',
    'Cache-Control': 'max-age=0',
    'Upgrade-Insecure-Requests': '1',
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
    'Accept-Encoding': 'gzip, deflate',
    'Accept-Language': 'en-GB,en-US;q=0.9,en;q=0.8',
}

#Post to Page 1
r = requests.post(
    url + 'Online_Status.aspx',
    headers=headers,
    cookies=cookies,
    data = {
    	'ctl00$ScriptManager1': 'ctl00$ContentPlaceHolder1$UpdatePanel1|ctl00$ContentPlaceHolder1$Button1',
        '__EVENTARGUMENT': '',
        '__EVENTTARGET': '',
        '__VIEWSTATE': VIEWSTATE,
        '__VIEWSTATEGENERATOR': GENERATOR,
        '__VIEWSTATEENCRYPTED': '',
        '__EVENTVALIDATION': VALIDATION,
        'ctl00$ContentPlaceHolder1$ddlyear': '-All Years-',
        'ctl00$ContentPlaceHolder1$ddl1': 'Select',
        'ctl00$ContentPlaceHolder1$ddl3': 'Select',
        'ctl00$ContentPlaceHolder1$ddlcategory': '-Select All-',
        'ctl00$ContentPlaceHolder1$DropDownList1': '-Select All-',
        'ctl00$ContentPlaceHolder1$txtsearch': '',
        'ctl00$ContentPlaceHolder1$HiddenField1': '',
        'ctl00$ContentPlaceHolder1$HiddenField2': '',
        '__ASYNCPOST': 'false',
        'ctl00$ContentPlaceHolder1$Button1': 'SEARCH',
    }
)

#Scrape Page 1
###################################################

#Initiate master data list
data = []

#Soupify
soup = BeautifulSoup(r.content, 'lxml')
table = soup.find('table', {'id' : 'ctl00_ContentPlaceHolder1_grdevents'})
page_no = int(table.find('tr', {'class': 'pagi'}).span.text)
rows = table.findAll('tr')
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
		report_page = requests.get(url + report_url)
		item.update(scrapeReport(report_page.content))

		#Scrape report tables
		item.update(scrapeTables(report_page.content))

	#Scrape Timeline
	#timeline_url = cols[10].find('a')['href']
	#if timeline_url.startswith('Timeline'):
	#	timeline_page = requests.get(url + timeline_url)
	#	item.update(scrapeTimeline(timeline_page.content))

	project_data.append(item)
	data += project_data

#Get form data for next page post request
VIEWSTATE, GENERATOR, VALIDATION = getFormData(r.content)


#Scrape remaining pages
pageDelay = [50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600]
lastPage = 641
for page in range(2, lastPage + 1):

	
	try:
		if page % 10 == 0:

			#Export to CSV
			os.chdir(dir)
			df = pd.DataFrame(data)
			df.to_csv('fc_raw2.csv', encoding = 'utf-8')

		if page in pageDelay:
			print("Sleeping for 5 mins...")
			time.sleep(300)

		r = requests.post(
		url + 'Online_Status.aspx',
		cookies=cookies,
		data = {
			'ctl00$ScriptManager1': 'ctl00$ContentPlaceHolder1$UpdatePanel1|ctl00$ContentPlaceHolder1$Button1',
			'ctl00$ContentPlaceHolder1$RadioButtonList1': 'New',
		    '__EVENTARGUMENT': 'Page${}'.format(page),
		    '__EVENTTARGET': 'ctl00$ContentPlaceHolder1$grdevents',
		    '__VIEWSTATE': VIEWSTATE,
		    '__VIEWSTATEGENERATOR': GENERATOR,
		    '__VIEWSTATEENCRYPTED': '',
		    '__EVENTVALIDATION': VALIDATION,
		    'ctl00$ContentPlaceHolder1$ddlyear': '-All Years-',
		    'ctl00$ContentPlaceHolder1$ddl1': 'Select',
		    'ctl00$ContentPlaceHolder1$ddl3': 'Select',
		    'ctl00$ContentPlaceHolder1$ddlcategory': '-Select All-',
		    'ctl00$ContentPlaceHolder1$DropDownList1': '-Select All-',
		    'ctl00$ContentPlaceHolder1$txtsearch': '',
		    'ctl00$ContentPlaceHolder1$HiddenField1': '',
		    'ctl00$ContentPlaceHolder1$HiddenField2': '',
		    '__ASYNCPOST': 'false',
			}
		)

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
					#Scrape Report Data
					report_page = requests.get(url + report_url)
					item.update(scrapeReport(report_page.content))
					
					#Scrape report tables
					item.update(scrapeTables(report_page.content))
				
				#Scrape Timeline
				#timeline_url = cols[10].find('a')['href']
				#if timeline_url.startswith('Timeline'):
				#	timeline_page = requests.get(url + timeline_url)
				#	item.update(scrapeTimeline(timeline_page.content))
				
				project_data.append(item)
				data += project_data

			except:
				print("Could not reach project link. Moving to next project...")
				time.sleep(300)
				continue

		#Get form data for next page post request
		VIEWSTATE, GENERATOR, VALIDATION = getFormData(r.content)

	except:
		print("Could not reach page. Trying next one...")
		time.sleep(300)
		continue


# Write to CSV
os.chdir(dir)
df = pd.DataFrame(data)
df.to_csv('fc_raw2.csv', encoding = 'utf-8')
