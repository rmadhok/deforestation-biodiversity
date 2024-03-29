import requests
from   bs4 import BeautifulSoup
import pandas as pd

def scrapeReport(page):
	"""
    Scrapes html data of project applications.

    Input: html page
    Output: dictionary of project variables and values
    """

    #Soupify html
	soup = BeautifulSoup(page, 'lxml')

	#Scrape report data
	item_report = {}
	
	# Get all Page Data
	spans = soup.findAll('span', {'class': ['li2', 'lisub2']})
	for span in spans:
		item_report[span.find('b').text] = span.find('b').next_sibling

	# Get data from non-table tables
	div_tables = soup.findAll('div', {'style': 'float:left;width:80%;margin-bottom:20px'})
	for div in div_tables:
		head = div.find('div', {'style': 'float:left;width:100%;padding:1% 0;border-bottom:solid 1px #000'})
		header = head.text.split(':')[0]
		item_report[header] = head.text.split(':')[1]

		var = div.findAll('span', {'class': ['li2', 'lisub2']})
		for v in var:
			key = header + '_' + v.find('b').text
			value = v.find('b').next_sibling
			item_report[key] = value 

	return item_report

def scrapeTables(page):
	"""
    Scrapes html table data from project applications.

    Input: html page
    Output: dictionary with rows labels as keys
    """

    #Select tables
	soup = BeautifulSoup(page, 'lxml')
	tables = soup.findAll('table', {'class': 'ez1'})

	#Scrape table data
	item_table = {}

	# Non-blank panges
	if len(tables) > 0:

		# Scrape only district and village tables
		for table in tables[2:4]:
			
			rows = table.findAll('tr')
			
			if len(rows) > 2:

				#Collect column names in list
				keys = []
				headers = table.findAll('th')
				prefix = headers[0].text
				for header in headers[1:5]:
					keys.append(prefix + header.text)

				#Scrape cell values into list
				table_data = [[td.text for td in rows[i].findAll('td')] for i in range(len(rows))][2:]
				table_data = [row for row in table_data if len(row) == 4] #Only table rows with 4 columns - eliminates bottom row (total)

				# Make data frame of table
				df = pd.DataFrame(table_data, columns=keys)

				# Only non-empty tables
				if len(df.index) > 0 :

					#reshape wide
					df = df.unstack().to_frame().sort_index(level=1).T
					df.columns = df.columns.map('{0[0]}_{0[1]}'.format)

					#convert to temp dictionary and populate main
					temp = df.to_dict('records')[0]
					item_table.update(temp)

	else:
		item_table = {'No Report': 'True'}

	return item_table

def scrapeTimeline(page):
	"""
    Scrapes html table data from project timeline.

    Input: html page
    Output: dictionary with rows labels as keys
    """

    #select table
	soup = BeautifulSoup(page, 'lxml')
	table_div = soup.find('div', {'id': 'detailHTML'})
	table = table_div.find('table')

	#collect columns and cells into separate lists
	keys = [header.text for header in table.findAll('th')]
	row = table.findAll('tr')[1]
	cols = row.findAll('td')
	values = [col.text for col in cols]

	#convert to dictionary
	item_timeline = dict(zip(keys, values))
	return item_timeline

def getFormData(page):
    """
    Finds form data required to post request for 
    next page. Returns tuple with data
    """
    soup = BeautifulSoup(page, 'html.parser')
    viewstate  = soup.find('input', {'id': '__VIEWSTATE'         })['value']
    generator  = soup.find('input', {'id': '__VIEWSTATEGENERATOR'})['value']
    validation = soup.find('input', {'id': '__EVENTVALIDATION'   })['value']
    return (viewstate, generator, validation)




