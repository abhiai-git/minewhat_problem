#*********** Importing Libraries
import requests,csv
from bs4 import BeautifulSoup

#********** Preparing Output File output 
from collections import defaultdict

output = csv.writer(open("ustre.csv", 'wb'))
columns = defaultdict(list)

#***********Function to open the url and fetch data def 
def getdata(url):
    res=requests.get(url)
    soup=BeautifulSoup(res.text.encode('utf-8'))
    table_content=soup.find_all('td',{'class':'setTableContentCellWidth'})
    for rows in table_content:
        breaker=rows.find_all('tr')
        for tag in breaker:
            value=str(tag)
            date=value.split(">")[2].split("<")[0].replace("/","-")
            val=value.split(">")[20].split("<")[0]
            output.writerow([date,val])
            
#*********** Setting the Limit to generate urls
for year in range(2005,2016):
    url='http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year='+str(year)
    getdata(url)
    print url



