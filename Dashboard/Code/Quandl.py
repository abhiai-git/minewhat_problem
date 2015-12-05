#*********** Importing Libraries
import Quandl
import csv,random
from collections import defaultdict

#********** Preparing Output File
output = csv.writer(open("quandl.csv", 'wb'))
columns = defaultdict(list)
output.writerow(["DATE","VALUE"])
#***********Using Qunadle Library To fetch Data data =
data = Quandl.get("FRED/USD3MTD156N")
print data.head()
for i in range(0,len(data['VALUE'])):
    year= int(str(data.index[i]).split("-")[0])
    if year>=2005:
        output.writerow([str(data.index[i]).split(" ")[0],data['VALUE'][i]])





    
