# openelections-data-cleaning

Processing ugly elections data for [OpenElections](https://github.com/openelections)
* 2016 general election: Monroe county

## Method
1. Divide PDF into [separate files](pdfs/) by office
2. Export to [CSVs](data-export/) in Tabula
3. Process in [scripts/cleanMonroeData_2016.R](scripts/cleanMonroeData_2016.R)