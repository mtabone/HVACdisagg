require(RPostgreSQL)
setwd('~/Dropbox/repos/HVACDisagg')

## Load username and password
keyfile <- file("pecankeys.keys","r")
un <- readLines(keyfile,n=1)
pw <- readLines(keyfile,n=2)
close(keyfile)

## Load driver
drv <- dbDriver("PostgreSQL")

## Disconnect anythin on this driver just in case
for (c in dbListConnections(drv)){
  dbDisconnect(c)
}

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "postgres",
                 host = "dataport.pecanstreet.org", port = 5434,
                 user = un, password = pw)

## remove username and password
rm(un,pw)

## Look at tables in PecanStreets dataport for academics.
tables = dbGetQuery(con, "select * from information_schema.tables where table_schema = \'university\' order by table_name")

## Find all homes with complete data for 2015 and 2016
query="SELECT dataid, count(*) FROM university.electricity_egauge_hours
WHERE localhour
BETWEEN '01-01-2015' AND '01-01-2017' GROUP BY dataid "
datacounts = dbGetQuery(con,query)

## Call metadata
meta = dbGetQuery(con, "select dataid, building_type, city, state from university.metadata")
meta = merge(datacounts, meta, type = 'leftinner')

## Choose only complete data
homesuse = meta[(meta$count == max(meta$count)) & (meta$city == 'Austin') , ]

## Pull Weather Data
weather = dbGetQuery(con, "SELECT localhour, temperature from university.weather WHERE latitude = 30.292432")

## Pull one home's Load data
for (dataid in homesuse$dataid[1:3]){
  querytmp = "SELECT dataid, localhour, use, gen, air1, air2, air3, airwindowunit1, furnace1, furnace2 FROM university.electricity_egauge_hours WHERE localhour BETWEEN '01-01-2015' AND '01-01-2017' AND dataid = %s;"
  query = sprintf(querytmp, dataid)
  loaddata = dbGetQuery(con,query)
  
  
  data = merge(loaddata, weather, type = 'leftinner')
  
  ## Aggregate cooling and heating data
  data$cooling = rowSums(data[,c('air1','air2','air3','airwindowunit1')], na.rm = TRUE)
  data$heating = rowSums(data[,c('furnace1','furnace2')], na.rm = TRUE)
  data = data[,c('localhour','dataid','use','gen','temperature','heating','cooling')]
  
  ## Write a csv
  write.csv(x = data, file = sprintf('pecandata_%s.csv',  dataid))
  }