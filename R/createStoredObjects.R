# This application (CMISST) requires several datasets
#  This code creates those datasets and saves them as RData files
#  This only has to be run to update the data, not each time you want to use the code



# LAND --------------------------------------------------------------------


#********************************************************
# get the land for plotting (wrap across antimeridian)
#********************************************************
land<-rnaturalearth::ne_countries(type='countries', scale = "large", returnclass = "sf")
amer <- land[land$region_un=='Americas',]
# shift the Americas to a more Pacific centric worldview
pacified_amer <- sf::st_shift_longitude(amer)
rest_of_world <- land[!land$region_un=='Americas',]
land <- rbind(pacified_amer, rest_of_world)
save("land", file = "data/land.Rdata")



# ERSST -------------------------------------------------------------------


#********************************************************
# THIS IS TO CREATE THE GLOBAL SST DATA (RDATA FILE)
#********************************************************
source("R/getOceanData.R")

dataSet='ERSST'
# Full globe
min.lon=0
max.lon=360
min.lat=-90
max.lat=90
years=seq(1967, 2023, 1)
months=seq(1,12,1)

# Function defined in create_OceanData_Object.R
oceanData_ERSST<-getOceanData(dataSet=dataSet,
                        min.lon=min.lon, max.lon=max.lon,
                        min.lat=min.lat, max.lat=max.lat,
                        years = years, months = months)
save(x = "oceanData_ERSST", file = 'data/oceanSSTData.RData')
load('data/oceanSSTData.RData')


# SSH ---------------------------------------------------------------------

#********************************************************
# THIS IS TO CREATE THE GLOBAL SSH DATA (RDATA FILE)
#********************************************************
source("R/getOceanData.R")

# The compiled SSH data (referred to as oceanSSHData.RData)
#  is too large to store in github.  Please download the original data
#  files here: https://psl.noaa.gov/data/gridded/data.godas.html
#  and create the file with the code below.

dataSet='SSH'
# Full globe
# SSH longitude goes from 0.5 to 359.5, by 1
min.lon=0
max.lon=360
# SSH latitude goes from -74 to 65, by 1/3?
min.lat=-90
max.lat=90
years=seq(1980, 2023, 1)
months=seq(1,12,1)

oceanData_SSH<-getOceanData(dataSet=dataSet,
                            min.lon=min.lon, max.lon=max.lon,
                            min.lat=min.lat, max.lat=max.lat,
                            years = years, months = months)
save(x = "oceanData_SSH", file = 'data/oceanSSHData.RData')
load('data/oceanSSHData.RData')



# AdultCounts -------------------------------------------------------------


# Dam counts from Columbia Basin Research (CBR)
#  Data Access in Real Time (DART)
sp_Chinook <- read.csv("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=csv&proj=BON&startdate=1%2F1&enddate=12%2F31&run=1")[,c("Year","Chinook")]
colnames(sp_Chinook)[2] <- "Sp_Chinook"
fa_Chinook <- read.csv("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=csv&proj=BON&startdate=1%2F1&enddate=12%2F31&run=3")[,c("Year","Chinook")]
colnames(fa_Chinook)[2] <- "Fa_Chinook"
steelhead <- read.csv("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=csv&proj=BON&startdate=1%2F1&enddate=12%2F31&run=")[,c("Year","Steelhead")]

# Create the data frame
response <- merge(merge(sp_Chinook, fa_Chinook), steelhead)
response <- response[response$Year <= 2024 & response$Year >= 1950 & !is.na(response$Year),]
names(response)[names(response) == 'Year'] <- 'year'

save(x = "response", file = 'data/responseData.RData')
load('data/oceanSSHData.RData')


