# This application (CMISST) requires several datasets
#  This code creates those datasets and saves them as RData files
#  This only has to be run to update the data, not each time you want to use the code

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

#********************************************************
# THIS IS TO CREATE THE GLOBAL SST DATA (RDATA FILE)
#********************************************************
dataSet='ERSST'
# Full globe
min.lon=0
max.lon=360
min.lat=-90
max.lat=90
years=seq(1967, 2023, 1)
months=seq(1,12,1)
removeBering=FALSE
returnDataType='anom'
returnObjectType='array'

# Function defined in create_OceanData_Object.R
oceanData_ERSST<-getOceanData(dataSet=dataSet,
                        returnDataType=returnDataType, returnObjectType=returnObjectType,
                        min.lon=min.lon, max.lon=max.lon,
                        min.lat=min.lat, max.lat=max.lat,
                        years = years, months = months,
                        removeBering = removeBering)
save(x = "oceanData_ERSST", file = 'data/oceanSSTData.RData')
load('data/oceanSSTData.RData')

