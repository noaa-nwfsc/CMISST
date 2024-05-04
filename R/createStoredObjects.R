# This application requires several datasets
#  This code creates those datasets and saves them as RData files
#  that can be loaded by the app
#  But it is not part of the app

library(reshape2)
library(sf)
library(ncdf4)

source('scripts/get_index.R') # This is not the same script as the app version
source('scripts/dam_counts/run_fetch_FPC_counts_single_species.R')

# get the land for plotting (wrap across antimeridian)
land<-rnaturalearth::ne_countries(type='countries', scale = "large", returnclass = "sf")
amer <- land[land$region_un=='Americas',]
# shift the Americas to a more Pacific centric worldview
pacified_amer <- st_shift_longitude(amer)
rest_of_world <- land[!land$region_un=='Americas',]
land <- rbind(pacified_amer, rest_of_world)
save("land", file = "CMISSTapp/data/land.Rdata")

# THIS WAS TO CREATE THE GLOBAL SST DATA (RDATA FILE) FOR SHINY
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

oceanData_ERSST<-getOceanData(dataSet=dataSet,
                        returnDataType=returnDataType, returnObjectType=returnObjectType,
                        min.lon=min.lon, max.lon=max.lon,
                        min.lat=min.lat, max.lat=max.lat,
                        years = years, months = months,
                        removeBering = removeBering)
save(x = "oceanData_ERSST", file = 'CMISSTapp/data/oceanSSTData.RData')
load('CMISSTapp/data/oceanSSTData.RData')

# SSH
dataSet='SSH'
# Full globe
# SSH longitude goes from 0.5 to 359.5, by 1
min.lon=0
max.lon=360
# SSH latitude goes from -74 to 65, by 1/3?
min.lat=-90
max.lat=90
years=seq(1980, 2022, 1)
months=seq(1,12,1)
removeBering=FALSE
returnDataType='anom'
returnObjectType='array'

oceanData_SSH<-getOceanData(dataSet=dataSet,
                        returnDataType=returnDataType, returnObjectType=returnObjectType,
                        min.lon=min.lon, max.lon=max.lon,
                        min.lat=min.lat, max.lat=max.lat,
                        years = years, months = months,
                        removeBering = removeBering)
save(x = "oceanData_SSH", file = 'CMISSTapp/data/oceanSSHData.RData')
load('CMISSTapp/data/oceanSSHData.RData')


#  Code to extract dam counts and save to a file
# The data this retreives has errors in 2023 - I'm moving to DART as the source
# spCK <- fetch_FPC_counts_single_species(my_species="spCK", my_age="adult", Year_start = 1970, Year_end = 2023)
# spCK$val <- spCK$BON[[1]]
# spCK <- spCK[,c("Year","val")]
# colnames(spCK) <- c("year","spCK")
# faCK <- fetch_FPC_counts_single_species(my_species="faCK", my_age="adult", Year_start = 1970, Year_end = 2023)
# faCK$val <- faCK$BON[[1]]
# faCK <- faCK[,c("Year","val")]
# colnames(faCK) <- c("year","faCK")
# steel <- fetch_FPC_counts_single_species(my_species="steel", my_age="adult", Year_start = 1970, Year_end = 2023)
# steel$val <- steel$BON[[1]]
# steel <- steel[,c("Year","val")]
# colnames(steel) <- c("year","steel")
# response <- merge(merge(spCK, faCK), steel)
response <- read.csv("data/response/response.csv")

# lag response (this is now done in the app)
#response$year <- response$year - 2
response <- response[response$year %in% c(1970:2023), ]
save(x = "response", file = 'CMISSTapp/data/responseData.RData')
load(file = 'CMISSTapp/data/responseData.RData')

# Use MCN data as a test for user-defined files
spCK <- fetch_FPC_counts_single_species(my_species="spCK", my_age="adult")
spCK$val <- spCK$MCN[[1]]
response <- spCK[,c("Year","val")]
colnames(response) <- c("year","val")
# lag response
#response$year <- response$year - 2
response <- response[response$year %in% c(1981:2023), ]
write.csv(x = response, file = 'CMISSTapp/data/user_response.csv', row.names = FALSE)



#*******************************
#*  PDO, NPGO, ONI, SSTarc, and PC1
#*******************************

pdo<-read.csv("https://oceanview.pfeg.noaa.gov/erddap/tabledap/cciea_OC_PDO.csv?time,PDO", header = TRUE, stringsAsFactors = FALSE)
pdo<-pdo[-1,]
pdo$PDO<-as.numeric(pdo$PDO)
pdo$year<-as.numeric(substr(pdo$time,1,4))
pdo$month <- as.numeric(substr(pdo$time,6,7))
pdo<-pdo[,c(3,4,2)]
colnames(pdo)[3]<-"index"

# Make the seasonal indices
pdo.win<-pdo
pdo.win<-pdo.win[pdo.win$month %in% 1:3,]
pdo.win<-aggregate(pdo.win$index, by=list(pdo.win$year), mean, na.rm=TRUE)
colnames(pdo.win)<-c("year","pdo.win")
pdo.spr<-pdo[pdo$month %in% 4:6,]
pdo.spr<-aggregate(pdo.spr$index, by=list(pdo.spr$year), mean, na.rm=TRUE)
colnames(pdo.spr)<-c("year","pdo.spr")
pdo.sum<-pdo[pdo$month %in% 7:9,]
pdo.sum<-aggregate(pdo.sum$index, by=list(pdo.sum$year), mean, na.rm=TRUE)
colnames(pdo.sum)<-c("year","pdo.sum")
pdo.aut<-pdo[pdo$month %in% 10:12,]
pdo.aut<-aggregate(pdo.aut$index, by=list(pdo.aut$year), mean, na.rm=TRUE)
colnames(pdo.aut)<-c("year","pdo.aut")

otherIndicators<-merge(merge(merge(pdo.win, pdo.spr), pdo.sum), pdo.aut)

# NPGO
npgo<-read.csv("https://oceanview.pfeg.noaa.gov/erddap/tabledap/cciea_OC_NPGO.csv?time,NPGO", header = TRUE, stringsAsFactors = FALSE)
npgo<-npgo[-1,]
npgo$NPGO<-as.numeric(npgo$NPGO)
npgo$year<-as.numeric(substr(npgo$time,1,4))
npgo$month <- as.numeric(substr(npgo$time,6,7))
npgo<-npgo[,c(3,4,2)]
colnames(npgo)[3]<-"index"
# Make the seasonal indices
npgo.win<-npgo
npgo.win<-npgo.win[npgo.win$month %in% 1:3,]
npgo.win<-aggregate(npgo.win$index, by=list(npgo.win$year), mean, na.rm=TRUE)
colnames(npgo.win)<-c("year","npgo.win")
npgo.spr<-npgo[npgo$month %in% 4:6,]
npgo.spr<-aggregate(npgo.spr$index, by=list(npgo.spr$year), mean, na.rm=TRUE)
colnames(npgo.spr)<-c("year","npgo.spr")
npgo.sum<-npgo[npgo$month %in% 7:9,]
npgo.sum<-aggregate(npgo.sum$index, by=list(npgo.sum$year), mean, na.rm=TRUE)
colnames(npgo.sum)<-c("year","npgo.sum")
npgo.aut<-npgo[npgo$month %in% 10:12,]
npgo.aut<-aggregate(npgo.aut$index, by=list(npgo.aut$year), mean, na.rm=TRUE)
colnames(npgo.aut)<-c("year","npgo.aut")

otherIndicators<-merge(merge(merge(merge(otherIndicators, npgo.win), npgo.spr), npgo.sum), npgo.aut)

# ONI
oni<-read.csv("https://oceanview.pfeg.noaa.gov/erddap/tabledap/cciea_OC_ONI.csv?time,ONI", header = TRUE, stringsAsFactors = FALSE)
oni<-oni[-1,]
oni$ONI<-as.numeric(oni$ONI)
oni$year<-as.numeric(substr(oni$time,1,4))
oni$month <- as.numeric(substr(oni$time,6,7))
oni<-oni[,c(3,4,2)]
colnames(oni)[3]<-"index"
# Make the seasonal indices
oni.win<-oni
oni.win<-oni.win[oni.win$month %in% 1:3,]
oni.win<-aggregate(oni.win$index, by=list(oni.win$year), mean, na.rm=TRUE)
colnames(oni.win)<-c("year","oni.win")
oni.spr<-oni[oni$month %in% 4:6,]
oni.spr<-aggregate(oni.spr$index, by=list(oni.spr$year), mean, na.rm=TRUE)
colnames(oni.spr)<-c("year","oni.spr")
oni.sum<-oni[oni$month %in% 7:9,]
oni.sum<-aggregate(oni.sum$index, by=list(oni.sum$year), mean, na.rm=TRUE)
colnames(oni.sum)<-c("year","oni.sum")
oni.aut<-oni[oni$month %in% 10:12,]
oni.aut<-aggregate(oni.aut$index, by=list(oni.aut$year), mean, na.rm=TRUE)
colnames(oni.aut)<-c("year","oni.aut")

otherIndicators<-merge(merge(merge(merge(otherIndicators, oni.win), oni.spr), oni.sum), oni.aut)

# SSTarc
arc<-read.csv("../OceanTeamData/scripts/indicators/ersstArc/ersstArc.anom.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(arc)[3]<-"index"
# Make the seasonal indices
arc.win<-arc
arc.win<-arc.win[arc.win$month %in% 1:3,]
arc.win<-aggregate(arc.win$index, by=list(arc.win$year), mean, na.rm=TRUE)
colnames(arc.win)<-c("year","arc.win")
arc.spr<-arc[arc$month %in% 4:6,]
arc.spr<-aggregate(arc.spr$index, by=list(arc.spr$year), mean, na.rm=TRUE)
colnames(arc.spr)<-c("year","arc.spr")
arc.sum<-arc[arc$month %in% 7:9,]
arc.sum<-aggregate(arc.sum$index, by=list(arc.sum$year), mean, na.rm=TRUE)
colnames(arc.sum)<-c("year","arc.sum")
arc.aut<-arc[arc$month %in% 10:12,]
arc.aut<-aggregate(arc.aut$index, by=list(arc.aut$year), mean, na.rm=TRUE)
colnames(arc.aut)<-c("year","arc.aut")

otherIndicators<-merge(merge(merge(merge(otherIndicators, arc.win), arc.spr), arc.sum), arc.aut)

save(otherIndicators, file = 'CMISSTapp/data/otherIndicators.RData')

# If we add PC1 from the stoplight, we're going to have lots of NAs,
#  so let's save it in a different file
# SSTarc
pc1<-read.csv("../../../indicators/2024/Stoplight_for_forecasts.csv", header = TRUE, stringsAsFactors = FALSE)
pc1<-pc1[,c("Year","PC1")]; colnames(pc1)[1]<-"year"

otherIndicators_pc1<-merge(otherIndicators, pc1)
save(otherIndicators_pc1, file = 'CMISSTapp/data/otherIndicators_pc1.RData')
