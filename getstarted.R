#setup working environment
library(usethis)
library(devtools)
install_github("maibrittbergh/lfanalyse")
library("shinythemes")
library(gridExtra)
library('scico')
library("dichromat")
library(tmaptools)
library(leaflet)
library(readxl)
library(sf)
library(tmap)
library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.providers)
library(zyp)
library(Kendall)
library(zoo)
library(readr)
library(viridisLite)
library(RColorBrewer)
library(sp)
#install.packages("rgdal")
library(rgdal)
library(DT) #make sure you load DT after Shiny
#library(dischanalyst)
library(shinycssloaders)
library(shinyWidgets)
library(fontawesome)
library(readr)
library(shinyjs)
library(lfanalyse)

#can i delete the other stuff?



# Load own dataset --------------------------------------------------------

path="/Users/maibrittberghofer/Desktop/Arbeit/GRDC_data/2022-11-02_10-59"
Country="DE" #in which country are you interested?
metadata_germany=metadata_grdc(Country, path)

# to avoid mistakes

metadata_repg=metadata_repg(metadata_germany, mark=T)

double=which(metadata_repg$station=="BAD SUELZE")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="BAD TOELZ")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="MITTENWALD")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="PFORZHEIM")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="PLOCHINGEN")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="STEIN")

metadata_repg=metadata_repg[-double[1],]

data=metadata_repg
data2=grdc_reader(data, path)


saveRDS(data, file = "metadata.rds")

saveRDS(data2, file = "dataset.rds")



# creating Lists of Secondplot


# MQLIST ------------------------------------------------------------------



MQlist=vector(mode="list", length=5)
MQlist[[1]]=MQ_1820_2019
MQlist[[2]]=MQ_1860_2019
MQlist[[3]]=MQ_1900_2019
MQlist[[4]]=MQ_1940_2019
MQlist[[5]]=MQ_1980_2019

names(MQlist)= c("1820-2019", "1860-2019", "1900-2019", "1940-2019", "1980-2019")



saveRDS(MQlist, file = "MQlist.rds")




# NMxQ --------------------------------------------------------------------

# 7 -----------------------------------------------------------------------


NMxQlist7=vector(mode="list", length=5)
NMxQlist7[[1]]=nmxq_7_1820_2019
NMxQlist7[[2]]=nmxq_7_1860_2019
NMxQlist7[[3]]=nmxq_7_1900_2019
NMxQlist7[[4]]=nmxq_7_1940_2019
NMxQlist7[[5]]=nmxq_7_1980_2019

View(NMxQlist7)

names(NMxQlist7)=c("1820-2019"
                   , "1860-2019", "1900-2019", "1940-2019", "1980-2019")

saveRDS(NMxQlist7, file = "NMxQlist7.rds")

# 14 ----------------------------------------------------------------------

NMxQlist14=vector(mode="list", length=5)
NMxQlist14[[1]]=nmxq_14_1820_2019
NMxQlist14[[2]]=nmxq_14_1860_2019
NMxQlist14[[3]]=nmxq_14_1900_2019
NMxQlist14[[4]]=nmxq_14_1940_2019
NMxQlist14[[5]]=nmxq_14_1980_2019

View(NMxQlist14)

names(NMxQlist14)=c("1820-2019"
                    , "1860-2019", "1900-2019", "1940-2019", "1980-2019")

saveRDS(NMxQlist14, file = "NMxQlist14.rds")






# 30 ----------------------------------------------------------------------

NMxQlist30=vector(mode="list", length=5)
NMxQlist30[[1]]=nmxq_1860_2019_30  ###################Hier NMxQList 1820-2019 hinzufügen
NMxQlist30[[2]]=nmxq_1860_2019_30
NMxQlist30[[3]]=nmxq_1900_2019_30
NMxQlist30[[4]]=nmxq_1940_2019_30
NMxQlist30[[5]]=nmxq_1980_2019_30

View(NMxQlist30)

names(NMxQlist30)=c("1820-2019"
                    , "1860-2019", "1900-2019", "1940-2019", "1980-2019")

saveRDS(NMxQlist30, file = "NMxQlist30.rds")




# 60 ----------------------------------------------------------------------

NMxQlist60=vector(mode="list", length=5)
NMxQlist60[[1]]=nmxq_1820_2019_60
NMxQlist60[[2]]=nmxq_1860_2019_60
NMxQlist60[[3]]=nmxq_1900_2019_60
NMxQlist60[[4]]=nmxq_1940_2019_60
NMxQlist60[[5]]=nmxq_1980_2019_60

View(NMxQlist60)

names(NMxQlist60)=c("1820-2019"
                    , "1860-2019", "1900-2019", "1940-2019", "1980-2019")

saveRDS(NMxQlist60, file = "NMxQlist60.rds")


# Periodmeta --------------------------------------------------------------



Periodmeta=vector(mode="list", length=5)
Periodmeta[[1]]=period_1820_2019
Periodmeta[[2]]=new_period_1860_2019
Periodmeta[[3]]=period_1900_2019
Periodmeta[[4]]=period_1940_2019
Periodmeta[[5]]=period_1980_2019


names(Periodmeta)=c("1820-2019"
                    , "1860-2019", "1900-2019", "1940-2019", "1980-2019")
View(Periodmeta)


saveRDS(Periodmeta, file = "Periodmeta.rds")
