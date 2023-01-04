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
#install.packages("tmap")
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


