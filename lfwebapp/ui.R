#install.packages("ggplot2")

#update.packages("leaflet")
#install.packages("leaflet")
#install.packages("ggplot2", dep=T)
library(ggplot2)
#install.packages("ggplot2", dep=T)
library("shinythemes")
library("shiny")
#install.packages("usethis")
#install.packages("rmarkdown")
library(gridExtra)






library("shinythemes")

#install.packages("rmarkdown")

library(grmarkdownlibrary(gridExtra))

library(devtools)
library("rmarkdown")
#install.packages("rmarkdown")
library(lfanalyse)
#install.packages("ggpubr")
library(ggpubr)
#devtools::install_github("maibrittbergh/lfanalyse")
library(lfanalyse)


library('scico')
library(dplyr)

library(trend)
library(Kendall)
library(zyp)
library("dichromat")
#install.packages("knitr")
library(leaflet)
library(readxl)
library(sf)
library(lfanalyse)

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
#library(rgdal)
#install.packages("DT")
library(shinycssloaders)
library(shinyWidgets)
library(DT) #make sure you load DT after Shiny
#devtools::install_github("r-lib/conflicted")
#library(dischanalyst)
library(conflicted)
#install.packages("fontawesome")
library(fontawesome)
library(readr)
library(shinyjs)

library(tidyverse)
library(tigris)
library(leaflet)

library(viridis)

library(lfanalyse)

library(png)

library(ggplot2)


library(conflicted)

data=read_rds("metadata.rds")

data2=read_rds("dataset.rds")


#path="/Users/maibrittberghofer/Desktop/Arbeit/GRDC_DATA_DOWNLOAD/Downloaded_GRDC_DATA_13_06_23"

#data=metadata_grdc(Country = "DE", path=path)

#data=metadata_repg(data, mark=T)



#delete incomplete measurements

which(data$station=="BAD SUELZE")
data=data[-241, ]


datam=data
#data2=grdc_reader(data, path)


#data2=grdc_reader(data, path)




#introducinglog10_scaled_Rasterplot
Rasterplot=function(station, data){


  # Creating Dataset --------------------------------------------------------



  nbr=which(names(data)==station)
  dataset=data[[nbr]]


  l= nrow(data[[nbr]])
  dates=rep(0,l)
  for (i in 1:  l){
    dates[i]=as.numeric(substr(data[[nbr]][i,1], 1, 4))
  }

  dataset=cbind(dataset, dates)
  month=rep(0,l)
  for (i in 1:  l){
    month[i]=as.numeric(substr(data[[nbr]][i,1], 6, 7))
  }



  dataset=cbind(dataset, month)

  month_chr=function(datan){
    l= length(datan)
    vec=rep(0,l)
    for ( i in 1:l){
      if (datan[i]==1) {
        h="Jan"
      } else if (datan[i]==2){
        h="Feb"
      } else if (datan[i]==3){
        h="Mar"
      } else if (datan[i]==4){
        h="Apr"
      } else if (datan[i]==5){
        h="May"
      } else if (datan[i]==6){
        h="Jun"
      } else if (datan[i]==7){
        h="Jul"
      }  else if (datan[i]==8){
        h="Aug"
      } else if (datan[i]==9){
        h="Sep"
      } else if (datan[i]==10){
        h="Oct"
      }  else if (datan[i]==11){
        h="Nov"
      } else if (datan[i]==12){
        h="Dec"}



      vec[i]=h
    }
    return(vec)}

  chr=month_chr(dataset$month)


  dataset=cbind(dataset, chr)

  # Statistics of data ------------------------------------------------------


  Mean=mean(dataset$Value)


  med=median(dataset$Value)



  # Graphical ---------------------------------------------------------------

  titl=paste("Rasterplot of Discharge Time Series at: ", station)

  subtitl=paste("from", format(data[[station]][,1], "%Y"),"to", format(data[[station]][(nrow(data[[station]])),1] , "%Y") )[1]
  subtitl

  plot=ggplot(dataset, aes(chr, dates )) +
    geom_raster(aes(fill = Value)) + scale_fill_viridis_c(option = "D", trans = "log10")+scale_x_discrete(limit=c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+ xlab("Month")+ylab("Year") +
    labs(fill=expression('log10(Discharge Value[m'^3*'/s])'), title = titl, subtitle=subtitl)


  return(plot)
}



# Prepare data -------------------------------------------------------






repres=relstat=c("HOHENSAATEN-FINOW", "DRESDEN", "MAGDEBURG-STROMBRUECKE",
                 "RATHENOW UP", "CALBE-GRIZEHNE", "INTSCHEDE",  "HANN.-MUENDEN", "VLOTHO",
                 "VERSEN-WEHRDURCHSTICH", "GREVEN", "MAXAU", "KAUB", "KOELN", "COCHEM", "WUERZBURG" , "ROCKENAU SKA", "ACHLEITEN", "BURGHAUSEN", "WASSERBURG", "LANDSBERG", "KEMPTEN")

# neuen Tab einfügen: navbarMenu

ui = navbarPage(title="Low Flow Analysis", theme = shinytheme("paper"),




                # First Tab ---------------------------------------------------------------
                tabPanel(title="Analysis",

                         fluidRow(
                           (column(8,

                                   tabsetPanel(id="ui_tab",

                                              # tabPanel("Map",
                                                   #     column(12, h4("Choose a station"), shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="800px"),
                                                                                                                     #     size=3, color="#0080b7"))),
                                               tabPanel("Table",
                                                        column(12, h4("Choose a station"), div(DT::dataTableOutput("table_input"), style = "font-size:70%"))),



                                            #   tabPanel("User Guide"
                                               #         ,

                                                 #      column(12,
                                                   #           includeMarkdown("User_Guide.Rmd") #including MArkdown for Users Guide

                                                    #   )
                                               #)
                                   )
                           )),


                           #Abschließen der linken Spalte mit Tabelle und Map

                           column(4, conditionalPanel(condition="input.ui_tab!='User Guide'",

                                                     # tabsetPanel(id="plot_tabs",
                                                      #            tabPanel("Analysetools",
                                                                           fluidRow(column(12,

                                                                                selectInput("ts_plot_type", "Analysing tool", choices=c("Time series Analysis", "Threshold based", "Trend Analysis (n.Threshold based)")



                                                                                            ),













                                                                                           #actionButton('help', 'Hilfe'),
                                                                                   #     #  radioButtons("ts_plot_type", "Analysing tool:", choices=c("Time series Analysis", "Trend Analysis (n.Threshold based)"),
                                                                                    #      #              inline=T), #Functions QBoxplot, QBoxploty, Qplot, Qploty

                                                                                           conditionalPanel(condition="input.ts_plot_type=='Time series Analysis'",
                                                                                                            selectInput("qplot_variety", label="Output Options:",
                                                                                                                        choices=c("Discharge Hydrograph", "Rasterplot", "Time Slice Analysis", "Annual Discharge Hydrograph", "Boxplot",  "Annual Boxplot",   "Seasonal Plot"))


                                                                                                            ,

                                                                                                            conditionalPanel(condition="input.qplot_variety=='Annual Boxplot'",  sliderInput("year", "Year:", 2000, min=1975, max=2015, sep="")),

                                                                                                            conditionalPanel(condition="input.qplot_variety=='Time Slice Analysis'",  sliderInput("tf1", "Period 1:", value=c(1980,1990), min=1975, max=2015, sep=""), sliderInput("tf2", "Period 2:", value=c(1995,2005), min=1975, max=2015, sep="")),

                                                                                                    #     #   conditionalPanel(condition="input.qplot_variety=='Discharge Hydrograph'" , checkboxInput("pettitt1", "Pettitt-Test:", value=FALSE)),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Rasterplot'",  sliderInput("year2", "Year:", 2000, min=1975, max=2015, sep=""), checkboxInput("hyeardis", label="Hydrological Year", value=TRUE)
                                                                                                                            # ,
                                                                                                                             #checkboxInput("pettitt2", "Pettitt-Test", value=FALSE)
                                                                                                                             ),


                                                                                                            conditionalPanel(condition="input.qplot_variety=='Annual Discharge Hydrograph'",  sliderInput("year2", "Year:", 2000, min=1975, max=2015, sep=""), checkboxInput("hyeardis", label="Hydrological Year", value=TRUE)
                                                                                                                              ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Seasonal Plot'",  sliderInput("season1", "Start of the period:",5,min=01, max=12)),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Seasonal Plot'",  sliderInput("season2", "End of the period:",5,min=01, max=12, ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Seasonal Plot'",  numericInput("ssy", "Start year:",2000, min=1999, max=2005 ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Seasonal Plot'",  numericInput("sey", "End year:",2001, min=1999, max=2005 ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Seasonal Plot'",      actionButton("printplot", label="Create Plot")  ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Trendplot'",      renderText({"Loading may take some time. Thank you for your patience."}) ),



                                                                                                            plotOutput("disch_plot", width = "100%"),


                                                                                                  downloadButton("DB", "Save plot")
                                                                                                #  ,


                                                                                                            #actionButton("cleardata", label="Lösche Analysing toolsoptionen")
                                                                                                            ),




                                                                                           conditionalPanel(condition="input.ts_plot_type=='Trend Analysis (n.Threshold based)'",
                                                                                                            selectInput("trendpltype", "Output Options:", choices=c("Trend of Minimum Values ", "NMxQ-Trend", "Trend of Mean Values ")),



                                                                                                            conditionalPanel( condition="input.trendpltype=='NMxQ-Trend'",
                                                                                                                              sliderInput("xVALUE", "X-Value", value=14, min=3, max=20),
                                                                                                                              selectInput("season_trend", "Year/Season: ",choices= c("Year", "Winter", "Spring", "Summer", "Autumn"))),


                                                                                                            #Trend der Mittewlwerte"){



                                                                                                            conditionalPanel( condition="input.trendpltype=='Trend of Mean Values '",

                                                                                                                              selectInput("season_trend_2", "Year/Season: ",choices= c("Year", "Winter", "Spring", "Summer", "Autumn"))),







                                                                                                            plotOutput("trendplot") %>% withSpinner(color="#0dc5c1"),

                                                                                                            #actionButton("cleardata2", label="Lösche Darstellungsoptionen")
                                                                                                            downloadButton("DB1", "Save plot")




                                                                                           ),


                                                     conditionalPanel(condition="input.ts_plot_type=='Threshold based'",


                                                                      actionButton('helpthres', 'Hilfe'),
                                                                      radioButtons("thres_type", "Threshold Value:", choices=c("Quantile based", "Numerical Value"),
                                                                                   inline=T),


                                                                      conditionalPanel(condition="input.thres_type=='Quantile based'",
                                                                                       sliderInput("quantile", label="Quantile based Threshold", min=0.05, max=1, value=0.3, step=0.05), sliderInput("yearq", "Year:", 2000, min=1975, max=2015, sep="")),


                                                                      conditionalPanel(condition="input.thres_type=='Numerical Value'",
                                                                                       sliderInput("value", label="Value", min=0, max=3000, value=150,  sep=""), sliderInput("yearv", "Year:", 2000, min=1975, max=2015, sep="")),

                                                                      plotOutput("thresplot", width = "100%"),

                                                                      #actionButton("cleardata3", label="Lösche Darstellungsoptionen")

                                                                      downloadButton("DB2", "Save plot")
                                                     )



                                                     ) )),









                                                  #    )),

                                #  conditionalPanel(condition="input.ui_tab='User Guide'",
                                            #       column(10))


                           )))



















)

