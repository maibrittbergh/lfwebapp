#install.packages("ggplot2")
library("shinythemes")

library(gridExtra)

#install_github("maibrittbergh/lfanalyse")
library(lfanalyse)

#install_github("maibrittbergh/lfanalyse")
library(lfanalyse)

library('scico')

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
library(rgdal)
#install.packages("DT")

library(DT) #make sure you load DT after Shiny

#library(dischanalyst)
library(shinycssloaders)
library(shinyWidgets)
#install.packages("fontawesome")
library(fontawesome)
library(readr)
library(shinyjs)


library(tidyverse)
library(tigris)
library(leaflet)

library(viridis)

library(lfanalyse)


library(ggplot2)
data=read_rds("metadata.rds")
data2=read_rds("dataset.rds")


# Daten vorbereiten -------------------------------------------------------




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

                                               tabPanel("Map",
                                                        column(12, h4("Choose a station"), shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="800px"),
                                                                                                                          size=3, color="#0080b7"))),
                                               tabPanel("Table",
                                                        column(12, h4("Choose a station"), div(DT::dataTableOutput("table_input"), style = "font-size:70%"))),



                                               tabPanel("User Guide"
                                                        #,

                                                     #   column(12,
                                                      #         includeMarkdown("user_guides/tabone/user_guide_tab-1.Rmd") #including MArkdown for Users Guide

                                                      #  )
                                               )
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

                                  conditionalPanel(condition="input.ui_tab='User Guide'",
                                                   column(10))


                           ))),




















                tags$footer(HTML('
                          <br>
                         <br>
                          <p>Author: Mai-Britt Berghöfer <br>
                          <a href="mailto:berghoefer@uni-potsdam.de">berghoefer@uni-potsdam.de</a></p>'), align = "center"))

