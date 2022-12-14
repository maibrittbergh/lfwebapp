
library("shinythemes")
library(gridExtra)


library('scico')

library("dichromat")
#install.packages("knitr")

library(leaflet)
library(readxl)
library(sf)

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

library(dischanalyst)
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





data=read_rds("metadata.rds")
data2=read_rds("dataset.rds")
MQlist=read_rds("NMQlist.rds")
NMxQlist7=read_rds("NNMxQlist7.rds")
NMxQlist14=read_rds("NNMxQlist14.rds")
NMxQlist60=read_rds("NNMxQlist60.rds")
Periodmeta=read_rds("Periodmeta.rds")

# Daten vorbereiten -------------------------------------------------------




repres=relstat=c("HOHENSAATEN-FINOW", "DRESDEN", "MAGDEBURG-STROMBRUECKE",
                 "RATHENOW UP", "CALBE-GRIZEHNE", "INTSCHEDE",  "HANN.-MUENDEN", "VLOTHO",
                 "VERSEN-WEHRDURCHSTICH", "GREVEN", "MAXAU", "KAUB", "KOELN", "COCHEM", "WUERZBURG" , "ROCKENAU SKA", "ACHLEITEN", "BURGHAUSEN", "WASSERBURG", "LANDSBERG", "KEMPTEN")

# neuen Tab einfügen: navbarMenu

ui = navbarPage(title="Low Flow Analysis for Germany", theme = shinytheme("paper"),




                # First Tab ---------------------------------------------------------------
                tabPanel(title="Analysis", #stationsanalyse

                         fluidRow(
                           (column(8,

                                   tabsetPanel(id="ui_tab",

                                               tabPanel("Map", #Karte
                                                        column(12, h4("Choose a station"), shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="800px"),
                                                                                                                          size=3, color="#0080b7"))),
                                               tabPanel("Table", #Tabelle
                                                        column(12, h4("Choose a station"), div(DT::dataTableOutput("table_input"), style = "font-size:70%"))),



                                            #   tabPanel("Users Guide",

                                               #        column(12,
                                               #                includeMarkdown("user_guides/tabone/user_guide_tab-1.Rmd") #including MArkdown for Users Guide

                                                 #       )
                                              # )
                                   )
                           )),


                           #Abschließen der linken Spalte mit Tabelle und Map

                           column(4, conditionalPanel(condition="input.ui_tab!='Anwendungshinweise'",



# Start of low flow analysis ----------------------------------------------



                                                                                      tabsetPanel(id="plot_tabs",
                                                                 tabPanel("Analysis",
                                                                      fluidRow(column(10,

                                                                                      actionButton('help', 'Help'),
                                                                                      selectInput("anatype", "Type of Analysis:", choices=c("Overview Timeseries", "Descriptive Statistics", "Threshold Based", "Trend Analysis")),





                                                                                      conditionalPanel(condition="input.anatype=='Overview Timeseries'",
                                                                                                       selectInput("qplot_variety", label="Options for Timeseries overview:",
                                                                                                                   choices=c("discharge timeseries",   "annual timeseries", "rasterplot", "seasonal plot"))
                                                                                                                             #, "boxplot",  "annual boxplot",   "seasonal plot")) ??? seasonal plot vllt. interessant?



                                                                                      ),



                                                                                      conditionalPanel(condition="input.anatype=='Descriptive Statistics'",
                                                                                                       selectInput("qplot_variety", label="Options for Descriptive Statistics:",
                                                                                                                   choices=c("boxplot", "annual boxplot"))
                                                                                                       #, "boxplot",  "annual boxplot",   "seasonal plot")) ??? seasonal plot vllt. interessant?



                                                                                      ),



                                                                                      conditionalPanel(condition="input.anatype=='Threshold Based'",

                                                                                                       radioButtons("thres_type", "Threshold:", choices=c("quantile based", "numerical value"),
                                                                                                                    inline=T),


                                                                                                       conditionalPanel(condition="input.thres_type=='quantile based'",
                                                                                                                        sliderInput("quantile", label="Quantile based threshold", min=0.05, max=1, value=0.3, step=0.05), sliderInput("yearq", "Year: ", 2000, min=1975, max=2015, sep="")),


                                                                                                       conditionalPanel(condition="input.thres_type=='numerical value'",
                                                                                                                        sliderInput("value", label="Value", min=0, max=3000, value=150,  sep=""), sliderInput("yearv", "Year: ", 2000, min=1975, max=2015, sep="")),

                                                                                                       plotOutput("thresplot", width = "100%")



                                                                                      ),

                                                                                      conditionalPanel(condition="input.anatype=='Descriptive Statistics'",
                                                                                                       selectInput("qplot_variety", label="Options for Descriptive Statistics:",
                                                                                                                   choices=c("boxplot", "annual boxplot"))
                                                                                                       #, "boxplot",  "annual boxplot",   "seasonal plot")) ??? seasonal plot vllt. interessant?



                                                                                      ),












                                                                                           # uiOutput("date_slider") Vielleicht statt Jahr?


                                                                                   #
                                                                                         #  radioButtons("ts_plot_type", "Darstellung:", choices=c("Zeitreihenanalyse", "Trendanalyse (n.schwellenwertbasiert)"),
                                                                                                 #       inline=T), #Functions QBoxplot, QBoxploty, Qplot, Qploty

                                                                                           conditionalPanel(condition="input.ts_plot_type=='Zeitreihenanalyse'",
                                                                                                            selectInput("qplot_variety", label="Optionen der Zeitreihenanalyse:",
                                                                                                                        choices=c("discharge timeseries",   "annual timeseries", "boxplot",  "annual boxplot",   "seasonal plot"))


                                                                                                            ,

                                                                                                            conditionalPanel(condition="input.qplot_variety=='annual boxplot'",  sliderInput("year", "Jahr: ", 2000, min=1975, max=2015, sep="")),

                                                                                                            conditionalPanel(condition="input.qplot_variety=='discharge timeseries'" , checkboxInput("pettitt1", "Pettitt-Test:", value=FALSE)),


                                                                                                            conditionalPanel(condition="input.qplot_variety=='annual timeseries'",  sliderInput("year2", "Jahr: ", 2000, min=1975, max=2015, sep=""), checkboxInput("hyeardis", label="Hydrologisches Jahr", value=TRUE),
                                                                                                                             checkboxInput("pettitt2", "Pettitt-Test", value=FALSE) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='seasonal plot'",  sliderInput("season1", "Anfang des Jahresabschnitts:",5,min=01, max=12)),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='seasonal plot'",  sliderInput("season2", "Ende des Jahresabschnitts:",5,min=01, max=12, ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='seasonal plot'",  numericInput("ssy", "Startjahr:",2000, min=1999, max=2005 ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='seasonal plot'",  numericInput("sey", "Endjahr:",2001, min=1999, max=2005 ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='seasonal plot'",      actionButton("printplot", label="Erstelle Plot")  ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Trendplot'",      renderText({"Loading may take some time. Thank you for your patience."}) ),


                                                                                                            plotOutput("disch_plot", width = "100%"),


                                                                                                            actionButton("cleardata", label="Lösche Darstellungsoptionen")),




                                                                                           conditionalPanel(condition="input.ts_plot_type=='Trendanalyse (n.schwellenwertbasiert)'",
                                                                                                            selectInput("trendpltype", "Optionen der Trendanalyse:", choices=c("Trend der Minimumwerte", "NMxQ-Trend", "Trend der Mittelwerte")),



                                                                                                            conditionalPanel( condition="input.trendpltype=='NMxQ-Trend'",
                                                                                                                              sliderInput("xVALUE", "X-Wert", value=14, min=4, max=90),
                                                                                                                              selectInput("season_trend", "Jahr/Jahreszeit:",choices= c("Jahr", "Winter", "Frühling", "Sommer", "Herbst"))),


                                                                                                            #Trend der Mittewlwerte"){



                                                                                                            conditionalPanel( condition="input.trendpltype=='Trend der Mittelwerte'",

                                                                                                                              selectInput("season_trend_2", "Jahr/Jahreszeit:",choices= c("Jahr", "Winter", "Frühling", "Sommer", "Herbst"))),







                                                                                                            plotOutput("trendplot") %>% withSpinner(color="#0dc5c1"),

                                                                                                            actionButton("cleardata2", label="Lösche Darstellungsoptionen")




                                                                                           )) )),


                                                                  tabPanel("User Guide",


                                                                           fluidRow(column(10,
                                                                                           # uiOutput("date_slider") Vielleicht statt Jahr?

                                                                                           # conditionalPanel(condition="input.qplot_variety=='annual timeseries'",
                                                                                           actionButton('helpthres', 'Hilfe'),
                                                                                           radioButtons("thres_type", "Grenzwert:", choices=c("Quantilbasiert", "Numerischer Wert"),
                                                                                                        inline=T),


                                                                                           conditionalPanel(condition="input.thres_type=='Quantilbasiert'",
                                                                                                            sliderInput("quantile", label="Quantilbasierter Schwellenwert", min=0.05, max=1, value=0.3, step=0.05), sliderInput("yearq", "Jahr: ", 2000, min=1975, max=2015, sep="")),


                                                                                           conditionalPanel(condition="input.thres_type=='Numerischer Wert'",
                                                                                                            sliderInput("value", label="Wert", min=0, max=3000, value=150,  sep=""), sliderInput("yearv", "Jahr: ", 2000, min=1975, max=2015, sep="")),

                                                                                           plotOutput("thresplot", width = "100%")
                                                                                           #,

                                                                                          # actionButton("cleardata3", label="Lösche Darstellungsoptionen")

                                                                           ))



                                                                  )






                                                      )),

                                  conditionalPanel(condition="input.ui_tab='Anwendungshinweise'",
                                                   column(10))


                           ))),














                tags$footer(HTML('
                          <br>
                         <br>
                          <p>Author: Mai-Britt Berghöfer <br>
                          <a href="mailto:berghoefer@uni-potsdam.de">berghoefer@uni-potsdam.de</a></p>'), align = "center"))


