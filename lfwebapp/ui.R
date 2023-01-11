
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

ui = navbarPage(title="Niedrigwasseranalyse für Deutschland", theme = shinytheme("paper"),




                # First Tab ---------------------------------------------------------------
                tabPanel(title="Stationsanalyse",

                         fluidRow(
                           (column(7,

                                   tabsetPanel(id="ui_tab",

                                               tabPanel("Karte",
                                                        column(12, h4("Wähle eine Station"), shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="800px"),
                                                                                                                          size=3, color="#0080b7"))),
                                               tabPanel("Tabelle",
                                                        column(12, h4("Wähle eine Station"), div(DT::dataTableOutput("table_input"), style = "font-size:70%"))),



                                               tabPanel("Anwendungshinweise"
                                                        #,

                                                     #   column(12,
                                                      #         includeMarkdown("user_guides/tabone/user_guide_tab-1.Rmd") #including MArkdown for Users Guide

                                                      #  )
                                               )
                                   )
                           )),


                           #Abschließen der linken Spalte mit Tabelle und Map

                           column(5, conditionalPanel(condition="input.ui_tab!='Anwendungshinweise'",

                                                     # tabsetPanel(id="plot_tabs",
                                                      #            tabPanel("Analysetools",
                                                                           fluidRow(column(12,

                                                                                selectInput("ts_plot_type", "Darstellung", choices=c("Zeitreihenanalyse", "Schwellenwertbasiert", "Trendanalyse (n.Schwellenwertbasiert)")



                                                                                            ),













                                                                                           #actionButton('help', 'Hilfe'),
                                                                                   #     #  radioButtons("ts_plot_type", "Darstellung:", choices=c("Zeitreihenanalyse", "Trendanalyse (n.schwellenwertbasiert)"),
                                                                                    #      #              inline=T), #Functions QBoxplot, QBoxploty, Qplot, Qploty

                                                                                           conditionalPanel(condition="input.ts_plot_type=='Zeitreihenanalyse'",
                                                                                                            selectInput("qplot_variety", label="Optionen der Zeitreihenanalyse:",
                                                                                                                        choices=c("Abflussganglinie", "Rasterplot der Abflusswerte",  "jährliche Abflussganglinie", "Boxplot der Messwerte",  "jährlicher Boxplot der Messwerte",   "Plot der Jahreszeiten"))


                                                                                                            ,

                                                                                                            conditionalPanel(condition="input.qplot_variety=='jährlicher Boxplot der Messwerte'",  sliderInput("year", "Jahr: ", 2000, min=1975, max=2015, sep="")),

                                                                                                    #     #   conditionalPanel(condition="input.qplot_variety=='Abflussganglinie'" , checkboxInput("pettitt1", "Pettitt-Test:", value=FALSE)),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Rasterplot'",  sliderInput("year2", "Jahr: ", 2000, min=1975, max=2015, sep=""), checkboxInput("hyeardis", label="Hydrologisches Jahr", value=TRUE)
                                                                                                                            # ,
                                                                                                                             #checkboxInput("pettitt2", "Pettitt-Test", value=FALSE)
                                                                                                                             ),


                                                                                                            conditionalPanel(condition="input.qplot_variety=='jährliche Abflussganglinie'",  sliderInput("year2", "Jahr: ", 2000, min=1975, max=2015, sep=""), checkboxInput("hyeardis", label="Hydrologisches Jahr", value=TRUE)
                                                                                                                              ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  sliderInput("season1", "Anfang des Jahresabschnitts:",5,min=01, max=12)),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  sliderInput("season2", "Ende des Jahresabschnitts:",5,min=01, max=12, ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  numericInput("ssy", "Startjahr:",2000, min=1999, max=2005 ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  numericInput("sey", "Endjahr:",2001, min=1999, max=2005 ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",      actionButton("printplot", label="Erstelle Plot")  ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Trendplot'",      renderText({"Loading may take some time. Thank you for your patience."}) ),



                                                                                                            plotOutput("disch_plot", width = "100%"),


                                                                                                  downloadButton("DB", "Plot sichern")
                                                                                                #  ,


                                                                                                            #actionButton("cleardata", label="Lösche Darstellungsoptionen")
                                                                                                            ),




                                                                                           conditionalPanel(condition="input.ts_plot_type=='Trendanalyse (n.Schwellenwertbasiert)'",
                                                                                                            selectInput("trendpltype", "Optionen der Trendanalyse:", choices=c("Trend der Minimumwerte", "NMxQ-Trend", "Trend der Mittelwerte")),



                                                                                                            conditionalPanel( condition="input.trendpltype=='NMxQ-Trend'",
                                                                                                                              sliderInput("xVALUE", "X-Wert", value=14, min=3, max=20),
                                                                                                                              selectInput("season_trend", "Jahr/Jahreszeit:",choices= c("Jahr", "Winter", "Frühling", "Sommer", "Herbst"))),


                                                                                                            #Trend der Mittewlwerte"){



                                                                                                            conditionalPanel( condition="input.trendpltype=='Trend der Mittelwerte'",

                                                                                                                              selectInput("season_trend_2", "Jahr/Jahreszeit:",choices= c("Jahr", "Winter", "Frühling", "Sommer", "Herbst"))),







                                                                                                            plotOutput("trendplot") %>% withSpinner(color="#0dc5c1"),

                                                                                                            actionButton("cleardata2", label="Lösche Darstellungsoptionen")




                                                                                           ),


                                                     conditionalPanel(condition="input.ts_plot_type=='Schwellenwertbasiert'",


                                                                      actionButton('helpthres', 'Hilfe'),
                                                                      radioButtons("thres_type", "Grenzwert:", choices=c("Quantilbasiert", "Numerischer Wert"),
                                                                                   inline=T),


                                                                      conditionalPanel(condition="input.thres_type=='Quantilbasiert'",
                                                                                       sliderInput("quantile", label="Quantilbasierter Schwellenwert", min=0.05, max=1, value=0.3, step=0.05), sliderInput("yearq", "Jahr: ", 2000, min=1975, max=2015, sep="")),


                                                                      conditionalPanel(condition="input.thres_type=='Numerischer Wert'",
                                                                                       sliderInput("value", label="Wert", min=0, max=3000, value=150,  sep=""), sliderInput("yearv", "Jahr: ", 2000, min=1975, max=2015, sep="")),

                                                                      plotOutput("thresplot", width = "100%"),

                                                                      actionButton("cleardata3", label="Lösche Darstellungsoptionen")
                                                     )



                                                     ) )),









                                                  #    )),

                                  conditionalPanel(condition="input.ui_tab='Anwendungshinweise'",
                                                   column(10))


                           ))),




















                tags$footer(HTML('
                          <br>
                         <br>
                          <p>Author: Mai-Britt Berghöfer <br>
                          <a href="mailto:berghoefer@uni-potsdam.de">berghoefer@uni-potsdam.de</a></p>'), align = "center"))

