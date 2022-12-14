
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



ui = navbarPage(title="Niedrigwasseranalyse für Deutschland", theme = shinytheme("paper"),




                # First Tab ---------------------------------------------------------------
                tabPanel(title="Stationsanalyse",

                         fluidRow(
                           (column(8,

                                   tabsetPanel(id="ui_tab",

                                               tabPanel("Karte",
                                                        column(12, h4("Wähle eine Station"), shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="800px"),
                                                                                                                          size=3, color="#0080b7"))),
                                               tabPanel("Tabelle",
                                                        column(12, h4("Wähle eine Station"), div(DT::dataTableOutput("table_input"), style = "font-size:70%"))),



                                               tabPanel("Anwendungshinweise",

                                                        column(12,
                                                               includeMarkdown("user_guides/tabone/user_guide_tab-1.Rmd") #including MArkdown for Users Guide

                                                        )
                                               )
                                   )
                           )),


                           #Abschließen der linken Spalte mit Tabelle und Map

                           column(4, conditionalPanel(condition="input.ui_tab!='Anwendungshinweise'",

                                                      tabsetPanel(id="plot_tabs",
                                                                  tabPanel("Deskriptive Statistik",
                                                                           fluidRow(column(10,
                                                                                           # uiOutput("date_slider") Vielleicht statt Jahr?


                                                                                           actionButton('help', 'Hilfe'),
                                                                                           radioButtons("ts_plot_type", "Darstellung:", choices=c("Zeitreihenanalyse", "Trendanalyse (n.schwellenwertbasiert)"),
                                                                                                        inline=T), #Functions QBoxplot, QBoxploty, Qplot, Qploty

                                                                                           conditionalPanel(condition="input.ts_plot_type=='Zeitreihenanalyse'",
                                                                                                            selectInput("qplot_variety", label="Optionen der Zeitreihenanalyse:",
                                                                                                                        choices=c("Abflussganglinie",   "jährliche Abflussganglinie", "Boxplot der Messwerte",  "jährlicher Boxplot der Messwerte",   "Plot der Jahreszeiten"))


                                                                                                            ,

                                                                                                            conditionalPanel(condition="input.qplot_variety=='jährlicher Boxplot der Messwerte'",  sliderInput("year", "Jahr: ", 2000, min=1975, max=2015, sep="")),

                                                                                                            conditionalPanel(condition="input.qplot_variety=='Abflussganglinie'" , checkboxInput("pettitt1", "Pettitt-Test:", value=FALSE)),


                                                                                                            conditionalPanel(condition="input.qplot_variety=='jährliche Abflussganglinie'",  sliderInput("year2", "Jahr: ", 2000, min=1975, max=2015, sep=""), checkboxInput("hyeardis", label="Hydrologisches Jahr", value=TRUE),
                                                                                                                             checkboxInput("pettitt2", "Pettitt-Test", value=FALSE) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  sliderInput("season1", "Anfang des Jahresabschnitts:",5,min=01, max=12)),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  sliderInput("season2", "Ende des Jahresabschnitts:",5,min=01, max=12, ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  numericInput("ssy", "Startjahr:",2000, min=1999, max=2005 ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  numericInput("sey", "Endjahr:",2001, min=1999, max=2005 ) ),
                                                                                                            conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",      actionButton("printplot", label="Erstelle Plot")  ),
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


                                                                  tabPanel("Schwellenwertbasiert",


                                                                           fluidRow(column(10,
                                                                                           # uiOutput("date_slider") Vielleicht statt Jahr?

                                                                                           # conditionalPanel(condition="input.qplot_variety=='jährliche Abflussganglinie'",
                                                                                           actionButton('helpthres', 'Hilfe'),
                                                                                           radioButtons("thres_type", "Grenzwert:", choices=c("Quantilbasiert", "Numerischer Wert"),
                                                                                                        inline=T),


                                                                                           conditionalPanel(condition="input.thres_type=='Quantilbasiert'",
                                                                                                            sliderInput("quantile", label="Quantilbasierter Schwellenwert", min=0.05, max=1, value=0.3, step=0.05), sliderInput("yearq", "Jahr: ", 2000, min=1975, max=2015, sep="")),


                                                                                           conditionalPanel(condition="input.thres_type=='Numerischer Wert'",
                                                                                                            sliderInput("value", label="Wert", min=0, max=3000, value=150,  sep=""), sliderInput("yearv", "Jahr: ", 2000, min=1975, max=2015, sep="")),

                                                                                           plotOutput("thresplot", width = "100%"),

                                                                                           actionButton("cleardata3", label="Lösche Darstellungsoptionen")

                                                                           ))



                                                                  )






                                                      )),

                                  conditionalPanel(condition="input.ui_tab='Anwendungshinweise'",
                                                   column(10))


                           ))),













                # Second Tab --------------------------------------------------------------



                tabPanel(title="Niedrigwassertrends für Deutschland",

                         id = "side-panel",

                         fluidRow(
                           column(9,
                                  conditionalPanel(condition="input.area_trend!='Anwendungshinweise'",
                                                   column(12, h4("  "),



                                                          column(12, h4("Klicke auf die Karte, um mehr Infos über eine Station zu erhalten."),h4("  "), shinycssloaders::withSpinner(leaflet::leafletOutput("datamap", height="800px"),
                                                                                                                                                                                     size=3, color="#0080b7") %>% withSpinner(color="#0dc5c1"))),

                                                   column(12, h4("  ")),
                                                   column(12, h4("  ")),
                                                   column(12, h4("  "))

                                  ),


                                  conditionalPanel(condition="input.area_trend='Anwendungshinweise'",

                                                   column(12,includeMarkdown('user_guides/tabtwo/user_guide_tab-2.Rmd') #including MArkdown for Users Guide

                                                   )

                                  )),

                           #  conditionalPanel(condition="input.area_trend=='Anleitung'",

                           #  includeMarkdown()
                           #   ),



                           column(3,

                                  tabsetPanel(id="area_trend",

                                              tabPanel("Einstellungen",
                                                       fluidRow(column(10,






                                                                       selectInput("trendtype2", label="Wähle einen Kennwert: ",
                                                                                   choices=c( "MQ - Trend der Mittelwerte","NMxQ", "Schwellenwertbasiert/Niedrigwasserperiode")) ,

                                                                       radioButtons("dataset", "Stationswahl:", choices=c("Alle GRDC-Messstationen","Nur representative Stationen")),

                                                                       selectInput("timerange2", "Zeitrahmen:",    choices=c("1820-2019", "1860-2019", "1900-2019", "1940-2019", "1980-2019")),
                                                                       #"1980-2020")),





                                                                       #MQ-Mean Discharge Trend

                                                                       conditionalPanel(condition="input.trendtype2=='MQ - Trend der Mittelwerte'",
                                                                                        selectInput("seasonmq", label="Saison/Jahr:",
                                                                                                    choices=c("Frühling",   "Sommer", "Herbst", "Winter",
                                                                                                              "Jahr")) ,

                                                                                        selectInput("trendtypemq", label="Wahl der Trendmethode:",
                                                                                                    choices=c( "Lineare Regression", "Zyp: Prewhitening und Sen-Slope Trend", "Mann-Kendall Signifikanztest (Sen-Slope Trend)")),
                                                                                        actionButton("go", "Ergebnisse laden")),


                                                                       #NMxQ


                                                                       conditionalPanel(condition="input.trendtype2=='NMxQ'",
                                                                                        selectInput("xval", label="Wahl von x:",
                                                                                                    choices=c("7","14", "60")) ,

                                                                                        selectInput("seasonmq2", label="Saison/Jahr:",
                                                                                                    choices=c("Frühling",   "Sommer", "Herbst", "Winter",
                                                                                                              "Jahr")),

                                                                                        selectInput("trendtypemq2", label="Wahl der Trendmethode:",
                                                                                                    choices=c( "Lineare Regression", "Zyp: Prewhitening und Sen-Slope Trend", "Mann-Kendall Signifikanztest (Sen-Slope Trend)")),
                                                                                        actionButton("go_NMxQ", "Ergebnisse laden")),







                                                                       ###Periodmeta


                                                                       conditionalPanel(condition="input.trendtype2=='Schwellenwertbasiert/Niedrigwasserperiode'",





                                                                                        selectInput("periodway", "Kennwert:", choices=c("Länge der maximalen Niedrigwasserperiode","Anzahl der Tage unter Schwellenwert")),




                                                                                        selectInput("trendtypeperiod", label="Wahl der Trendmethode:",
                                                                                                    choices=c( "Lineare Regression", "Zyp: Prewhitening und Sen-Slope Trend", "Mann-Kendall Signifikanztest (Sen-Slope Trend)")),

                                                                                        selectInput("quantiles", label="Quantil [%]:",
                                                                                                    choices=c("70","75", "80","85", "90", "90", "95")) ,
                                                                                        actionButton("go_2", "Ergebnisse laden"))


















                                                       ))),


                                              tabPanel("Anwendungshinweise",
                                                       fluidRow(column(10))



                                              ))))


                ),














                # Third Page --------------------------------------------------------------



                navbarMenu(title="Der Datensatz",



                           # MAP ---------------------------------------------------------------------


                           tabPanel("Infokarte",





                                    fluidRow(
                                      column(7,


                                             tabPanel("Lage der Stationen",
                                                      column(12, h4("Klicke auf die Karte, um mehr Infos über eine Station zu erhalten."), shinycssloaders::withSpinner(leaflet::leafletOutput("stationmap", height="800px"),
                                                                                                                                                                        size=3, color="#0080b7"))) ,




                                      ),

                                      column(5,
                                             tabsetPanel(id="data_dist",
                                                         tabPanel("Einstellungen",
                                                                  fluidRow(column(10,



                                                                                  radioButtons("dataselect", "Wähle einen Datensatz", choices=c("Alle GRDC Messstationen","Nur representative Stationen")),
                                                                                  sliderInput("range", "Zeitrahmen:", value=c(1998
                                                                                                                              ,2004), min=min(data$startyear), max=max(data$endyear), sep="")











                                                                  ))))))),





                           # Graphics ----------------------------------------------------------------

                           # conditionalPanel(condition="input.plot_tabs!='Anleitung'",
                           #tabsetPanel(id="ui_tab",
                           tabPanel("Der Datensatz in Zahlen",


                                    fluidRow(
                                      column(4,

                                             selectInput("ddgraph", "Datensatz", choices=c(
                                               #"Length: Timeseries of Discharge Data",
                                               "Einzugsgebietsgrößen", "Vergleich der Abflussmengen", "Längen der Messreihen")),


                                             # conditionalPanel(condition= "input.ddgraph=='Length: Timeseries of Discharge Data'",  radioButtons("densl", "Presentation", choices=c("Density Plot","Colour Map"))),
                                             conditionalPanel(condition= "input.ddgraph=='Vergleich der Abflussmengen'", sliderInput("yeatise", " Skalierung der X-Achse:", value=c(1950, 2000), min=1820, max=2020, sep="") , sliderInput("frametise", "Skalierung der Y-Achse:", value=c(0, 3100), min=0, max=7000, sep="") )





                                      ),
                                      column(8,
                                             # conditionalPanel(condition= "input.ddgraph=='Length: Timeseries of Discharge Data'",
                                             #conditionalPanel(condition="input.densl=='Density Plot'", plotOutput("distplot", width = "100%", height=400) %>% withSpinner(color="#0dc5c1")),
                                             #conditionalPanel(condition="input.densl=='Colour Map'", tmapOutput("tmap", width = "100%", height = 700) %>% withSpinner(color="#0dc5c1"))) ,

                                             conditionalPanel(condition="input.ddgraph=='Vergleich der Abflussmengen'",  plotOutput("tisepl", width = "100%", height = 700) %>% withSpinner(color="#0dc5c1")) ,

                                             conditionalPanel(condition="input.ddgraph=='Einzugsgebietsgrößen'",  plotOutput("areapl", width = "100%", height = 700) %>% withSpinner(color="#0dc5c1")) ,
                                             conditionalPanel(condition="input.ddgraph=='Längen der Messreihen'",  plotOutput("lengthpl", width = "100%", height = 700) %>% withSpinner(color="#0dc5c1"))


                                      )

                                    )),

                           tabPanel("Informationen",


                                    column(9,
                                           includeMarkdown("user_guides/tabthree/user_guide_tab-3.Rmd") #including MArkdown for Users Guide

                                    )



                           )

                ),

                tabPanel("R-Paket: Dischanalyst",

                         column(9,
                                includeMarkdown("user_guides/tabfour/user_guide_tab-4.Rmd") #including MArkdown for Users Guide

                         )


                ),



                # Pagefive ----------------------------------------------------------------


                navbarMenu(title="Methodik",



                           # MAP ---------------------------------------------------------------------


                           tabPanel("Niedrigwasserkennwerte",


                                    column(9,
                                           includeMarkdown("user_guides/tabfive/NW_ANALYSE/user_guide_tab-5.Rmd") #including MArkdown for Users Guide

                                    )),

                           tabPanel("Statistik",


                                    column(9,
                                           includeMarkdown("user_guides/tabfive/STAT_ANALYSE/user_guide_tab-5Stat.Rmd") #including MArkdown for Users Guide

                                    ))),
                tabPanel("Theoretischer Hintergrund",

                         column(9,
                                includeMarkdown("user_guides/tabsix/user_guide_tab-6.Rmd") #including MArkdown for Users Guide

                         )
                ),












                tags$footer(HTML('
                          <br>
                         <br>
                          <p>Author: Mai-Britt Berghöfer <br>
                          <a href="mailto:berghoefer@uni-potsdam.de">berghoefer@uni-potsdam.de</a></p>'), align = "center"))


