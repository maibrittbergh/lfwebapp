

server= function(input, output, session){


  Qmin_trend=function(data, station, mod=1) {

    nbr=which(names(data)==station)
    val=data[[nbr]]
    abs_min=min(data[[nbr]][,2])

    year_one=as.numeric(substring(as.character(data[[nbr]][1,1]),1,4))
    length=length(data[[nbr]][,1])
    last_year=as.numeric(substring(as.character(data[[nbr]][length,1]),1,4))
    years=c(year_one:last_year)
    l=length(years)
    q_min=rep(0, l)
    for ( i in 1:l){
      year=as.character(years[i])
      j=grep(year, data[[nbr]][,1])
      Val=data[[nbr]][,2][j]
      q_min[i]=min(Val)
    }
    results=data.frame(years, q_min)
    model= min_trend(data, station)


    if(mod==1){
      titl=paste("Yuepilon and Linear Trend of Minimum Values at",station)
      cap=paste("Absolute Minimum is: ", abs_min, "slope: Trend Line- Sens Sloap:",model$slope_zyp,"slope: Trend Line- Least Squares:", model$slope_lm)
      plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" , y="Minimum Discharge Value", caption=cap)+
        geom_abline(aes(intercept = model$intercept_zyp, slope= model$slope_zyp,  col="b"), show.legend=TRUE)+
        geom_abline(aes(intercept= model$intercept_lm, slope=model$slope_lm,col="c"), show.legend=TRUE)+  scale_color_manual(name = "Legend:   ",
                                                                                                                             labels=c("Minimum values", "Trend Line - Sens Sloap",
                                                                                                                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "#00BDD0", "c"="darkblue"), guide="legend")+ theme(legend.position = "bottom" )

    }else if (mod==2){

      titl=paste("Yuepilon  Trend of Minimum Values at",station)
      cap=paste("Absolute Minimum is: ", abs_min, "slope: Trend Line- Sens Sloap:",model$slope_zyp)
      plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" , y="Minimum Discharge Value", caption=cap)+
        geom_abline(aes(intercept = model$intercept_zyp, slope= model$slope_zyp,  col="b"), show.legend=TRUE)+
        scale_color_manual(name = "Legend:   ",
                           labels=c("Minimum values", "Trend Line - Sens Sloap"
                           ), values=c("a"="#F8766D","b"= "#00BDD0"), guide="legend")+ theme(legend.position = "bottom" )
    }else if(mod==3){

      titl=paste("Linear Trend of Minimum Values at",station)
      cap=paste("Absolute Minimum is: ", abs_min,"slope: Trend Line- Least Squares:", model$slope_lm)
      plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" , y="Minimum Discharge Value", caption=cap)+
        geom_abline(aes(intercept= model$intercept_lm, slope=model$slope_lm,col="c"), show.legend=TRUE)+  scale_color_manual(name = "Legend:   ",
                                                                                                                             labels=c("Minimum values",
                                                                                                                                      "Trend Line-Least Squares"), values=c("a"="#F8766D", "c"="darkblue"), guide="legend")+ theme(legend.position = "bottom" )
    }




    return(plot)


  }



  # Introduction ------------------------------------------------------------



  query_modal <- modalDialog(
    title = "This Webapp uses the GRDC-dataset for a statistical low flow analysis in Germany. ",

    easyClose = F,
    footer = tagList(
      actionButton("start_window", "Lege los!")
    )
  )

  # Show the model on start up ...
  showModal(query_modal)

  observeEvent(input$start_window, {
    removeModal()
  })



  # First Page --------------------------------------------------------------




  # Map  ---------------------------------------------------------------------


  map = createLeafletMap(session, 'map')









  session$onFlushed(once = T, function() {


    getColor <- function(data) {
      sapply(data$rep_stat, function(rep_stat) {
        if(rep_stat==F) {
          "green"

        } else {
          "orange"
        } })
    }

    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(data)
    )



    output$map <- renderLeaflet({
      leaflet(data) %>%
        addTiles() %>%
        addAwesomeMarkers(lat = ~latitude, lng = ~longitude, icon=icons,
                          clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T),


                          popup = ~paste(
                            paste('<b>', 'Fluss:', '</b>', river),
                            paste('<b>',  'Station:', '</b>', station),
                            paste('<b>',  'Zeitspanne [Jahre]:', '</b>', d_years ),

                            sep = '<br/>'),
                          popupOptions = popupOptions(closeButton = FALSE)
        )  %>%
        addLegend("topright", colors = c("orange","green"), values = c("orange", "green"),labels = c("rep. Station", "Station"),
                  title = "Legende",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1
        )%>%
        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )
    })
  })


  # Table ---------------------------------------------------------------
  ?datatable

  output$table_input=DT::renderDataTable({
    DT::datatable(data, selection='single', rownames=FALSE,colnames= c("GRDC Nr."="grdc_no" , "Fluss"=   "river"   ,"Station"=   "station", "Land"=    "country",
                                                                       "Einzugsgebietsgröße"="catch_area", "Höhe"= "altitude" ,"Beginn der Messung"="startday", "Ende der Messung"="endday", "Startjahr"= "startyear" , "Endjahr"= "endyear" ,
                                                                       "Zeitspanne"=   "d_years"   , "Längengrad"= "longitude"  , "Breitengrad"="latitude"   ), filter="top",
                  options = list(scrollY = '600px', paging = FALSE, scrollX=TRUE, dom="ltipr")


    )
  })




  # reactive Values

  # Help Button -------------------------------------------------------------


  #Help Button

  observeEvent(input$help,{
    showModal(modalDialog(
      title = "Benötigen Sie Hilfe?",
      "Bitte Wählen sie eine Station auf der Karte oder in der Tabelle und eine Analysemethode. Die Tabelle bietet die Option die Stationen in Bezug auf ein bestimmtes Merkmal zu sortieren, oder nach einer Eigenschaft zu suchen. Die Berechnung der Trendwerte nimmt etwas Zeit in Anspruch. Um eine neue Analyse oder Station zu wählen,
      empfiehlt es sich  >Lösche Darstellungsoptionen< zu klicken. Das verbessert die Stabilität der Anwendung."

      ,
      easyClose = F,
      footer = tagList(
        actionButton("help1", "Schließen")
      )
    ))
  })

  observeEvent(input$help1, {
    removeModal()
  })


  observeEvent(input$helpthres,{
    showModal(modalDialog(
      title = "Benötigen Sie Hilfe?",
      "Wählen Sie einen Schwellenwert aus. Dieser kann entweder numerisch oder individuell gewählt werden. Dieser Schwellenwert charakterisiert im Anschluss die Niedrigwasserperiode." ,

      easyClose = F,
      footer = tagList(
        actionButton("help2", "Schließen")
      ))
    )
  })

  observeEvent(input$help2, {
    removeModal()
  })


  # Empty functions ---------------------------------------------------------


  #Initial conditions: 'Select station on map.'
  t_plot <- function(){

    tpl= plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Bitte eine Station auswählen", line = -1, cex = 1.5)
    return(tpl)
  }






  output$disch_plot=renderPlot({t_plot()})
  output$disch_plot=renderPlot({empty()})




  empty=   function(){

    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Bitte eine Station auswählen", line = -1, cex = 1.5)
    return(plot)

  }

  output$disch_plot <- renderPlot({empty()})


  trendpl= function(){

    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Bitte eine Station auswählen ", line = -1, cex = 1.5)
    return(plot)


  }


  # selpl=   function(){

  #  plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
  # mtext(paste("Station:", stat_name, "selected"), line = -1, cex = 1.5)
  #return(plot)

  #}




  output$disch_plot=renderPlot({empty()})


  output$trendplot=renderPlot({trendpl()})



  thres= function(){

    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Bitte eine Station auswählen  ", line = -1, cex = 1.5)
    return(plot)


  }

  output$thresplot= renderPlot({thres()})


  # Reactive Map -----------------------------------------------------


  #Dummy which gets selected gauge
  gauge_sel <-  shiny::reactiveValues(clicked_gauge = "XXX")

  #Reaction to selection of station on map
  observeEvent(input$map_marker_click,{

    gauge_sel$clicked_gauge <- input$map_marker_click

    stat_sel <- which(data$latitude == gauge_sel$clicked_gauge$lat)

    stat_name <- data$station[stat_sel] #station name



    #read discharge time series
    disc_data <- data2[[stat_name]]

    observe({

      sta_yea_cla <- as.numeric(format(disc_data[1,1], "%Y"))

      end_yea_cla <- as.numeric(format(disc_data[nrow(disc_data),1], "%Y"))-1

      updateSliderInput(session, "year", label = "Jahr: ",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "year2", label = "Jahr: ",
                        min = sta_yea_cla, max = end_yea_cla)


      updateSliderInput(session, "yearq", label = "Jahr: ",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "yearv", label = "Jahr: ",
                        min = sta_yea_cla, max = end_yea_cla)



    })

    observe({

      sta_yea_cla <- as.numeric(format(disc_data[1,1], "%Y"))

      end_yea_cla <- as.numeric(format(disc_data[nrow(disc_data),1], "%Y"))

      rast_time_init <- c(sta_yea_cla, end_yea_cla)


      updateNumericInput(session, "ssy", label = "Startjahr:", sta_yea_cla,
                         min = sta_yea_cla+1, max = end_yea_cla-1)
      updateNumericInput(session, "sey", label = "Endjahr:", sta_yea_cla+1,
                         min = sta_yea_cla+1, max = end_yea_cla-1)

    })



    t_plot <- function(){


      if(input$qplot_variety == "Abflussganglinie"){

        if (input$pettitt1){
          Qplot=Qplot(data2, stat_name, T)
        }else{

          Qplot=Qplot(data2, stat_name, F)
        }
        return(Qplot)

      }
      if(input$qplot_variety == "jährliche Abflussganglinie"){

        if (input$hyeardis){
          Year=input$year2

          if (input$pettitt2){
            qploty=Qploty(data2, stat_name, year=Year,h=T, pettitt=T)
          }else{  qploty=Qploty(data2, stat_name, year=Year,h=T, pettitt=F)    }
        }else{
          Year=input$year2
          if (input$pettitt2){
            qploty=Qploty(data2, stat_name, year=Year,h=F, pettitt=T)
          }else{  qploty=Qploty(data2, stat_name, year=Year,h=F, pettitt=F)    }

        }

        return(qploty)

      }
      if(input$qplot_variety == "jährlicher Boxplot der Messwerte"){


        Year=input$year
        qboxploty=QBoxploty(data=data2,  station=stat_name, year=Year, h=T)
        return(qboxploty)

      }
      if(input$qplot_variety == "Boxplot der Messwerte"){

        qboxplot=QBoxplot(data=data2,  station=stat_name)
        return(qboxplot)

      }


    }

    output$disch_plot <- renderPlot({t_plot()})


    if(input$qplot_variety == "Plot der Jahreszeiten"){

      observeEvent(input$printplot, {
        Startyear=input$ssy
        Endyear=input$sey
        month_start=input$season1
        month_end=input$season2

        seasonplot=seasonpl(data=data2, station=stat_name, Startyear=Startyear, Endyear=Endyear, month_start=month_start, month_end =month_end )

        output$disch_plot <- renderPlot({seasonplot})
      })


    }


    #  trendpl=function(){

    #"season_trend", "Choose Season", c("Year", "Winter", "Frühling", "Sommer", "Herbst"))

    observeEvent(input$season_trend,{
      if (input$season_trend=="Jahr"){
        season="Y"
      }
      if (input$season_trend=="Herbst"){
        season="AU"
      }
      if (input$season_trend=="Winter"){
        season="WI"
      }
      if (input$season_trend=="Frühling"){
        season="SP"
      }
      if (input$season_trend=="Sommer"){
        season="SU"
      }



      observeEvent(input$season_trend_2,{
        if (input$season_trend_2=="Jahr"){
          seas="Y"
        }
        if (input$season_trend_2=="Herbst"){
          seas="AU"
        }
        if (input$season_trend_2=="Winter"){
          seas="WI"
        }
        if (input$season_trend_2=="Frühling"){
          seas="SP"
        }
        if (input$season_trend_2=="Sommer"){
          seas="SU"
        }





        trendpl=function(){
          if (input$trendpltype=="Trend der Minimumwerte"){

            plotr=Qmin_trend(data=data2,  station=stat_name, mod=1)
            return(plotr)
          }
          if (input$trendpltype=="NMxQ-Trend"){
            x_val=input$xVALUE

            plotr=NMxQ_trend(data=data2,  station=stat_name, x=x_val, seasonal=season, graphic=T)
            return(plotr)
          }
          if (input$trendpltype=="Trend der Mittelwerte"){


            plotr=MQ_trend(data=data2,  station=stat_name, seasonal=seas )
            return(plotr)
          }



        }


        output$trendplot=renderPlot({trendpl()})


      })


    })









    thres= function(){
      if(input$thres_type=="Quantilbasiert"){

        quant=input$quantile
        quantile=1-quant
        Year=input$yearq

        qperipl=periodplot_quantile(data2, stat_name , quantile, year=Year, graph=T)
        return( qperipl)
      }
      if(input$thres_type=="Numerischer Wert"){

        Val=input$value
        Year=input$yearv

        uperipl=U_periodploty(data2, stat_name ,U= Val, year=Year, graph=T)
        return( uperipl)
      }



    }
    output$thresplot=renderPlot({thres()})





  }) #Observe Event Map/Marker/Table Marker Click finishes



  observeEvent(input$cleardata, {
    output$disch_plot=renderPlot({empty()})
  })


  observeEvent(input$cleardata2, {
    output$trendplot=renderPlot({empty()})
  })



  observeEvent(input$cleardata3, {
    output$thresplot=renderPlot({empty()})
  })



  #Dummy which gets selected gauge
  gauge_sel <-  shiny::reactiveValues(clicked_gauge = "XXX")


  # Reactive Table ----------------------------------------------------------


  observeEvent(input$table_input_row_last_clicked,{
    s= input$table_input_row_last_clicked

    #gauge_sel$clicked_gauge <- input$tableId_row_last_clicked


    #stat_sel <- which(data$latitude == gauge_sel$clicked_gauge$lat)

    stat_name <- data$station[s] #station name



    #read discharge time series





    disc_data <- data2[[stat_name]]

    observe({

      sta_yea_cla <- as.numeric(format(disc_data[1,1], "%Y"))

      end_yea_cla <- as.numeric(format(disc_data[nrow(disc_data),1], "%Y"))-1

      updateSliderInput(session, "year", label = "Jahr: ",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "year2", label = "Jahr: ",
                        min = sta_yea_cla, max = end_yea_cla)



      updateSliderInput(session, "yearq", label = "Jahr: ",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "yearv", label = "Jahr: ",
                        min = sta_yea_cla, max = end_yea_cla)



    })

    observe({

      sta_yea_cla <- as.numeric(format(disc_data[1,1], "%Y"))

      end_yea_cla <- as.numeric(format(disc_data[nrow(disc_data),1], "%Y"))

      rast_time_init <- c(sta_yea_cla, end_yea_cla)


      updateNumericInput(session, "ssy", label = "Startjahr:", sta_yea_cla,
                         min = sta_yea_cla+1, max = end_yea_cla-1)
      updateNumericInput(session, "sey", label = "Endjahr:", sta_yea_cla+1,
                         min = sta_yea_cla+1, max = end_yea_cla-1)

    })


    t_plot <- function(){


      if(input$qplot_variety == "Abflussganglinie"){

        Qplot=Qplot(data2, stat_name)
        return(Qplot)

      }
      if(input$qplot_variety == "jährliche Abflussganglinie"){


        Year=input$year2
        qploty=Qploty(data2, stat_name, year=Year,h=T)
        return(qploty)

      }
      if(input$qplot_variety == "jährlicher Boxplot der Messwerte"){


        Year=input$year
        qboxploty=QBoxploty(data=data2,  station=stat_name, year=Year, h=T)
        return(qboxploty)

      }
      if(input$qplot_variety == "Boxplot der Messwerte"){

        qboxplot=QBoxplot(data=data2,  station=stat_name)
        return(qboxplot)

      }


    }

    output$disch_plot <- renderPlot({t_plot()})


    if(input$qplot_variety == "Plot der Jahreszeiten"){

      observeEvent(input$printplot, {
        Startyear=input$ssy
        Endyear=input$sey
        month_start=input$season1
        month_end=input$season2

        plja=seasonpl(data=data2, station=stat_name, Startyear=Startyear, Endyear=Endyear, month_start=month_start, month_end =month_end )


      })
      output$disch_plot <- renderPlot({plja})

    }


    #  trendpl=function(){

    #"season_trend", "Choose Season", c("Year", "Winter", "Frühling", "Sommer", "Herbst"))

    observeEvent(input$season_trend,{
      if (input$season_trend=="Jahr"){
        season="Y"
      }
      if (input$season_trend=="Herbst"){
        season="AU"
      }
      if (input$season_trend=="Winter"){
        season="WI"
      }
      if (input$season_trend=="Frühling"){
        season="SP"
      }
      if (input$season_trend=="Sommer"){
        season="SU"
      }



      observeEvent(input$season_trend_2,{
        if (input$season_trend_2=="Jahr"){
          seas="Y"
        }
        if (input$season_trend_2=="Herbst"){
          seas="AU"
        }
        if (input$season_trend_2=="Winter"){
          seas="WI"
        }
        if (input$season_trend_2=="Frühling"){
          seas="SP"
        }
        if (input$season_trend_2=="Sommer"){
          seas="SU"
        }





        trendpl=function(){
          if (input$trendpltype=="Trend der Minimumwerte"){

            plotr=Qmin_trend(data=data2,  station=stat_name, mod=1)
            return(plotr)
          }
          if (input$trendpltype=="NMxQ-Trend"){
            x_val=input$xVALUE

            plotr=NMxQ_trend(data=data2,  station=stat_name, x=x_val, seasonal=season, graphic=T)
            return(plotr)
          }
          if (input$trendpltype=="Trend der Mittelwerte"){


            plotr=MQ_trend(data=data2,  station=stat_name, seasonal=seas )
            return(plotr)
          }



        }


        output$trendplot=renderPlot({trendpl()})


      })


    })









    thres= function(){
      if(input$thres_type=="Quantilbasiert"){


        quant=input$quantile
        quantile=1-quant
        Year=input$yearq

        qperipl=periodplot_quantile(data2, stat_name , quantile, year=Year, graph=T)
        return( qperipl)
      }
      if(input$thres_type=="Numerischer Wert"){

        Val=input$value
        Year=input$yearv

        uperipl=U_periodploty(data2, stat_name ,U= Val, year=Year, graph=T)
        return( uperipl)
      }



    }
    output$thresplot=renderPlot({thres()})






  })#Observe Event Map/Marker/Table Marker Click finishes


























  # second Page: Trend Analysis ---------------------------------------------


  # germany Map -------------------------------------------------------------

  map = createLeafletMap(session, 'datamap')









  session$onFlushed(once = T, function() {

    mapdata=data


    output$datamap <- renderLeaflet({
      leaflet(mapdata) %>%
        clearPopups() %>%
        clearMarkers() %>%
        addTiles() %>%

        addCircleMarkers(lat = ~latitude, lng = ~longitude,



                         popup = ~paste(
                           paste('<b>', 'Fluss:', '</b>', river),
                           paste('<b>',  'Station:', '</b>', station),
                           paste('<b>',  'Länge der Messreihe [in Jahren]:', '</b>', d_years ),

                           sep = '<br/>'),
                         popupOptions = popupOptions(closeButton = FALSE)
        )  %>%

        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )
    })
  })










  #season, color collected


  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !;
    left:50;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))


  tag.map.title2 <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !;
    left: 50;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))
  #Meantrend



  observeEvent(input$go,{

    mapd=MQlist
    mapdata=mapd[[input$timerange2]]



    if(input$dataset=="Nur representative Stationen"){

      l=  length(mapdata$station)

      iden=rep(F,l)
      for ( i in 1:l){
        iden[i]=is.element(mapdata$station[i], repres)

      }


      mapdata=mapdata[which(iden==T),] }else{

        mapdata=mapdata

      }




    if(input$trendtypemq== "Zyp: Prewhitening und Sen-Slope Trend"){

      #######






      sequence=c(-0.65, -0.45, -0.25, -0.01, 0, 0.01,0.25, 0.45, 0.65)
      lsl=length(sequence)

      risk.bins =sequence

      #scico(lsl, palette = 'hawaii')
      risk.pal<- colorBin(scico(lsl, palette = 'hawaii'), bins=risk.bins, na.color = "#aaff56")


      #######

      if (input$seasonmq=="Frühling"){

        ######


        title <- tags$div(
          tag.map.title2, HTML(" Frühling - Zyp Trend der Mittelwerte")
        )

        Spzyp= as.numeric(mapdata$Spslopezyp)





        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Spzyp),




                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Spzyp),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

          #va,s=Spzyp

          addLegend(opacity=1,pal= risk.pal, values=sequence, position="topleft",  title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c( "Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )





        ######













      }
      if (input$seasonmq=="Sommer"){


        #####


        title <- tags$div(
          tag.map.title2, HTML(" Sommer - Zyp Trend der Mittelwerte ")
        )

        Szyp= as.numeric(mapdata$Sslopezyp)

        # pal=colorNumeric("RdYlBu", domain=  Szyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%




          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude,  color = ~ risk.pal(Szyp),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>',Szyp ),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####



























      }
      if (input$seasonmq=="Herbst"){



        #####



        title <- tags$div(
          tag.map.title2, HTML(" Herbst - Zyp Trend der Mittelwerte")
        )

        Azyp= as.numeric(mapdata$Aslopezyp)

        # pal=colorNumeric("RdYlBu", domain=  Azyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~  risk.pal(Azyp),#pal(Azyp),



                           popup = ~paste(

                             paste('<b>',  'Wert:', '</b>',Azyp),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal= risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####



      }
      if (input$seasonmq=="Winter"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Winter - Zyp Trend der Mittelwerte")
        )

        Wzyp= as.numeric(mapdata$Wslopezyp)

        #  pal=colorNumeric("RdYlBu", domain=  Wzyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Wzyp),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Wzyp),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal,position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####




      }
      if (input$seasonmq=="Year"){

        #####

        title <- tags$div(
          tag.map.title2, HTML("Hydrologisches Jahr - Zyp Trend der Mittelwerte")
        )

        Yzyp= as.numeric(mapdata$Yslopezyp)

        # pal=colorNumeric("RdYlBu", domain=  Yzyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Yzyp),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Yzyp),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )


        #####


      }



    }



    if(input$trendtypemq== "Lineare Regression"){



      sequence=c(-0.65, -0.45, -0.25, -0.01, 0, 0.01,0.25, 0.45, 0.65)
      lsl=length(sequence)

      risk.bins =sequence

      #scico(lsl, palette = 'hawaii')
      risk.pal<- colorBin(scico(lsl, palette = 'hawaii'), bins=risk.bins, na.color = "#aaff56")

      if (input$seasonmq=="Frühling"){



        #####


        title <- tags$div(
          tag.map.title2, HTML(" Frühling - Lineare Regression - Trend der Mittelwerte")
        )

        Splm= as.numeric(mapdata$Spslopelm)

        # pal=colorNumeric("RdYlBu", domain=  Splm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Splm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>',   Splm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1,
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        ####



      }
      if (input$seasonmq=="Sommer"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Sommer - Lineare Regression - Trend der Mittelwerte ")
        )

        Slm= as.numeric(mapdata$Sslopelm)

        # pal=colorNumeric("RdYlBu", domain=  Slm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Slm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Slm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####



      }
      if (input$seasonmq=="Herbst"){


        #####


        title <- tags$div(
          tag.map.title2, HTML(" Herbst - Lineare Regression - Trend der Mittelwerte")
        )

        Alm= as.numeric(mapdata$Aslopelm)

        # pal=colorNumeric("RdYlBu", domain=  Alm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Alm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Alm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        #####



      }
      if (input$seasonmq=="Winter"){


        #####


        title <- tags$div(
          tag.map.title2, HTML(" Winter - Lineare Regression - Trend der Mittelwerte")
        )

        Wlm= as.numeric(mapdata$Wslopelm)

        # pal=colorNumeric("RdYlBu", domain=  Wlm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Wlm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Wlm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        #####



      }
      if (input$seasonmq=="Year"){


        #####


        title <- tags$div(
          tag.map.title2, HTML("Hydrologisches Jahr - Lineare Regression - Trend der Mittelwerte ")
        )

        Ylm= as.numeric(mapdata$Yslopelm)

        #pal=colorNumeric("RdYlBu", domain=  Ylm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~ risk.pal(Ylm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Ylm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal= risk.pal, position="topleft", values=sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )


        #####



      }
    }

    #mapdata$Spsigzyp)
    if(input$trendtypemq== "Mann-Kendall Signifikanztest (Sen-Slope Trend)"){

      sequence=c(0, 0.01,0.02,0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1,1)
      lsl=length(sequence)

      risk.bins =sequence


      risk.pal<- colorBin( "viridis", bins=risk.bins, na.color = "#aaff56")


      if (input$seasonmq=="Frühling"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Frühling - Signifikanz des Sen-Slope Trends ")
        )

        Spsig_= as.numeric(mapdata$Spsigzyp)

        #pal=colorNumeric("viridis", reverse = T, domain=  Spsig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Spsig_),



                           popup = ~paste(

                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  Spsig_, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        #####



      }
      if (input$seasonmq=="Sommer"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Sommer - Signifikanz des Sen-Slope Trends ")
        )

        Ssig_= as.numeric(mapdata$Ssigzyp)

        pal=colorNumeric("viridis", reverse = T,domain=  Ssig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Ssig_),



                           popup = ~paste(

                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  Ssig_, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####



      }
      if (input$seasonmq=="Herbst"){

        #####


        title <- tags$div(
          tag.map.title2, HTML(" Herbst - Signifikanz des Sen-Slope Trends ")
        )

        Asig_= as.numeric(mapdata$Asigzyp)

        pal=colorNumeric("viridis", reverse = T,domain=  Asig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Asig_),



                           popup = ~paste(

                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  Asig_, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        #####




      }
      if (input$seasonmq=="Winter"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Winter - Signifikanz des Sen-Slope Trends ")
        )

        Wsig_= as.numeric(mapdata$Wsigzyp)

        pal=colorNumeric("viridis", reverse = T,domain=  Wsig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Wsig_),



                           popup = ~paste(

                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  Wsig_, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####





      }

      if (input$seasonmq=="Year"){

        #####



        title <- tags$div(
          tag.map.title2, HTML("Hydrologisches Jahr - Signifikanz des Sen-Slope Trends  ")
        )

        Ysig_= as.numeric(mapdata$Ysigzyp)

        pal=colorNumeric("viridis", reverse = T,domain=  Ysig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Ysig_),



                           popup = ~paste(

                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  Ysig_, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )
        #####









      }



    }


  })
  #Mintrend






  ###### NMxQ



  observeEvent(input$go_NMxQ,{


    if (input$xval=="14"){
      mapd=NMxQlist14

    }

    if (input$xval=="60"){
      mapd=NMxQlist60

    }else{
      mapd=NMxQlist7}





    mapdata=mapd[[input$timerange2]]


    if(input$dataset=="Nur representative Stationen"){

      l=  length(mapdata$station)

      iden=rep(F,l)
      for ( i in 1:l){
        iden[i]=is.element(mapdata$station[i], repres)

      }


      mapdata=mapdata[which(iden==T),] }else{

        mapdata=mapdata

      }




    if(input$trendtypemq2== "Zyp: Prewhitening und Sen-Slope Trend"){





      sequence=c(-4,-2,-1.5,-1,-0.5, -0.01, 0, 0.01,0.5, 1, 1.5, 2,4)
      lsl=length(sequence)

      risk.bins =sequence


      risk.pal<- colorBin( scico(lsl, palette = 'hawaii'), bins=risk.bins, na.color = "#aaff56")




      if (input$seasonmq2=="Frühling"){

        ######


        title <- tags$div(
          tag.map.title2, HTML(" Frühling - Zyp Trend der NMxQ-Werte")
        )

        Spzyp= as.numeric(mapdata$Spslopezyp)

        #  pal=colorNumeric("RdYlBu", domain=  Spzyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Spzyp),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>',Spzyp ),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )





        ######













      }
      if (input$seasonmq2=="Sommer"){


        #####


        title <- tags$div(
          tag.map.title2, HTML(" Sommer - Zyp Trend der NMxQ-Werte ")
        )

        Szyp= as.numeric(mapdata$Sslopezyp)

        #  pal=colorNumeric("RdYlBu", domain=  Szyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Szyp),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Szyp),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####



























      }
      if (input$seasonmq2=="Herbst"){



        #####



        title <- tags$div(
          tag.map.title2, HTML(" Herbst - Zyp Trend der NMxQ-Werte")
        )

        Azyp= as.numeric(mapdata$Aslopezyp)

        # pal=colorNumeric("RdYlBu", domain=  Azyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Azyp),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Azyp),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####



      }
      if (input$seasonmq2=="Winter"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Winter - Zyp Trend der NMxQ-Werte")
        )

        Wzyp= as.numeric(mapdata$Wslopezyp)

        # pal=colorNumeric("RdYlBu", domain=  Wzyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~ risk.pal(Wzyp),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Wzyp),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal= risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####




      }
      if (input$seasonmq2=="Jahr"){

        #####

        title <- tags$div(
          tag.map.title2, HTML("   Hydrologisches Jahr - Zyp Trend der NMxQ-Werte")
        )

        Yzyp= as.numeric(mapdata$Yslopezyp)

        #  pal=colorNumeric("RdYlBu", domain=  Yzyp)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Yzyp),



                           popup = ~paste(

                             paste('<b>',  'Wert:', '</b>', Yzyp),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=sequence, labels=c("< -7.5", as.character(sequence2), ">7.5"), title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )


        #####


      }



    }



    if(input$trendtypemq2== "Lineare Regression"){


      sequence=c(-4,-2,-1.5,-1,-0.5, -0.01, 0, 0.01,0.5, 1, 1.5, 2,4)
      lsl=length(sequence)

      risk.bins =sequence


      risk.pal<- colorBin( scico(lsl, palette = 'hawaii'), bins=risk.bins, na.color = "#aaff56")



      if (input$seasonmq2=="Frühling"){



        #####


        title <- tags$div(
          tag.map.title2, HTML(" Frühling - Lineare Regression - Trend der NMxQ-Werte")
        )

        Splm= as.numeric(mapdata$Spslopelm)

        # pal=colorNumeric("RdYlBu", domain=  Splm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Splm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Splm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        ####



      }
      if (input$seasonmq2=="Sommer"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Sommer - Lineare Regression - Trend der NMxQ-Werte ")
        )

        Slm= as.numeric(mapdata$Sslopelm)

        # pal=colorNumeric("RdYlBu", domain=  Slm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~ risk.pal(Slm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Slm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal= risk.pal, position="topleft", values=sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####



      }
      if (input$seasonmq2=="Herbst"){


        #####


        title <- tags$div(
          tag.map.title2, HTML(" Herbst - Lineare Regression - Trend der NMxQ-Werte")
        )

        Alm= as.numeric(mapdata$Aslopelm)

        #pal=colorNumeric("RdYlBu", domain=  Alm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~  risk.pal(Alm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Alm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=  risk.pal, position="topleft", values= sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        #####



      }
      if (input$seasonmq2=="Winter"){


        #####


        title <- tags$div(
          tag.map.title2, HTML(" Winter - Lineare Regression - Trend der NMxQ-Werte")
        )

        Wlm= as.numeric(mapdata$Wslopelm)

        # pal=colorNumeric("RdYlBu", domain=  Wlm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~ risk.pal(Wlm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Wlm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal= risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        #####



      }
      if (input$seasonmq2=="Jahr"){


        #####


        title <- tags$div(
          tag.map.title2, HTML("Hydrologisches Jahr - Lineare Regression - Trend der NMxQ-Werte ")
        )

        Ylm= as.numeric(mapdata$Yslopelm)

        #  pal=colorNumeric("RdYlBu", domain=  Ylm)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Ylm),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Ylm),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )


        #####



      }
    }

    #mapdata$Spsigzyp)
    if(input$trendtypemq2== "Mann-Kendall Signifikanztest (Sen-Slope Trend)"){


      sequence=c(0, 0.01,0.02,0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1,1)
      lsl=length(sequence)

      risk.bins =sequence


      risk.pal<- colorBin( "viridis", bins=risk.bins, na.color = "#aaff56")

      if (input$seasonmq2=="Frühling"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Frühling - Signifikanz des Sen-Slope Trends ")
        )

        Spsig_= as.numeric(mapdata$Spsigzyp)

        #pal=colorNumeric("viridis", reverse = T, domain=  Spsig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Spsig_),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Spsig_),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        #####



      }
      if (input$seasonmq2=="Sommer"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Sommer - Signifikanz des Sen-Slope Trends ")
        )

        Ssig_= as.numeric(mapdata$Ssigzyp)

        #  pal=colorNumeric("viridis", reverse = T,domain=  Ssig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Ssig_),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Ssig_),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####



      }
      if (input$seasonmq2=="Herbst"){

        #####


        title <- tags$div(
          tag.map.title2, HTML(" Herbst - Signifikanz des Sen-Slope Trends ")
        )

        Asig_= as.numeric(mapdata$Asigzyp)

        #  pal=colorNumeric("viridis", reverse = T,domain=  Asig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~ risk.pal(Asig_),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Asig_),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal= risk.pal, position="topleft", values=  sequence, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )




        #####




      }
      if (input$seasonmq2=="Winter"){


        #####



        title <- tags$div(
          tag.map.title2, HTML(" Winter - Signifikanz des Sen-Slope Trends ")
        )

        Wsig_= as.numeric(mapdata$Wsigzyp)

        # pal=colorNumeric("viridis", reverse = T,domain=  Wsig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Wsig_),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Wsig_),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )



        #####





      }

      if (input$seasonmq2=="Jahr"){

        #####



        title <- tags$div(
          tag.map.title2, HTML("Hydrologisches Jahr - Signifikanz des Sen-Slope Trends  ")
        )

        Ysig_= as.numeric(mapdata$Ysigzyp)

        #pal=colorNumeric("viridis", reverse = T,domain=  Ysig_)

        leafletProxy("datamap",session )%>%
          clearPopups() %>%
          clearControls()%>%
          clearMarkers() %>%
          addTiles() %>%

          addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(Ysig_),



                           popup = ~paste(
                             paste('<b>',  'Wert:', '</b>', Ysig_),
                             paste('<b>',  'Station:', '</b>', station),
                             paste('<b>',  'Fluss:', '</b>', river),


                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE), opacity = 1
          )  %>%

          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



          addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Kendall's P-Wert", labFormat = labelFormat(digits = 6))%>%
          addControl(title, position="topright", className="map-title")%>%

          addLayersControl(

            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",

            options = layersControlOptions(collapsed = F)
          )
        #####









      }



    }


  })


  #####Minimum Period


  observeEvent(input$go_2,{


    mapd=Periodmeta
    mapdata=mapd[[input$timerange2]]

    if(input$dataset=="Nur representative Stationen"){

      l=  length(mapdata$station)

      iden=rep(F,l)
      for ( i in 1:l){
        iden[i]=is.element(mapdata$station[i], repres)

      }


      mapdata=mapdata[which(iden==T),]
    }else{

      mapdata=mapdata

    }


    if(input$trendtypeperiod=="Zyp: Prewhitening und Sen-Slope Trend"){










      if(input$periodway=="Länge der maximalen Niedrigwasserperiode"){

        sequence=c(-2,-1.0,-0.5 , -0.01, 0, 0.01,0.5 , 1, 2)
        lsl=length(sequence)

        risk.bins =sequence


        risk.pal<- colorBin(    scico(lsl, palette = 'hawaii', direction = -1),   bins=risk.bins, na.color = "#aaff56")


        if(input$quantiles=="70"){



          ?colorBin


          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter (Q70) - Zyp Trend")
          )

          tmaxzypQ70= as.numeric(mapdata$Q70_tmax_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T,domain=    tmaxzypQ70)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%

            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(tmaxzypQ70),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxzypQ70),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



            addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )





        }
        if(input$quantiles=="75"){



          #####




          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter (Q75) - Zyp Trend")
          )

          tmaxzypQ75= as.numeric(mapdata$Q75_tmax_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ75)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(tmaxzypQ75),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxzypQ75),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%


            addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%


            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )







          ######







        }
        if(input$quantiles=="80"){




          #####



          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter (Q80) - Zyp Trend")
          )

          tmaxzypQ80= as.numeric(mapdata$Q80_tmax_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ80)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(tmaxzypQ80),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxzypQ80),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%




            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )

          ####



        }
        if(input$quantiles=="85"){




          #####



          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter (Q85) - Zyp Trend")
          )

          tmaxzypQ85= as.numeric(mapdata$Q85_tmax_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ85)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(tmaxzypQ85),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxzypQ85),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%


            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )



          #####






        }
        if(input$quantiles=="90"){



          #####


          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter (Q90) - Zyp Trend")
          )

          tmaxzypQ90= as.numeric(mapdata$Q90_tmax_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ90)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(tmaxzypQ90),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxzypQ90),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%


            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          ####





        }
        if(input$quantiles=="95"){
          #####

          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter (Q95) - Zyp Trend")
          )

          tmaxzypQ95= as.numeric(mapdata$Q95_tmax_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ95)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(tmaxzypQ95),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxzypQ95),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )

          ####







        }


      }

      if(input$periodway=="Anzahl der Tage unter Schwellenwert"){

        sequence=c(-5, -4, -3,  -2,-1.0,-0.5 , -0.01, 0, 0.01,0.5 , 1, 2, 3, 4, 5)
        lsl=length(sequence)

        risk.bins =sequence


        risk.pal<- colorBin(    scico(lsl, palette = 'hawaii', direction = -1), bins=risk.bins, na.color = "#aaff56")


        if(input$quantiles=="70"){
          #####

          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q70) - Zyp Trend")
          )

          ldzypQ70= as.numeric(mapdata$Q70_ld_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ70)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(   ldzypQ70),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',ldzypQ70),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=    sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )

          ####






        }
        if(input$quantiles=="75"){



          #######

          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q75) - Zyp Trend")
          )

          ldzypQ75= as.numeric(mapdata$Q75_ld_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ75)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(   ldzypQ75),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',ldzypQ975),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #######







        }
        if(input$quantiles=="80"){

          #####



          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q80) - Zyp Trend")
          )

          ldzypQ80= as.numeric(mapdata$Q80_ld_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ80)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(   ldzypQ80),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',ldzypQ80),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )





          #####






        }
        if(input$quantiles=="85"){




          ######




          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q85) - Zyp Trend")
          )

          ldzypQ85= as.numeric(mapdata$Q85_ld_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ85)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(   ldzypQ85),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',ldzypQ85),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=    sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )



          #####





        }
        if(input$quantiles=="90"){


          ######

          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q90) - Zyp Trend")
          )

          ldzypQ90= as.numeric(mapdata$Q90_ld_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ90)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(   ldzypQ90),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',ldzypQ90),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )



          #####





        }
        if(input$quantiles=="95"){



          #######


          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q95) - Zyp Trend")
          )

          ldzypQ95= as.numeric(mapdata$Q95_ld_zyp)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ95)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(   ldzypQ95),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',ldzypQ95),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=    sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          ######











        }





      }




    }


    if(input$trendtypeperiod=="Lineare Regression"){




      sequence=c( -2,-1.0,-0.5 , -0.01, 0, 0.01,0.5 , 1, 2)
      lsl=length(sequence)

      risk.bins =sequence


      risk.pal<- colorBin(    scico(lsl, palette = 'hawaii', direction=-1),  bins=risk.bins, na.color = "#aaff56")


      if(input$periodway=="Länge der maximalen Niedrigwasserperiode"){
        sequence=c( -2,-1.0,-0.5 , -0.01, 0, 0.01,0.5 , 1, 2)
        lsl=length(sequence)

        risk.bins =sequence


        risk.pal<- colorBin( scico(lsl, palette = 'hawaii', direction=-1) ,  bins=risk.bins, na.color = "#aaff56")
        if(input$quantiles=="70"){

          #####


          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter(Q70) - Lineare Regression -Trend")
          )

          tmaxlmQ70= as.numeric(mapdata$Q70_tmax_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ70)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  tmaxlmQ70),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',  tmaxlmQ70),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )



          ######





        }
        if(input$quantiles=="75"){




          ######



          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter(Q75) - Lineare Regression -Trend")
          )

          tmaxlmQ75= as.numeric(mapdata$Q75_tmax_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ75)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  tmaxlmQ75),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',  tmaxlmQ75 ),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )




          #######








        }
        if(input$quantiles=="80"){



          #######



          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter(Q80) - Lineare Regression -Trend")
          )

          tmaxlmQ80= as.numeric(mapdata$Q80_tmax_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ80)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  tmaxlmQ80),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',  tmaxlmQ80),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )





          #######







        }
        if(input$quantiles=="85"){


          ######


          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter(Q85) - Lineare Regression -Trend")
          )

          tmaxlmQ85= as.numeric(mapdata$Q85_tmax_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ85)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  tmaxlmQ85),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxlmQ85),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )

          ##########







        }
        if(input$quantiles=="90"){


          #########


          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter(Q90) - Lineare Regression -Trend")
          )

          tmaxlmQ90= as.numeric(mapdata$Q90_tmax_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ90)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  tmaxlmQ90),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',  tmaxlmQ90),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )





          #########





        }
        if(input$quantiles=="95"){


          #######



          title <- tags$div(
            tag.map.title, HTML("Maximale Zeitspanne unter(Q95) - Lineare Regression -Trend")
          )

          tmaxlmQ95= as.numeric(mapdata$Q95_tmax_lm)

          Periodmeta[[1]]$Q95_tmax_lm

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ95)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  tmaxlmQ95),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',  tmaxlmQ95),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )




          #######






        }


      }

      if(input$periodway=="Anzahl der Tage unter Schwellenwert"){


        sequence=c(-5, -4, -3,  -2,-1.0,-0.5 , -0.01, 0, 0.01,0.5 , 1, 2, 3, 4, 5)
        lsl=length(sequence)

        risk.bins =sequence


        risk.pal<- colorBin(   scico(lsl, palette = 'hawaii', direction=-1),  bins=risk.bins, na.color = "#aaff56")

        if(input$quantiles=="70"){



          #####

          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q70) - Lineare Regression -Trend")
          )

          ldlmQ70= as.numeric(mapdata$Q70_ld_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ70)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( ldlmQ70),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldlmQ70),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )





          #####







        }
        if(input$quantiles=="75"){



          #######

          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q75) - Lineare Regression -Trend")
          )

          ldlmQ75= as.numeric(mapdata$Q75_ld_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ75)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( ldlmQ75),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldlmQ75),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )



          ######





        }
        if(input$quantiles=="80"){


          #######


          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q80) - Lineare Regression -Trend")
          )

          ldlmQ80= as.numeric(mapdata$Q80_ld_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ80)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( ldlmQ80),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldlmQ80),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #######





        }
        if(input$quantiles=="85"){


          #####


          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q85) - Lineare Regression -Trend")
          )

          ldlmQ85= as.numeric(mapdata$Q85_ld_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ85)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( ldlmQ85),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldlmQ85),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )

          #####







        }
        if(input$quantiles=="90"){

          ######


          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q90) - Lineare Regression -Trend")
          )

          ldlmQ90= as.numeric(mapdata$Q90_ld_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ90)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( ldlmQ90),



                             popup = ~paste(

                               paste('<b>',  'Wert:', '</b>', ldlmQ90),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=    sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )

          ######





        }
        if(input$quantiles=="95"){


          #####


          title <- tags$div(
            tag.map.title, HTML("Anzahl der Tage unter Schwellenwert(Q95) - Lineare Regression -Trend")
          )

          ldlmQ95= as.numeric(mapdata$Q95_ld_lm)

          #pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ95)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( ldlmQ95),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldlmQ95),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Slope")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          ####




        }



      }


    }

    if(input$trendtypeperiod=="Mann-Kendall Signifikanztest (Sen-Slope Trend)"){
      sequence=c(0, 0.01,0.02,0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1,1)
      lsl=length(sequence)

      risk.bins =sequence


      risk.pal<- colorBin( "viridis", bins=risk.bins, na.color = "#aaff56")






      if(input$periodway=="Länge der maximalen Niedrigwasserperiode"){

        if(input$quantiles=="70"){


          #####


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Maximale Zeitspanne unter(Q70) - Zyp Trend")
          )

          tmaxsigQ70= as.numeric(mapdata$Q70sigtmax)

          #pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ70)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( tmaxsigQ70),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',tmaxsigQ70 ),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )



          #####








        }
        if(input$quantiles=="75"){

          ######



          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Maximale Zeitspanne unter(Q75) - Zyp Trend")
          )

          tmaxsigQ75= as.numeric(mapdata$Q75sigtmax)

          #pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ75)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( tmaxsigQ75),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxsigQ75),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=  sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #####






        }
        if(input$quantiles=="80"){


          ######


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Maximale Zeitspanne unter(Q80) - Zyp Trend")
          )

          tmaxsigQ80= as.numeric(mapdata$Q80sigtmax)

          #pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ80)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( tmaxsigQ80),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxsigQ80),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          ######







        }
        if(input$quantiles=="85"){


          ######


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Maximale Zeitspanne unter(Q85) - Zyp Trend")
          )

          tmaxsigQ85= as.numeric(mapdata$Q85sigtmax)

          #pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ85)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( tmaxsigQ85),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',tmaxsigQ85 ),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values= sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          ######








        }
        if(input$quantiles=="90"){



          ######


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Maximale Zeitspanne unter(Q90) - Zyp Trend")
          )

          tmaxsigQ90= as.numeric(mapdata$Q90sigtmax)

          #pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ90)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( tmaxsigQ90),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxsigQ90),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          ######




        }
        if(input$quantiles=="95"){

          ######


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Maximale Zeitspanne unter(Q95) - Zyp Trend")
          )

          tmaxsigQ95= as.numeric(mapdata$Q95sigtmax)

          #pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ95)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal( tmaxsigQ95),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', tmaxsigQ95),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #####





        }


      }

      if(input$periodway=="Anzahl der Tage unter Schwellenwert"){

        if(input$quantiles=="70"){


          #####

          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Anzahl der Tage unter Schwellenwert(Q70) - Zyp Trend")
          )

          ldsigQ70= as.numeric(mapdata$Q70sigld)

          #pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ70)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  ldsigQ70),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldsigQ70),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence,title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #####






        }
        if(input$quantiles=="75"){



          #####


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Anzahl der Tage unter Schwellenwert(Q75) - Zyp Trend")
          )

          ldsigQ75= as.numeric(mapdata$Q75sigld)

          #pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ75)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  ldsigQ75),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldsigQ75),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #####





        }
        if(input$quantiles=="80"){



          #####


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Anzahl der Tage unter Schwellenwert(Q80) - Zyp Trend")
          )

          ldsigQ80= as.numeric(mapdata$Q80sigld)

          #pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ80)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  ldsigQ80),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldsigQ80),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #####


        }
        if(input$quantiles=="85"){

          #####



          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Anzahl der Tage unter Schwellenwert(Q85) - Zyp Trend")
          )

          ldsigQ85= as.numeric(mapdata$Q85sigld)

          #pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ85)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  ldsigQ85),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',  ldsigQ85),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence,title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )

          #####






        }
        if(input$quantiles=="90"){

          #####


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Anzahl der Tage unter Schwellenwert(Q90) - Zyp Trend")
          )

          ldsigQ90= as.numeric(mapdata$Q90sigld)

          #pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ90)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  ldsigQ90),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>',ldsigQ90),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #####






        }
        if(input$quantiles=="95"){



          #####


          title <- tags$div(
            tag.map.title, HTML("Signifikanz: Anzahl der Tage unter Schwellenwert(Q95) - Zyp Trend")
          )

          ldsigQ95= as.numeric(mapdata$Q95sigld)

          #pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ95)

          leafletProxy("datamap",session )%>%
            clearPopups() %>%
            clearControls()%>%
            clearMarkers() %>%
            addTiles() %>%
            addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~risk.pal(  ldsigQ95),



                             popup = ~paste(
                               paste('<b>',  'Wert:', '</b>', ldsigQ95),
                               paste('<b>',  'Station:', '</b>', station),
                               paste('<b>',  'Fluss:', '</b>', river),


                               sep = '<br/>'),
                             popupOptions = popupOptions(closeButton = FALSE), opacity = 1
            )  %>%

            addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
            addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%

            addLegend(opacity=1,pal=risk.pal, position="topleft", values=   sequence, title="Kendall's P-Wert")%>%
            addControl(title, position="topright", className="map-title")%>%

            addLayersControl(

              baseGroups = c("Open Street Map", "Terrain Background"),
              position = "topright",

              options = layersControlOptions(collapsed = F)
            )


          #####




        }



      }




    }
  })






































  # third Page --------------------------------------------------------------



  # Map, distribution of stations -------------------------------------------









  map = createLeafletMap(session, "stationmap")

  session$onFlushed(once = T, function() {




    output$stationmap <- renderLeaflet({
      leaflet(data) %>%
        clearPopups() %>%
        clearMarkers() %>%
        addTiles() %>%

        addMarkers(lat = ~latitude, lng = ~longitude,



                   popup = ~paste(
                     paste('<b>', 'Fluss:', '</b>', river),
                     paste('<b>',  'Station:', '</b>', station),
                     paste('<b>',  'Länge der Messreihe [in Jahren]:', '</b>', d_years ),
                     paste('<b>',  'Beginn der Messungen:', '</b>', startday ),
                     paste('<b>',  'Ende der Messungen:', '</b>', endday ),
                     paste('<b>',  'Einzugsgebietsgröße [km^2]', '</b>',catch_area ),


                     sep = '<br/>'),
                   popupOptions = popupOptions(closeButton = FALSE)
        )  %>%

        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%



        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )




    })


  })


  observeEvent({input$dataselect}, {
    if(input$dataselect=="Nur representative Stationen"){

      l=  length(data$station)

      iden=rep(F,l)
      for ( i in 1:l){
        iden[i]=is.element(data$station[i], repres)

      }
      if(any(iden)==T){

        filtdata=data[which(iden==T),]



        startyear=input$range[1]

        endyear=input$range[2]


        l=nrow(filtdata) #all stations, included in measurements





        stations_s=rep(F,l)
        stations_e=rep(F,l)
        for ( i in 1:l){
          stations_s[i]=filtdata$startyear[i]<=startyear  #measurements at least as long as given timeseries
          stations_e[i]=filtdata$endyear[i]>=endyear
        }

        start=which(stations_s == TRUE)
        end=which(stations_e == TRUE)
        l=length(start)
        vec=rep(F,l)
        for ( i in 1:l){
          if (identical(which(end==start[i]), integer(0))){
            vec[i]=F
          }else{ vec[i]=T}

        }
        timeseries=start[which(vec==T)]      #filtered. only stations with measurements during whole time included
        l=length(timeseries)
        filtdata=filtdata[timeseries,]











      }else {renderText("No Stations available")}













      leafletProxy("stationmap",session, data=filtdata )%>%
        clearPopups() %>%
        clearMarkers() %>%
        addTiles() %>%
        addMarkers(data=filtdata, lat = ~latitude, lng = ~longitude,



                   popup = ~paste(
                     paste('<b>', 'Fluss:', '</b>', river),
                     paste('<b>',  'Station:', '</b>', station),
                     paste('<b>',  'Länge der Messreihe [in Jahren]:', '</b>', d_years ),
                     paste('<b>',  'Beginn der Messungen:', '</b>', startday ),
                     paste('<b>',  'Ende der Messungen:', '</b>', endday ),
                     paste('<b>',  'Einzugsgebietsgröße [km^2]', '</b>',catch_area ),


                     sep = '<br/>'),
                   popupOptions = popupOptions(closeButton = FALSE)
        )  %>%

        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%




        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )

    }else if(input$dataselect=="Alle GRDC Messstationen"){


      filtdata=data




      startyear=input$range[1]

      endyear=input$range[2]


      l=nrow(filtdata) #all stations, included in measurements





      stations_s=rep(F,l)
      stations_e=rep(F,l)
      for ( i in 1:l){
        stations_s[i]=filtdata$startyear[i]<=startyear  #measurements at least as long as given timeseries
        stations_e[i]=filtdata$endyear[i]>=endyear
      }

      start=which(stations_s == TRUE)
      end=which(stations_e == TRUE)
      l=length(start)
      vec=rep(F,l)
      for ( i in 1:l){
        if (identical(which(end==start[i]), integer(0))){
          vec[i]=F
        }else{ vec[i]=T}

      }
      timeseries=start[which(vec==T)]      #filtered. only stations with measurements during whole time included
      l=length(timeseries)
      filtdata=filtdata[timeseries,]









      leafletProxy("stationmap",session, data=filtdata )%>%
        clearPopups() %>%
        clearMarkers() %>%
        addTiles() %>%
        addMarkers(data=filtdata, lat = ~latitude, lng = ~longitude,




                   popup = ~paste(
                     paste('<b>', 'Fluss:', '</b>', river),
                     paste('<b>',  'Station:', '</b>', station),
                     paste('<b>',  'Länge der Messreihe [in Jahren]:', '</b>', d_years ),
                     paste('<b>',  'Beginn der Messungen:', '</b>', startday ),
                     paste('<b>',  'Ende der Messungen:', '</b>', endday ),
                     paste('<b>',  'Einzugsgebietsgröße [km^2]', '</b>',catch_area ),


                     sep = '<br/>'),
                   popupOptions = popupOptions(closeButton = FALSE)
        )  %>%

        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%




        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )









    }









    observeEvent({input$range},{
      #updateSliderInput(session, "yearq", label = "Select Year:",
      #                 min = sta_yea_cla, max = end_yea_cla)
      updateSliderInput(session, "range", "Zeitrahmen:"
      )

      STA=input$range[1]
      END=input$range[2]


      l=nrow(filtdata) #all stations, included in measurements





      stations_s=rep(F,l)
      stations_e=rep(F,l)
      for ( i in 1:l){
        stations_s[i]=filtdata$startyear[i]<= STA  #measurements at least as long as given timeseries
        stations_e[i]=filtdata$endyear[i]>=  END
      }

      start=which(stations_s == TRUE)
      end=which(stations_e == TRUE)
      l=length(start)
      vec=rep(F,l)
      for ( i in 1:l){
        if (identical(which(end==start[i]), integer(0))){
          vec[i]=F
        }else{ vec[i]=T}

      }
      timeseries=start[which(vec==T)]      #filtered. only stations with measurements during whole time included
      l=length(timeseries)
      filtdata=filtdata[timeseries,]







      leafletProxy("stationmap",session )%>%
        clearPopups() %>%
        clearMarkers() %>%
        addTiles() %>%
        addMarkers(data=filtdata, lat = ~latitude, lng = ~longitude,



                   popup = ~paste(
                     paste('<b>', 'Fluss:', '</b>', river),
                     paste('<b>',  'Station:', '</b>', station),
                     paste('<b>',  'Länge der Messreihe [in Jahren]:', '</b>', d_years ),
                     paste('<b>',  'Beginn der Messungen:', '</b>', startday ),
                     paste('<b>',  'Ende der Messungen:', '</b>', endday ),
                     paste('<b>',  'Einzugsgebietsgröße [km^2]', '</b>',catch_area ),


                     sep = '<br/>'),
                   popupOptions = popupOptions(closeButton = FALSE)
        )  %>%

        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%




        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )





    })














  })































  # Settings Select ---------------------------------------------------------





  # Distribution Graph ------------------------------------------------------



  # selectInput("ddgraph", "Data Distribution Graph", choices=c("Length: Timeseries of Discharge Data", "Einzugsgebietsgrößen" )),


  # conditionalPanel(condition= "input.ddgraph=='Length: Timeseries of Discharge Data'",  radioButtons("densl", "Presentation", choices=c("Density Plot","Colour Map")))



  observeEvent({input$ddgraph}, {




    #   if (input$ddgraph=="Length: Timeseries of Discharge Data"){



    #    observe({


    #     if (input$densl=="Density Plot"){
    #      plot=length_distribution(data, "j")

    #     output$distplot=renderPlot({  plot})

    #      }
    #     if (input$densl=="Colour Map"){
    #      plot=length_distribution(data, "map")
    #     output$tmap=renderTmap({ plot })

    #  }


    #})

    #}

    if (input$ddgraph=="Vergleich der Abflussmengen"){
      observe({
        startyear2=input$yeatise[1]
        endyear2=input$yeatise[2]
        frame_1=input$frametise[1]
        frame_2=input$frametise[2]

        haha=tiseger(data, data2, startyear2, endyear2, frame_1, frame_2)
        output$tisepl= renderPlot({haha})
      })






    }
    if (input$ddgraph=="Einzugsgebietsgrößen"){



      area_plot= ggplot(data)+geom_histogram(aes( x=catch_area/1000),bins=20, fill="darkcyan", col="grey")+ylab("Count")+labs(title="Distribution of Size of Catchmen-Areas")+xlab(expression('Size of Catchemnt Area[km'^2*']  x10'^3))
      output$areapl= renderPlot({area_plot})


    }
    if (input$ddgraph=="Längen der Messreihen"){
      length_plot=ggplot(data)+geom_histogram(aes( x=d_years),bins=20, fill="brown", col="grey")+ylab("Count")+labs(title="Distribution of Length of Measurements [years] ")+xlab("Length of Measurements [years]")
      output$lengthpl=renderPlot({length_plot})
    }


  })



















}











shinyApp(ui=ui, server=server)
#Frühlingfunctions abchecken
#slope ausformulieren in caption
#Achsenbeschriftung
#anders Runden, kleine Flüsse sehen nicht sinnvoll aus ........
# Input Dataset: Timerange and class of stations --------------------------

