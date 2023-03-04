


server= function(input, output, session){


  U_periodploty=function(data, station, U,year, graph=T){


    data=data[[station]]
    min=paste(as.character(year), "-11-01")
    min=sub(" -", "-",min)
    max=paste(as.character(year+1), "-10-31")
    max=sub(" -", "-",max)
    mindate=grep(min,data[,1])
    maxdate=grep(max,data[,1])
    Val=data[,2]
    datayear=data[mindate:maxdate,]
    valyear=datayear[,2]
    le=length(mindate:maxdate)







    #calculate deficite
    if(any(valyear<U)==F){return(paste("No results.Please select a value higher than",U, "or change the year"))}else{
      g=which(valyear<U)
      vals=valyear[g]
      suml=length(g)
      sum=rep(0, suml)
      for(i in 1:suml){
        sum[i]=U-vals[i]
      }
      deficite=sum(sum)

      uU=g
      l=length(uU)
      c=rep(0,l)#oder l-1
      for ( i in 1:l){
        c[i]=(uU[i+1]-uU[i])
      }



      G=which(c>1)
      c[G]=0  #Vektor c in 0 und 1
      c[is.na(c)] = 0


      d=c
      l=length(c)
      e=rep(0,l)

      for (i in 2:l){
        e[1]=d[1]
        if ((e[i-1]+d[i]) > e[i-1]){e[i]=e[i-1]+d[i]}
        else {e[i]=0}
      }



      if (graph==T){
        plot=ggplot()+labs(title=paste("Low Flow Period at", station, "in", year, "/",year+1), subtitle = paste("Threshold:",round(U,2),  "[m³/s] \n Mean Value:", round( mean(data[,2]),2), "[m³/s]"), caption=paste("Volume of deficite: ",round(deficite,2), "[m³] \n Sum of days under Threshold:", suml, "days \n Longest Low Flow period:", max(e), "days"))+

          ylab(expression('Discharge Value [m'^3*'/s]'))+xlab("Days")+
          geom_polygon(aes(c(datayear$YYYY.MM.DD[1],datayear$YYYY.MM.DD[1],  datayear$YYYY.MM.DD[le], datayear$YYYY.MM.DD[le] ),c(0,U,U,0 ), col="i"), colour="red", fill="brown3")+
          geom_polygon(aes(c(datayear$YYYY.MM.DD[1], datayear$YYYY.MM.D, datayear$YYYY.MM.DD[le] ), c(0, valyear, 0)), colour="cornflowerblue", fill="cornflowerblue")+
          geom_hline(yintercept = U, linetype=2, colour="black")

        plot
        return(plot)}

      else{
        lb= list(paste(year, year+1),U , deficite, suml, max(e))

        names(lb)=c("Hydrological Year", "Threshold[m³/s]", "Deficite [m³]",  "Sum of days under Threshold", "Longest Period in Hydrological Year [days] ")


        return(lb)
      }}

  }

  periodplot_quantile= function (data, station , quantile, year, graph=T){


    data=data[[station]]
    min=paste(as.character(year), "-11-01")
    min=sub(" -", "-",min)
    max=paste(as.character(year+1), "-10-31")
    max=sub(" -", "-",max)
    mindate=grep(min,data[,1])
    maxdate=grep(max,data[,1])
    Val=data[,2]

    U=quantile(Val, probs=quantile, na.rm=T)
    datayear=data[mindate:maxdate,]
    valyear=datayear[,2]
    le=length(mindate:maxdate)







    #calculate deficite
    if(any(valyear<U)==F){return(paste("No results.Please select a value higher than",U, "or change the year"))}else{
      g=which(valyear<U)
      vals=valyear[g]
      suml=length(g)
      sum=rep(0, suml)
      for(i in 1:suml){
        sum[i]=U-vals[i]
      }
      deficite=sum(sum)

      uU=g
      l=length(uU)
      c=rep(0,l)#oder l-1
      for ( i in 1:l){
        c[i]=(uU[i+1]-uU[i])
      }



      G=which(c>1)
      c[G]=0  #Vektor c in 0 und 1
      c[is.na(c)] = 0


      d=c
      l=length(c)
      e=rep(0,l)

      for (i in 2:l){
        e[1]=d[1]
        if ((e[i-1]+d[i]) > e[i-1]){e[i]=e[i-1]+d[i]}
        else {e[i]=0}
      }



      if (graph==T){

        plot=ggplot()+labs(title=paste("Low Flow Period at", station, "in", year, "/",year+1), subtitle = paste("Threshold:",round(U,2), "[m³/s] ~", quantile*100, "% Quantile \n Mean Value:", round( mean(data[,2]),2), "[m³/s]"), caption=paste("Volume of deficite: ",round(deficite,2), "[m³] \n Sum of days under Threshold:", suml, "days \n Longest Low Flow period:", max(e), "days"))+

          ylab(expression('Discharge Value [m'^3*'/s]'))+xlab("Days")+
          geom_polygon(aes(c(datayear$YYYY.MM.DD[1],datayear$YYYY.MM.DD[1],  datayear$YYYY.MM.DD[le], datayear$YYYY.MM.DD[le] ),c(0,U,U,0 ), col="i"), colour="red", fill="brown3")+
          geom_polygon(aes(c(datayear$YYYY.MM.DD[1], datayear$YYYY.MM.D, datayear$YYYY.MM.DD[le] ), c(0, valyear, 0)), colour="cornflowerblue", fill="cornflowerblue")+
          geom_hline(yintercept = U, linetype=2, colour="black")

        plot
        return(plot)}

      else{
        lb= list(paste(year, year+1),U,quantile*100 , deficite, suml, max(e))

        names(lb)=c("Hydrological Year", "Threshold [m³/s]", "Quantile [%]","Deficite [m³]",  "Sum of days under Threshold", "Longest Period in Hydrological Year [days] ")


        return(lb)
      }}

  }


  Qmin_trend=function(data, station, mod=1) {
    library(ggplot2)
    min_trend=function(data, station, mod= 1) {


      data=data[[station]]
      val=data[,2]
      abs_min=min(val)
      #Minima der Jahre
      year_one=as.numeric(substring(as.character(data[1,1]),1,4))

      last_year=as.numeric(substring(as.character(data[length(val),1]),1,4))
      years=c(year_one:last_year)
      l=length(years)
      q_min=rep(0, l)
      for ( i in 1:l){
        year=as.character(years[i])
        j=grep(year, data[,1])
        Val=data[,2][j]
        q_min[i]=min(Val)
      }
      results=data.frame(years, q_min)

      if (mod == 3){


        model=lm(q_min ~ years, results)  #least squares.lm to fit a linear model.
        intercept_ls=as.numeric(model$coefficients[1])
        slope_ls=as.numeric(model$coefficients[2])
        llm=list(intercept_ls, slope_ls)
        names(llm)=c("intercept_lm", "slope_lm")
        return(llm)

      }else if (mod == 2){
        mod=zyp.trend.vector(y=results$q_min, x=years,  method="yuepilon")  #
        intercept_zyp=as.numeric(mod[11])
        slope_zyp=as.numeric(mod[2])
        sig_zyp=as.numeric(mod[6])
        lzyp= list(intercept_zyp, slope_zyp, sig_zyp)

        names(lzyp)=c("intercept_zyp", "slope_zyp","sig_zyp")


        return(lzyp)



      }else{


        model=lm(q_min ~ years, results)  #least squares.lm to fit a linear model.
        intercept_ls=as.numeric(model$coefficients[1])
        slope_ls=as.numeric(model$coefficients[2])

        mod=zyp.trend.vector(results$q_min, x=years, method="yuepilon")  #
        intercept_zyp=as.numeric(mod[11])
        slope_zyp=as.numeric(mod[2])
        sig_zyp=as.numeric(mod[6])
        lb= list(intercept_zyp, slope_zyp, sig_zyp, intercept_ls, slope_ls)

        names(lb)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm")


        return(lb)
      }


    }



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
      cap=paste('Slope: Trend- Sens Sloap:',round(model$slope_zyp,3),"Kendall's P-Value:", round(model$sig_zyp, 3),"Slope: Trend- Least Squares:", round(model$slope_lm,3))
      plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" ,  y=expression('Minimum Discharge Value [m'^3*'/s]'), caption=cap)+
        geom_abline(aes(intercept = model$intercept_zyp, slope= model$slope_zyp,  col="b"), show.legend=TRUE)+
        geom_abline(aes(intercept= model$intercept_lm, slope=model$slope_lm,col="c"), show.legend=TRUE)+  scale_color_manual(name = "Legend:   ",
                                                                                                                             labels=c("Minimum values", "Trend - Sens Sloap",
                                                                                                                                      "Trend-Least Squares"), values=c("a"="#F8766D","b"= "#00BDD0", "c"="darkblue"), guide="legend")+ theme(legend.position = "bottom" )

    }else if (mod==2){

      titl=paste("Yuepilon  Trend of Minimum Values at",station)
      cap=paste('Slope: Trend- Sens Sloap:',round(model$slope_zyp,3), "Kendall's P-Value:", round(model$sig_zyp, 3))
      plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" , y=expression('Minimum Discharge Value [m'^3*'/s]'), caption=cap)+
        geom_abline(aes(intercept = model$intercept_zyp, slope= model$slope_zyp,  col="b"), show.legend=TRUE)+
        scale_color_manual(name = "Legend:   ",
                           labels=c("Minimum values", "Trend - Sens Sloap"
                           ), values=c("a"="#F8766D","b"= "#00BDD0"), guide="legend")+ theme(legend.position = "bottom" )
    }else if(mod==3){

      titl=paste("Linear Trend of Minimum Values at",station)
      cap=paste('Slope: Trend- Least Squares:', round(model$slope_lm,3))
      plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" ,  y=expression('Minimum Discharge Value [m'^3*'/s]'), caption=cap)+
        geom_abline(aes(intercept= model$intercept_lm, slope=model$slope_lm,col="c"), show.legend=TRUE)+  scale_color_manual(name = "Legend:   ",
                                                                                                                             labels=c("Minimum values",
                                                                                                                                      "Trend-Least Squares"), values=c("a"="#F8766D", "c"="darkblue"), guide="legend")+ theme(legend.position = "bottom" )
    }




    return(plot)


  }








  # Introduction ------------------------------------------------------------



  query_modal <- modalDialog(
    title = "This Webapp uses the GRDC-dataset for a statistical low flow analysis in Germany. ",

    easyClose = F,
    footer = tagList(
      actionButton("start_window", "Start!")
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
                  title = "Legend",
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


  output$DB1 <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = trendpl(), device = "png", width=10, height=7)
    }
  )

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

      updateSliderInput(session, "year", label = "Year:",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "year2", label = "Year:",
                        min = sta_yea_cla, max = end_yea_cla)


      updateSliderInput(session, "yearq", label = "Year:",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "yearv", label = "Year:",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "tf1", label= "Zeitspanne 1:",
                        min = sta_yea_cla, max = end_yea_cla)


      updateSliderInput(session, "tf2", "Zeitspanne 2:",
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


      if(input$qplot_variety == "Discharge Hydrograph"){



          Qplot=Qplot(data2, stat_name, F)

        return(Qplot)

      }


      if(input$qplot_variety == "Rasterplot"){



        Rplot=Rasterplot(stat_name, data2)

        return(Rplot)

      }
      if(input$qplot_variety == "Annual Discharge Hydrograph"){

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
      if(input$qplot_variety == "Annual Boxplot"){


        Year=input$year
        qboxploty=QBoxploty(data=data2,  station=stat_name, year=Year, h=T)
        return(qboxploty)

      }
      if(input$qplot_variety == "Boxplot"){

        qboxplot=QBoxplot(data=data2,  station=stat_name)
        return(qboxplot)

      }



      if(input$qplot_variety == "Time Slice Analysis"){


        min1=input$tf1[1]

        max1=input$tf1[2]
        min2=input$tf2[1]
        max2=input$tf2[2]
       timeslice=timeslice(data=data2, station=stat_name,  min1, max1, min2, max2)

        return(timeslice)

      }


    }

    output$disch_plot <- renderPlot({t_plot()})

  output$DB <- downloadHandler(
      filename = function() { paste(input$dataset, '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = t_plot(), device = "png", width=10, height=7)
      }
    )


    if(input$qplot_variety == "Seasonal Plot"){

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


  output$DB2 <- downloadHandler(
      filename = function() { paste(input$dataset, '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = thres(), device = "png", width=10, height=7)
      }
    )




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

      updateSliderInput(session, "year", label = "Year:",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "year2", label = "Year:",
                        min = sta_yea_cla, max = end_yea_cla)



      updateSliderInput(session, "yearq", label = "Year:",
                        min = sta_yea_cla, max = end_yea_cla)

      updateSliderInput(session, "yearv", label = "Year:",
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


      if(input$qplot_variety == "Discharge Hydrograph"){

        Qplot=Qplot(data2, stat_name)
        return(Qplot)

      }
      if(input$qplot_variety == "Annual Discharge Hydrograph"){


        Year=input$year2
        qploty=Qploty(data2, stat_name, year=Year,h=T)
        return(qploty)

      }
      if(input$qplot_variety == "Annual Boxplot"){


        Year=input$year
        qboxploty=QBoxploty(data=data2,  station=stat_name, year=Year, h=T)
        return(qboxploty)

      }
      if(input$qplot_variety == "Boxplot"){

        qboxplot=QBoxplot(data=data2,  station=stat_name)
        return(qboxplot)

      }


    }

    output$disch_plot <- renderPlot({t_plot()})


    if(input$qplot_variety == "Seasonal Plot"){

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

    output$DB2 <- downloadHandler(

      filename = function() { paste(input$dataset, '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = thres(), device = "png", width=10, height=7)
      }
    )





  })#Observe Event Map/Marker/Table Marker Click finishes


























}









shinyApp(ui=ui, server=server)
#Frühlingfunctions abchecken
#slope ausformulieren in caption
#Achsenbeschriftung
#anders Runden, kleine Flüsse sehen nicht sinnvoll aus ........
# Input Dataset: Timerange and class of stations --------------------------

