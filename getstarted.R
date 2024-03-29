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
#install.packages("shiny")
library(shiny)
#can i delete the other stuff?



# Load own dataset --------------------------------------------------------

path="/Users/maibrittberghofer/Desktop/Arbeit/GRDC_data/2022-11-02_10-59"
#metadata_ww=metadata_grdc(path=path, country_selection=F )
dataset_ww=lfanalyse::grdc_reader(metadata_ww, path="/Users/maibrittberghofer/Desktop/Arbeit/GRDC_DATA_DOWNLOAD/Downloaded_GRDC_DATA_13_06_23")

dataset_WW=sr
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



# Functions needed  -------------------------------------------------------

periodplot_quantile= function (data, station , quantile, year, graph=T){


  data=data[[station]]
  min=paste(as.character(year), "-11-01")
  min=sub(" -", "-",min)
  max=paste(as.character(year+1), "-10-31")
  max=sub(" -", "-",max)
  mindate=grep(min,data[,1])
  maxdate=grep(max,data[,1])
  Val=data[,2]
  U=quantile(Val, probs=quantile)
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
seasonpl=function(data, station, Startyear, Endyear, month_start, month_end){

  MQ_trend=function(data, station, seasonal="Y", graphic=T){


    datan=data[[station]]

    MEAN=mean(datan$Value)

    if (seasonal=="Y"){

      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){
        yearmin=years[i]
        yearmax=years[i+1]

        min=paste(yearmin,"-", "11-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        data=datan[start:end, ]
        MQ[i]=round(mean(data[,2],0))

      }

      results=data.frame(cbind(MQ, years[-l]))


      titl=paste("MQ- Trend within Hydrological Years at", station)
      subtitl=paste("Timeseries from", startyear, "to", endyear)
      model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

      linmod=lm(MQ~V2, results)
      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])

      if (graphic==T){
        #
        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                            "Slope: Trend Line- Least Squares:", round(linmod$coefficients[2],3) ),y=expression('MQ-Value [m'^3*'/s]'))+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("Mean Values", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
          theme(legend.position = "bottom" )

      }else{


        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
      }
      return(res)


    }else if (seasonal=="WI"){


      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){
        yearmin=years[i]
        yearmax=years[i+1]

        min=paste(yearmin,"-", "11-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "01-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        data=datan[start:end, ]
        MQ[i]=round(mean(data[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))



      titl=paste("MQ- Trend within Winter at", station)
      subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Nov, Dec, Jan")
      model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

      linmod=lm(MQ~V2, results)
      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl,caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                           "Slope: Trend Line- Least Squares:", round(linmod$coefficients[2],3) ),y=expression('MQ-Value [m'^3*'/s]'))+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("Mean Values", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
          theme(legend.position = "bottom" )


      }else{


        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
      }

      return(res)


    }else if (seasonal=="SP"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "02-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "04-30")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        data=datan[start:end, ]
        MQ[i]=round(mean(data[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))



      titl=paste("MQ- Trend within Spring at", station)
      subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Feb, Mar, Apr")
      model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

      linmod=lm(MQ~V2, results)
      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                            "Slope: Trend Line- Least Squares:", round(linmod$coefficients[2],3) ),y=expression('MQ-Value [m'^3*'/s]'))+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("Mean Values", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
          theme(legend.position = "bottom" )


      }else{


        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
      }

      return(res)


    }else if (seasonal=="SU"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "05-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "07-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        data=datan[start:end, ]
        MQ[i]=round(mean(data[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))



      titl=paste("MQ- Trend within Summer at", station)
      subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: May, June, July")
      model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

      linmod=lm(MQ~V2, results)
      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                            "Slope: Trend Line- Least Squares:", round(linmod$coefficients[2],3) ),y=expression('MQ-Value [m'^3*'/s]'))+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("Mean Values", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
          theme(legend.position = "bottom" )


      }else{


        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
      }

      return(res)


    }else if (seasonal=="AU"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "08-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        data=datan[start:end, ]
        MQ[i]=round(mean(data[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))



      titl=paste("MQ- Trend within Autumn at", station)
      subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Aug, Sep, Oct")
      model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

      linmod=lm(MQ~V2, results)
      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl,caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                           "Slope: Trend Line- Least Squares:", round(linmod$coefficients[2],3) ),y=expression('MQ-Value [m'^3*'/s]'))+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("Mean Values", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
          theme(legend.position = "bottom" )

      }else{


        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
      }

      return(res)


    }

  }

  seasonpl=function(data, station, Startyear, Endyear, month_start, month_end){



    # convert month -----------------------------------------------------------




    if (month_start==1){
      month_start="01"
    }
    if(month_end==1){
      month_end="01"
    }

    if (month_start==2){
      month_start="02"
    }
    if(month_end==2){
      month_end="02"
    }


    if (month_start==3){
      month_start="03"
    }
    if(month_end==3){
      month_end="03"
    }


    if (month_start==4){
      month_start="04"
    }
    if(month_end==4){
      month_end="04"
    }

    if (month_start==5){
      month_start="05"
    }
    if(month_end==5){
      month_end="05"
    }


    if (month_start==6){
      month_start="06"
    }
    if(month_end==6){
      month_end="06"
    }



    if (month_start==7){
      month_start="07"
    }
    if(month_end==7){
      month_end="07"
    }


    if (month_start==8){
      month_start="08"
    }
    if(month_end==8){
      month_end="08"
    }


    if (month_start==9){
      month_start="09"
    }
    if(month_end==9){
      month_end="09"
    }

    month_start=as.character(month_start)

    month_end=as.character(month_end)

    ########






    years=Startyear:Endyear
    years
    l=length(years)

    list2 =vector(mode = "list", length = l)

    nbr=which(names(data)==station)
    for ( t in 1:l){



      year=years[t]



      one=rep("0",2)
      two=rep("0", 10)

      for (i in 1:2){
        hydroyear1=c("11", "12" )
        one[i]=paste(year,"-",hydroyear1[i])
        one=sub(" - ", "-", one)

      }
      for (i in 1:10){
        hydroyear2=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10")
        two[i]=paste(year+1,"-",hydroyear2[i])
        two=sub(" - ", "-", two)
      }
      year_=c(one,two)

      Nov=grep(year_[1],data[[nbr]][,1] )
      Dec=grep(year_[2],data[[nbr]][,1] )
      Jan=grep(year_[3],data[[nbr]][,1] )
      Feb=grep(year_[4],data[[nbr]][,1] )
      Mar=grep(year_[5],data[[nbr]][,1] )
      April=grep(year_[6],data[[nbr]][,1] )
      May=grep(year_[7],data[[nbr]][,1] )
      June=grep(year_[8],data[[nbr]][,1] )
      July=grep(year_[9],data[[nbr]][,1] )
      August=grep(year_[10],data[[nbr]][,1] )
      Sep=grep(year_[11],data[[nbr]][,1] )
      Oct=grep(year_[12],data[[nbr]][,1] )




      j=c(Nov,Dec,Jan, Feb,Mar, April, May, June, July, August, Sep, Oct)

      datan=data[[nbr]][j,]
      datan$HydroYear=paste(year,"/", year+1)


      list2[[t]]= datan



    }


    list3 =vector(mode = "list", length = l)
    for ( b in 1:l){

      list2[[b]]$Month=format(list2[[b]][,1], "%m/%d")
      list2[[b]]$Year=substr(list2[[b]][,1],1,4)

      start=min(which(format(list2[[b]][,1], "%m")==month_start))
      end=max(which(format(list2[[b]][,1], "%m")==month_end))
      list3[[b]]=list2[[b]][start:end,]
      list3[[b]]$Number=1:length(start:end)

    }




    Result=bind_rows(list3, .id="HydroYear")

    for ( i in 1:l){

      Result$HydroYear[which(Result$HydroYear==i)]=paste(years[i],"/", years[i]+1)
    }




    graph= ggplot(Result, aes(x=Number, y=Value, group=HydroYear, col=HydroYear ))+geom_line()+
      labs(title= paste("Seasonal plot within the timespan of month" ,month_start, "-", month_end ),
           subtitle = paste ("During the years", Startyear, "to", Endyear, "at", station) ,
           colour="Year" )+ xlab("Day (within given timespan)")+ ylab(expression('Discharge Value [m'^3*'/s]'))




    return(graph)






  }

  NMxQ_trend=function(data, station, x, seasonal="Y", graphic=T){

    datan=data[[station]]
    l=nrow(datan)
    j=l-x
    NMXQ=rep(0,j)
    for ( i in 1:j){
      NMXQ[i]=mean(datan[i:(i+x), 2])

    }

    mNMXQ=mean(NMXQ)



    if (seasonal=="SP"){



      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      le=length(years)


      Nmxq=rep(0,le)

      for (i in 2:le){

        yearmax=years[i]

        min=paste(yearmax,"-", "02-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "04-30")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        datam=datan[start:end, ]


        l=nrow(datam)

        len=l-x

        for ( k in 1:len){
          Nmxq[i]=mean(datam[k:(k+x), 2])


        }
      }

      Nmxq=Nmxq[-1]
      results=data.frame(cbind(Nmxq, years[-1]))





      titl=paste("NMxQ- Trend. For x=", x, "at", station, "in Spring")
      subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Feb, Mar, Apr")
      model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

      linmod=lm(Nmxq~V2, results)
      slop=round(as.numeric(model[2]),4)
      sig=as.numeric(model[6])
      int=as.numeric(model[11])
      slope=round(linmod$coefficients[2], 4)

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                              "Slope: Trend Line- Least Squares:", round(slope,3) ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("NMxQ", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
          theme(legend.position = "bottom" )






      }else{

        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
      }

      return(res)



    }else if (seasonal=="SU"){



      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      le=length(years)


      Nmxq=rep(0,le)

      for (i in 2:le){

        yearmax=years[i]

        min=paste(yearmax,"-", "05-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "07-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        datam=datan[start:end, ]


        l=nrow(datam)

        len=l-x

        for ( k in 1:len){
          Nmxq[i]=mean(datam[k:(k+x), 2])


        }
      }
      Nmxq=Nmxq[-1]
      results=data.frame(cbind(Nmxq, years[-1]))
      results




      titl=paste("NMxQ- Trend. For x=", x, "at", station, "in Summer")
      subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: May, June, July")
      model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

      linmod=lm(Nmxq~V2, results)
      slop=round(as.numeric(model[2]),4)
      sig=as.numeric(model[6])
      int=as.numeric(model[11])
      slope=round(linmod$coefficients[2], 4)

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                              "Slope: Trend Line- Least Squares:", round(slope,3) ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("NMxQ", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
          theme(legend.position = "bottom" )






      }else{

        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
      }

      return(res)





    }else if (seasonal=="AU"){



      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      le=length(years)


      Nmxq=rep(0,le)

      for (i in 1:le){

        yearmax=years[i]

        min=paste(yearmax,"-", "08-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        datam=datan[start:end, ]


        l=nrow(datam)

        len=l-x

        for ( k in 1:len){
          Nmxq[i]=mean(datam[k:(k+x), 2])


        }
      }
      Nmxq=Nmxq[-1]

      results=data.frame(cbind(Nmxq, years[-1]))
      results




      titl=paste("NMxQ- Trend. For x=", x, "at", station, "in Autumn")
      subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Aug, Sep, Oct")
      model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

      linmod=lm(Nmxq~V2, results)
      slop=round(as.numeric(model[2]),4)
      sig=as.numeric(model[6])
      int=as.numeric(model[11])
      slope=round(linmod$coefficients[2], 4)

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                              "Slope: Trend Line- Least Squares:", round(slope,3) ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("NMxQ", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
          theme(legend.position = "bottom" )






      }else{

        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
      }

      return(res)


    }else if (seasonal=="WI"){



      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      le=length(years)

      la=le-1
      Nmxq=rep(0,la)

      for (i in 1:la){
        yearmin=years[i]
        yearmax=years[i+1]

        min=paste(yearmin,"-", "11-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "01-30")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        datam=datan[start:end, ]


        l=nrow(datam)

        len=l-x

        for ( k in 1:len){
          Nmxq[i]=mean(datam[k:(k+x), 2])


        }
      }

      results=data.frame(cbind(Nmxq, years[1:la]))





      titl=paste("NMxQ- Trend. For x=", x, "at", station, "in Winter")
      subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Nov, Dec, Jan")
      model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

      linmod=lm(Nmxq~V2, results)
      slop=round(as.numeric(model[2]),4)
      sig=as.numeric(model[6])
      int=as.numeric(model[11])
      slope=round(linmod$coefficients[2], 4)

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                              "Slope: Trend Line- Least Squares:", round(slope,3) ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("NMxQ", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
          theme(legend.position = "bottom" )






      }else{

        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
      }

      return(res)








    }else if (seasonal=="Y"){



      startyear=as.numeric(substr(datan[1,1],1,4))+1
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      le=length(years)

      la=le-1
      Nmxq=rep(0,la)

      for (i in 1:la){
        yearmin=years[i]
        yearmax=years[i+1]

        min=paste(yearmin,"-", "11-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        datam=datan[start:end, ]


        l=nrow(datam)

        len=l-x

        for ( k in 1:len){
          Nmxq[i]=mean(datam[k:(k+x), 2])


        }
      }

      results=data.frame(cbind(Nmxq, years[1:la]))





      titl=paste("NMxQ- Trend. For x=", x, "at", station)
      subtitl=paste("Timeseries from", startyear, "to", endyear)
      model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

      linmod=lm(Nmxq~V2, results)
      slop=round(as.numeric(model[2]),4)
      sig=as.numeric(model[6])
      int=as.numeric(model[11])
      slope=round(linmod$coefficients[2], 4)

      if (graphic==T){

        res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("Slope: Trend Line- Sens Sloap:",round(slop,3), "Kendall's P-Value:",    round(sig,3) ,
                                                                                                                                              "Slope: Trend Line- Least Squares:", round(slope,3) ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

          geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
          geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
          scale_color_manual(name = "Legend:   ",
                             labels=c("NMxQ", "Trend Line - Sens Sloap",
                                      "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
          theme(legend.position = "bottom" )






      }else{

        res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

        names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
      }

      return(res)



    }}
