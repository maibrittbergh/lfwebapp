


server= function(input, output, session){

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



          Qplot=Qplot(data2, stat_name, F)

        return(Qplot)

      }


      if(input$qplot_variety == "Rasterplot der Abflusswerte"){



        Rplot=Rasterplot(stat_name, data2)

        return(Rplot)

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


























}









shinyApp(ui=ui, server=server)
#Frühlingfunctions abchecken
#slope ausformulieren in caption
#Achsenbeschriftung
#anders Runden, kleine Flüsse sehen nicht sinnvoll aus ........
# Input Dataset: Timerange and class of stations --------------------------

