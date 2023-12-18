
metadata=metadata_ww
path="/Users/maibrittberghofer/Desktop/Arbeit/GRDC_DATA_DOWNLOAD/Downloaded_GRDC_DATA_13_06_23"

length(which(is.na(metadata_ww$startyear)==T))

metadata_WW=metadata_ww[-which(is.na(metadata_ww$startyear)==T),]
View(metadata_WW)

dataset_WW=lfanalyse::grdc_reader(metadata_WW, path=path)

grdc_reader=function(metadata, path){

  single_reader=function(metadata, path ){
    data_fluss= metadata #metadata[which(metadata$river==rivername),]
    grdc_no=data_fluss$grdc_no
    l=length(grdc_no)
    read=c(1:l)
    for (i in 1:l){

      grdc_numb=grdc_no[i]
      grdc_numb=as.character(grdc_numb)
      read[i]=paste(path,"/",grdc_numb,"_Q_Day.Cmd.txt")
    }
    read=sub(" ","", read)
    read=sub(" _","_", read)
    read=sub("/ ", "/", read)
    name= vector(mode = "list", length = l)

    for (i in 1:l){
      Tabelle=read.table(read[i], header=T, sep=";", dec=".", na.strings = "NA")[-2]#-999 als NA Value
      Tabelle$YYYY.MM.DD=as.Date(Tabelle$YYYY.MM.DD)
      Tabelle$Value[(which(Tabelle$Value<0))] = NA
      name[[i]]=Tabelle #hours,minutes rausgeschmissen
    }

    for (i in 1:l){
      names(name)[[i]]= data_fluss$station[i]
    }

    return(name)
  }
  sr=single_reader(metadata_WW, path)

  length(names(data2))
  for( i in 1: )

  length=nrow(metadata)
  grdc_list=vector(mode = "list", length = length)



  for ( i in 1:length){

    data=single_reader(metadata   , path)
    station=metadata$station[i]
    nbr=which(names(data)== station)
    val=data[[nbr]]



    grdc_list[[i]]=val
    names(grdc_list)=metadata$station

  }


  return(grdc_list)
}
