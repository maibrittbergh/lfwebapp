#lfanalyse basics

metadata_repg=function(metadata, mark=F){

  relstat=c("HOHENSAATEN-FINOW", "DRESDEN", "MAGDEBURG-STROMBRUECKE",
            "RATHENOW UP", "CALBE-GRIZEHNE", "INTSCHEDE",  "HANN.-MUENDEN", "VLOTHO",
            "VERSEN-WEHRDURCHSTICH", "GREVEN", "MAXAU", "KAUB", "KOELN", "COCHEM", "WUERZBURG" ,
            "ROCKENAU SKA", "ACHLEITEN", "BURGHAUSEN", "WASSERBURG", "LANDSBERG", "KEMPTEN")
  if(mark==F){
    #relevante Stationen
    l=length(relstat)
    rows=rep(0,l)
    for ( i in 1:l){

      rows[i]=which(metadata$station==relstat[i])
    }



    metadata_repg=metadata[rows,]
    return(metadata_repg)}
  #(von Ost nach West):
  # Ratzdorf / Oder: nicht gefunden  gab es nicht, habe "HOHENSAATEN-FINOW" genommen , oder ist "EISENHUETTENSTADT" besser?
  #Nienburg / Saale  #Nienburg garb es nicht. Habe CALBE-GRIZEHNE genommen
  #Höxter; / Weser #höxter gab es nicht, habe "HANN.-MUENDEN", "VLOTHO" genommen
  #Lingen-Darme / Ems # gab es nicht habe "VERSEN-WEHRDURCHSTICH", "GREVEN" genommen

  #Neu Ulm, Achleiten/ Donau #neu ulm gibts nicht


  if(mark==T){

    l=length(relstat)
    rows=rep(0,l)
    for ( i in 1:l){

      rows[i]=which(metadata$station==relstat[i])
    }



    representative=rep(F,nrow(metadata))
    representative[rows]=T
    metadata$rep_stat=representative
    return(metadata)
  }



}


