funcion<-function(org,fs=FALSE){  
############################################################################################################
########## This function finds organism microarrays information (GSEs,GPLs,GSMs,Number of  #################
########## samples,R libraries for GPL,CDF)                                                #################
########## - org - target organism                                                         #################
########## - fs (default FALSE)- Boolean to download files size (This process is very time consuming)     #################
############################################################################################################

  # Libraries
  library("GEOmetadb")
  library(httr)
  library(stringr)
  
  # DataBase connection
  if( !file.exists("GEOmetadb.sqlite") ) {
    sqlfile <- getSQLiteFile()
  } else {
    sqlfile <- "GEOmetadb.sqlite"
  }
  con <- dbConnect(SQLite(), sqlfile)
  
  # List of organism target GSEs 
  gpl1<-dbGetQuery(con,paste("SELECT gpl FROM gpl WHERE organism='",org,"'",sep=""))
  gse1<-sapply(gpl1$gpl,function(x){dbGetQuery(con,paste("SELECT gse FROM gse_gpl WHERE gpl='",x,"'",sep=""))},simplify=T)
  gse<-unlist(gse1)
  
  # Capture Table
  if(fs){
    DF<-data.frame(GSE=NA,GPL=NA,GSM=NA,type=NA,size=NA,download=NA,samples=NA,Bioconductor=NA,CDF=NA)  
  } else {
    DF<-data.frame(GSE=NA,GPL=NA,GSM=NA,type=NA,download=NA,samples=NA,Bioconductor=NA,CDF=NA)
  }
  for(g in gse){
    
    # GSEs information search
    gsm<-dbGetQuery(con,paste("SELECT gse_gsm.gse,gsm.gpl,gse_gsm.gsm FROM gse_gsm JOIN gsm WHERE gse_gsm.gse='",g,"' AND gse_gsm.gsm=gsm.gsm",sep=""))
    gsm_gpl<-tapply(gsm$gsm,gsm$gpl,function(x){
      x
    })
    gpls<-names(gsm_gpl)                                                     # GPLs
    gses<-rep(g,length(gsm_gpl))                                             # GSEs
    gsms<-sapply(gpls,function(x) paste(gsm_gpl[[x]],collapse="_"))          # GSMs
    samples<-sapply(gpls,function(x) length(gsm_gpl[[x]]))                   # Number of samples per GPL
    
    # files size
    # There isn't this information from GEOmetadb library, we find this data with the httr library
    if(fs){
      gsizes<-c()
      for(l in gsm_gpl){
        gsize<-0
        for(m in l){
          link<-GET(paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",m,sep=""))
          tsize<-str_extract(content(link, 'text',encoding = "Latin1"), 'full table size <strong>\\d+')
          tsize<-strsplit(tsize,"<strong>")[[1]][2]
          gsize<-gsize+as.double(tsize)
          # Extraer Tax_id:
          # href="/Taxonomy/Browser/wwwtax.cgi?mode=Info&amp;id=562"
        }
        gsizes<-c(gsizes,gsize)
      }
    }
    
    # R Library search
    bios<-sapply(gpls,function(x) dbGetQuery(con,paste("SELECT bioc_package FROM gpl WHERE gpl='",x,"'",sep="")))
    bios<-lapply(bios, function(x) if(identical(x, character(0))) NA_character_ else x)
    
    # CDF search
    cdfs<-sapply(gpls,function(x) dbGetQuery(con,paste("SELECT supplementary_file  FROM gpl WHERE gpl='",x,"' AND supplementary_file LIKE '%cdf%'",sep="")))
    cdfs<-lapply(cdfs, function(x) if(identical(x, character(0))) NA_character_ else x)
    
    # GSE type search 
    gpl<-dbGetQuery(con,paste("SELECT gpl FROM gse_gpl WHERE gse='",g,"'",sep=""))
    if(dim(gpl)[1]==1){
      p<-"0"
    } else{
      p<-"1"
    }
    ss<-dbGetQuery(con,paste("SELECT gse,overall_design FROM gse WHERE overall_design LIKE '%Refer%' AND gse='",g,"'",sep=""))
    if(dim(ss)[1]==1){
      s<-"B"
    } else{
      s<-"A"
    }
    type<-paste(p,s,sep="")
    if(s=="B"){
      dl<-F
    } else{
      dl<-T
    }
    types<-rep(type,length(gsm_gpl))                     # GSE type
    dls<-rep(dl,length(gsm_gpl))                         # Boolean to download
    
    # Data Capture
    if(fs){
      DF<-rbind(DF,data.frame(GSE=gses,GPL=gpls,GSM=gsms,type=types,size=gsizes,download=dls,samples=samples,Bioconductor=unlist(bios),CDF=unlist(cdfs)))
    } else {
      DF<-rbind(DF,data.frame(GSE=gses,GPL=gpls,GSM=gsms,type=types,download=dls,samples=samples,Bioconductor=unlist(bios),CDF=unlist(cdfs)))
    }
  }
  DF<-DF[-1,]
  return(DF)
}


funcion(org="Staphylococcus aureus")

