#################################################################################################################
#Name:
#	GEOdb_CreadorTablas.r 
#Author:
#	Escobedo M. A. Sof and D. Monzalvo Andrea G.
#Version
#	v0.1
#Description
#	The script allows querying from organism name to obtain ...xd
#	Input parameters:
#		-O or --Org   			Name of the organism 
#		-p or --pathSQLiteFile	Path of SQLite file
#		-o or --outPath 		Path where the output files will be deposited 
#		-n or --nameDir			Name of the output directory
#		-d or --firstDown		Flag to first download of the SQLite file (if it is a path it'd be FALSE)
# 	Output
# 	1) rablas xd
#
# Examples:
# Rscript  GEOdb_CreadorTablas.r   -O  'Escherichia coli'  -p  /export/storage/users/aescobed/Systems/GEOdb/GEOmetadb.sqlite  -o  /export/storage/users/aescobed/Systems/GEOdb
#################################################################################################################

### Condicional banderita descarga primeriza con la opcion de path 

#Importacion de librerias 
library("GEOmetadb")
library(httr)
library(stringr)
library("argparse")

#Creacion de argumento
parser$add_argument("-O", "--Org", type="character", help="Pon tu puto organismo, no seas huevon >:v",
                    default="Escherichia coli")
parser$add_argument("-p", "--pathSQLiteFile", type= "character", help= 'El path de su archivo SQLite', default='GEOmetadb.sqlite')
args <- parser$parse_args()
print(args$Org)
### Falta poner los demás argumentos 

#Conection 
sqlfile <- file.path(args$pathSQLiteFile)
geo_con <- dbConnect(SQLite(), sqlfile)

#Funcion de consulta GSE-GPL
get_organism_GPL <- function(organism, geo_con){
  gPlataforms <-dbGetQuery(geo_con,paste("SELECT gpl FROM gpl WHERE organism='",organism,"'",sep="")) #Checar lo del paste
  gPlataforms <- as.vector(gPlataforms$gpl)
  return(gPlataforms)
}

get_organism_GSEbyGPL <- function(organism, geo_con){
  gPlataforms <- get_organism_GPL(organism, geo_con)
  gPlataforms <- gPlataforms[1:20]
  gSeries <- vector()
  for (gpl in gPlataforms){
    gse <- dbGetQuery(geo_con,paste("SELECT gse FROM gse_gpl WHERE gpl='",gpl,"'",sep=""))
    gSeries <- rbind(gSeries, gse)}
  gSeries <- gSeries$gse
  return (gSeries)
}


get_format_geo <- function(gsm_gpl){
    gpls<-names(gsm_gpl)
    gses<-rep(g,length(gsm_gpl))
    gsms<-sapply(gpls,function(x) paste(gsm_gpl[[x]],collapse="_"))
    samples<-sapply(gpls,function(x) length(gsm_gpl[[x]]))
    df <- data.frame(GSE = gses, GPL = gpls, GSM = gsms, Samples = samples)
    return (df) 
}


get_download_type <- function(g, geo_con){
 # GSE type search 
    gpl<-dbGetQuery(con,paste("SELECT gpl FROM gse_gpl WHERE gse='",g,"'",sep=""))
    if(dim(gpl)[1]==1){
      p<-"0"} 
    else{
      p<-"1"}
  
    ss<-dbGetQuery(con,paste("SELECT gse,overall_design FROM gse WHERE overall_design LIKE '%Refer%' AND gse='",g,"'",sep=""))
    if(dim(ss)[1]==1){
      s<-"B"} 
    else{
      s<-"A"}
  
    type<-paste(p,s,sep="")
    if(s=="B"){
      dl<-F
    } else{
      dl<-T
    }
    types<-rep(type,length(gsm_gpl))                     # GSE type
    dls<-rep(dl,length(gsm_gpl))                         # Boolean to download
  type_df <- data.frame(Type = types, BD = dls)
  return(type_df)
}
                   

get_sample_list <- function(g, geo_con){
  gsm<-dbGetQuery(geo_con,paste("SELECT gse_gsm.gse,gsm.gpl,gse_gsm.gsm FROM gse_gsm JOIN gsm WHERE gse_gsm.gse='",g,"' AND gse_gsm.gsm=gsm.gsm",sep=""))
  gsm_gpl<-tapply(gsm$gsm,gsm$gpl,function(x){x})
  return(gsm_gpl)
}

get_download_size <- function(gsm_gpl){
    gsizes<-c()
    for(l in gsm_gpl){
      gsize<-0
      for(m in l){
        link<-GET(paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",m,sep=""))
        tsize<-str_extract(content(link, 'text',encoding = "Latin1"), 'full table size <strong>\\d+')
        tsize<-strsplit(tsize,"<strong>")[[1]][2]
        gsize<-gsize+as.double(tsize)}
      gsizes<-c(gsizes,gsize)}
    return (gsizes)
}
                    
get_CDF <- function(gpls, geo_con){
  cdf_List <- vector()
  for (g in gpls) {
    cdf <- dbGetQuery(con,paste("SELECT supplementary_file  FROM gpl WHERE gpl='",g,"' AND supplementary_file LIKE '%cdf%'",sep=""))
    cdf_List <- c(cdf_List, cdf)}
  cdfs<-lapply(cdf_List, function(x) if(identical(x, character(0))) NA_character_ else x)
  return(cdfs)
}
                    
get_Bioconductor <- function(gpls,geo_con){
  biocon_List <- vector()
  for (g in gpls){
    biocon <- dbGetQuery(geo_con,paste("SELECT bioc_package FROM gpl WHERE gpl='",g,"'",sep=""))
    biocon_List <- c(biocon_List, biocon)
  }
  bios<-lapply(biocon_List, function(x) if(identical(x, character(0))) NA_character_ else x)
  return(bios)
}
                    
# Todavia falta xd
get_TaxID <- function(){}
                    
get_report_table <- function (gSeries, geo_con){
  format_Dfote <- data.frame()
  type_Dfote <- data.frame()
  size_Dfote <- data.frame()
  cdf_Dfote <- data.frame()
  biocon_Dfote <- data.frame()
  for (g in gSeries){
    gsm_gpl <- get_sample_list(g, geo_con)
    format_df <- get_format_geo(gsm_gpl)
    type_df <- get_download_type(g, geo_con)
    size_df <- get_download_size(gsm_gpl)
    cdf_df <- get_CDF(format_df$GPL, geo_con)
    biocon_df <- get_Bioconductor(format_df$GPL, geo_con)
  }
  report_Df <- cbind(format_Dfote, type_Dfote, size_Dfote, ) 
  return(report_Df)
}


