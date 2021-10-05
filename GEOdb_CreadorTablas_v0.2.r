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


funcionChida<-function(geo_con,organism){
  
  query<-paste("SELECT gpl.bioc_package, gpl.supplementary_file, gse_gpl.gse,gpl.gpl,gse_gsm.gsm FROM gpl JOIN gse_gpl,gse_gsm WHERE gpl.organism= '",organism,"' AND gse_gsm.gse=gse_gpl.gse AND gse_gpl.gpl=gpl.gpl",sep="")
  tabla_iden<-dbGetQuery(geo_con,query)
  return(tabla_iden)
}
#Ejemplo 
#table<- funcionChida(geo_con,"Staphylococcus aureus")
#table[table$gpl=="GPL17764",]


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
                    
                    
# Todavia falta xd
#Hay que juntar la tabla de tipos con la de consulta

get_TaxID <- function(){}
                    
get_report_table <- function (gSeries, geo_con){
  type_Dfote <- data.frame()
  size_Dfote <- data.frame()
  
  for (g in gSeries){
    type_df <- get_download_type(g, geo_con)
    size_df <- get_download_size(gsm_gpl)
  
  }
  report_Df <- cbind(type_Dfote, size_Dfote, ) 
  return(report_Df)
}
