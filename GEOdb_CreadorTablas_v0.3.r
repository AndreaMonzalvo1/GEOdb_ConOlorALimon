#################################################################################################################
#Name:
#	GEOdb_CreadorTablas.r 
#Author:
#	Escobedo M. A. Sof and D. Monzalvo Andrea G.
#Version
#	v0.3
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

#Funcion de consulta 
funcionChidota<-function(geo_con,organism){
	query<-paste("SELECT gpl.bioc_package, gpl.supplementary_file, gse_gpl.gse,gpl.gpl,gse_gsm.gsm,gse.overall_design FROM gpl JOIN gse_gpl,gse_gsm,gse WHERE gpl.organism= '",organism,"' AND gse_gsm.gse=gse_gpl.gse AND gse_gpl.gpl=gpl.gpl AND gse.gse=gse_gpl.gse",sep="")
	tabla_iden<-dbGetQuery(geo_con,query)
	return(tabla_iden)
}

getMultiPlatCateSupport <- function(x, tableFull){
	lele <- length(unique(filter(tableFull, gse == x)$gpl))
	return(lele)
}

getDownloadTypeModTable <- function(tableFC){
	uniqGSE <- unique(tableFC$gse)
	lenVecPlat <- sapply(uniqGSE, getMultiPlatCateSupport , tableFull = tableFC)
	columnCatPlat <- mapvalues(tableFC$gse, from=  names(lenVecPlat), to = as.numeric(as.vector(lenVecPlat)))
	tableFCnew <- tableFC %>% mutate(gseCatNP = ifelse(columnCatPlat == 1, 0, 1), gseSuSe = ifelse(grepl(x=tableFC$overall_design, pattern= 'Refer'), 'B', 'A'))
	tableFCfinal <- unite(tableFCnew, col = 'download_type', gseCatNP:gseSuSe, remove= TRUE, sep='')
	tableFCfinal <- tableFCfinal %>% mutate(download_bool = ifelse(tableFCfinal$download_type %in% c('1A', '0A'), TRUE, FALSE))
	return(tableFCfinal)
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

get_TaxID <- function(){}

getReportTable <- function (..., geo_con){
	... #faltan taxID y size
	return(reportDf)
}
