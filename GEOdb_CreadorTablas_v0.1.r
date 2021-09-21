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

get_sample_list <- function (gSeries, geo_con){
  

}
