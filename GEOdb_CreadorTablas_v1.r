#################################################################################################################
#Name:
#	GEOdb_CreadorTablas.r 
#Author:
#	Escobedo M. A. Sof and D. Monzalvo Andrea G.
#Version
#	v1
#Description
#	The script allows querying from organism name to obtain ...xd
#	Input parameters:
#		-O or --Org   			Name of the organism 
#		-p or --pathSQLiteFile	Path of SQLite file
#		-o or --outPath 		Path where the output files will be deposited 
#		-n or --nameDir			Name of the output file
# 	Output
# 	1) tablas xd
#
# Examples:
# Rscript  /export/storage/users/aescobed/Systems/GEOdb/bin/GEOdb_CreadorTablas_v1.r   -O  'Escherichia coli'  -p  /export/storage/users/aescobed/Systems/GEOdb/input/GEOmetadb.sqlite  -o  /export/storage/users/aescobed/Systems/GEOdb/output  -n  metadata_Ecoli.tsv
#################################################################################################################
message('====================================================================================================================')
message('\t\t\t\t\t|-|-|-|-|-|-|\tWELCOME TO GEOdbQuery\t|-|-|-|-|-|-|')

# Importacion de librerias 
message('\t\t\tSTEP 1: Importing libraries...')
suppressPackageStartupMessages(library("GEOmetadb"))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library('stringr'))
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("reshape2"))
suppressPackageStartupMessages(library("plyr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library('progress'))
message('\t\t\tDone!\n')

################################################## FUNCTIONS ####################################################
# Funcion 1: consulta de metadatos directamente del SQLite
funcionChidota<-function(geo_con,organism){
	query<-paste("SELECT gpl.organism,gpl.bioc_package, gpl.supplementary_file, gpl.technology, gpl.distribution, gpl.manufacturer, gse_gpl.gse, gpl.gpl, gse_gsm.gsm, gse.overall_design FROM gpl JOIN gse_gpl, gse_gsm, gse WHERE gpl.organism= '",organism,"' AND gse_gsm.gse=gse_gpl.gse AND gse_gpl.gpl=gpl.gpl AND gse.gse=gse_gpl.gse",sep="")
	tabla_iden<-dbGetQuery(geo_con,query)
	return(tabla_iden)
}

# Funcion de apoyo 1: obtener numero de plataforma segun un gse
getMultiPlatCateSupport <- function(x, tableFull){
	lele <- length(unique(filter(tableFull, gse == x)$gpl))
	return(lele)
}

# Funcion 2: obtener metadata de descarga, tipo de descarga y booleano de descarga
getDownloadTypeModTable <- function(tableFC){
	uniqGSE <- unique(tableFC$gse)
	lenVecPlat <- sapply(uniqGSE, getMultiPlatCateSupport , tableFull = tableFC)
	columnCatPlat <- mapvalues(tableFC$gse, from=  names(lenVecPlat), to = as.numeric(as.vector(lenVecPlat)))
	tableFCnew <- tableFC %>% mutate(gseCatNP = ifelse(columnCatPlat == 1, 0, 1), gseSuSe = ifelse(grepl(x=tableFC$overall_design, pattern= 'Refer to individual Series'), 'B', 'A'))
	tableFCfinal <- unite(tableFCnew, col = 'download_type', gseCatNP:gseSuSe, remove= TRUE, sep='')
	tableFCfinal <- tableFCfinal %>% mutate(download_bool = ifelse(tableFCfinal$download_type %in% c('1A', '0A'), TRUE, FALSE))
	return(tableFCfinal)
}

# Funcion de apoyo 2: obtener tamaños segun html
getSizeDownload <- function(gID){
	link<-GET(paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",gID,sep=""))
	matchSize <- str_extract(content(link, 'text',encoding = "Latin1"), 'full table size <strong>\\d+')
	size <- unlist(strsplit(matchSize,"<strong>"))[2]
	row <- data.frame(ID= gID, download_size= size)
	return(row)
}

# Funcion 3: obtener tamaños de los gsm de un table
createGsmDataSize <- function(tableData){
	dfSamples <- data.frame()
	gSamples <- unique(tableData$gsm)
	for (gSample in gSamples){
		rowSam <- getSizeDownload(gSample)
		dfSamples <- rbind(dfSamples, rowSam)}
	names(dfSamples) <- c('gsm', 'download_size_gsm')
	return(dfSamples)
}

# Funcion de apoyo 3: obtener taxID segun html
getTaxID <- function(gID){
	link<-GET(paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",gID,sep=""))
	matchTaxId <- str_extract(content(link, 'text',encoding = "Latin1"), '<a href="/Taxonomy/.*onmouseout')
	taxId <- unlist(strsplit(str_remove(matchTaxId, '^.*id='), '" '))[1]
	taxID <- as.double(taxId)
	row <- data.frame(ID= gID, taxon_id = taxID)
	return(row)
}

# Funcion 4: obtener comparacion de taxID de gsm y gpl, sacar taxID de ambos
createGsmGplDataTaxIDComp <- function(tableData){
	dfSamples <- data.frame()
	dfPlatforms <- data.frame()
	subsetData <- data.frame(gsm= tableData$gsm, gpl = tableData$gpl)
	gPlatforms <- unique(tableData$gpl)
	gSamples <- unique(tableData$gsm)
	for (gSample in gSamples){
		rowSam <- getTaxID(gSample)
		dfSamples <- rbind(dfSamples, rowSam)}
	names(dfSamples) <- c('gsm', 'taxon_id_gsm')
	for (gPlatform in gPlatforms){
		rowPlat <- getTaxID(gPlatform)
		dfPlatforms <- rbind(dfPlatforms, rowPlat)}
	names(dfPlatforms) <- c('gpl', 'taxon_id_gpl')
	df <- subsetData %>% left_join(dfSamples, by= 'gsm') %>% left_join(dfPlatforms, by= 'gpl')
	dfFinal <- df %>% mutate(comparation_tax_id = ifelse(taxon_id_gsm == taxon_id_gpl, 'T', 'F'))
	dfFinal <- dfFinal %>% select(-gpl)
	return(dfFinal)
}

# Funcion 5: une todas las fases y las reporta, regresa el metadata final
getReportTable <- function (organism, geo_con){
	message('\t\t\t\tSTEP 4.1: Making consult from SQLite and filtering...')
	tableDataOrg <- funcionChidota(geo_con,organism) %>% filter( manufacturer %in% c('Affymetrix'))
	message('\t\t\t\tDone!\n')
	message('\t\t\t\tSTEP 4.2: Creating download metadata and boolean categories...')
	downloadMetaData <- getDownloadTypeModTable(tableDataOrg)
	message('\t\t\t\tDone!\n')
	message('\t\t\t\tSTEP 4.3: Getting gsm/gpl taxID, comparing them and creating dataframe...')
	taxIDData <- createGsmGplDataTaxIDComp(downloadMetaData)
	message('\t\t\t\tDone!\n')
	message('\t\t\t\tSTEP 4.4: Getting gsm size and creating dataframe')
	sizeData <- createGsmDataSize(downloadMetaData)
	message('\t\t\t\tDone!\n')
	message('\t\t\t\tSTEP 4.5: Uniting taxID and size dataframes to the principal metadata table')
	preReportDf <- downloadMetaData %>% left_join(taxIDData, by= 'gsm') %>% left_join(sizeData, by= 'gsm')
	message('\t\t\t\tDone!\n')
	message('\t\t\t\tSTEP 4.6: Making categories using distribution and bioc_package status')
	reportDf <- preReportDf %>% mutate(bioconductor_disagree = ifelse(distribution %in% c('commercial') & bioc_package %in% c(NA), 'T', 'F'))
	message('\t\t\t\tDone!\n')
	return(reportDf)
}

################################################## MAIN CODE ##########################################################

# Inicio de toma de tiempo
start <- Sys.time()

# Creacion de argumento
message('\t\t\tSTEP 2: Creating arguments...')
parser <- ArgumentParser()
parser$add_argument("-O", "--Org", type="character", help="Pon tu puto organismo, no seas huevon >:v", default="Escherichia coli")
parser$add_argument("-p", "--pathSQLiteFile", type= "character", help= 'El path de su archivo SQLite', default='GEOmetadb.sqlite')
parser$add_argument("-o", "--outPath", type= "character", help="Patas de salida", default= getwd())
parser$add_argument("-n", "--nameOutFile", type= "character", help="Archivo de salida", default= paste('metaTableData', paste(Sys.Date(), '/', sep =''),sep= '_'))
args <- parser$parse_args()
message('\t\t\tDone!\n')

# Conection 
message('\t\t\tSTEP 3: Creating connection to SQLite...')
sqlfile <- file.path(args$pathSQLiteFile)
geo_con <- dbConnect(SQLite(), sqlfile)
message('\t\t\tDone!\n')

# Crear metadata para organismo
message(paste('\t\t\tSTEP 4: Processing metadata of', args$Org, '...'))
metadata <- getReportTable(args$Org, geo_con)
message('\t\t\tDone!\n')

# Guardar metadata
message('\t\t\tSTEP 5: Saving metadata results...')
outputFinal <- paste(args$outPath, args$nameOutFile, sep = '/')
write.table(x= metadata, file= outputFinal, sep= '\t', quote = FALSE, col.names= TRUE, row.names= FALSE, na = "NA")
message('\t\t\tDone!\n')

end <- Sys.time()
timeTaken <- end - start
timeTaken
message('\t\t\t\t\t|-|-|-|-|-|-|\tThanks for using GEOdbQuery! Have a nice day!\t|-|-|-|-|-|-|')
message('====================================================================================================================')