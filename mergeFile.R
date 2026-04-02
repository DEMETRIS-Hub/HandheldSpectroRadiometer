# **SpectroRadiometer file merging script**
# 
# This script take all the .sed files from a directory and create a unique table 
# with all the reflectance values inside and metadata. 
# Moreover this script extract from the filename user inserted information 
# considering the characters before the first "_" and the filenumber extracting
# the digit between the last "_" and the extension (default Spectral Evolution 
# setting). 
# Further information may be extracted from user inserted information using 
# regexp 
#
# All the script parameter are written in a yaml 
#
# Author: André Fabbri 
# E-mail: andre.fabbri@cnr.it
# Version: 1.0
# License: CC-BY-SA



if (!require(spectrolab)) install.packages(spectrolab)
library(spectrolab)## for read_spectra function
if (!require(dplyr)) install.packages(dplyr)
library(dplyr) ## for tibble manipulation
if (!require(openxlsx)) install.packages(openxlsx)
library(openxlsx) ## for write.xlsx function
if (!require(stringr)) install.packages(stringr)
library(stringr)## for string operation str_extract str_split
if (!require(yaml)) install.packages(yaml)
library(yaml) ##read_yaml


## load config file

### argument processing for config filename 
args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  p_configFile <- "./config.yml"
}else{
  p_configFile <- args[1]
}
### load config file 
configSpectroRadiometer <-  read_yaml(p_configFile)

### script parameter setting
workingDir <- configSpectroRadiometer$workingDir
destSubDir <- configSpectroRadiometer$destSubDir
spectralEvolutionMetaData <- configSpectroRadiometer$spectralEvolutionMetaData
outputFile <- configSpectroRadiometer$outputFile
areaUserIdReg= configSpectroRadiometer$areaUserIdRegEx
plantUserIdReg=configSpectroRadiometer$planUserIdRegEx
treatmentUserIdReg = configSpectroRadiometer$treatmentUserIdRegEx
genotypeUserIdReg = configSpectroRadiometer$genotypeUserIdRegEx
destDir <- paste0(workingDir,"/",destSubDir)


## util function for information extraction from the filename
### extract User inserted value from the filename 
extractUserInsertedIdFromFileName <- function(name) {
  firstPart <- do.call(rbind,str_split(name, "_"))[,1]
  return(firstPart)
}
### extract file number from filename 
extractFileNumberFromFileName <- function(name) {
  listSplit <- str_split(do.call(rbind,str_split(name, "\\."))[,1],"_")
  lastPart <- do.call(rbind,lapply(X = listSplit,FUN = tail, n=1 ))[,1]
  return(lastPart)
}

## load data 
sp <- read_spectra(path=workingDir, format = "sed",extract_metadata = TRUE)
sp_value <- as_tibble(as.matrix(sp))
sp_meta <- as_tibble(sp$meta) %>% select(all_of(spectralEvolutionMetaData))

## combine Filename, metadata and value tibble 
dataset <- cbind(FileName = sp$name, sp_meta, sp_value) %>% 
  ## + extract FileUserID and FileNumber from FileName 
  mutate(FileUserID = extractUserInsertedIdFromFileName(FileName), 
         FileNumber = as.numeric(extractFileNumberFromFileName(FileName)),
         .after=FileName)
## extract values form file name FileUserID
dataset <- dataset %>% 
  mutate( areaID =  "",
          plantID = "",
          treatment = "",
          genotype = "", 
          .after=FileNumber
  )
### area 
if(!is.null(areaUserIdReg)){
  dataset <- dataset %>% 
    mutate( areaID = str_extract(string = FileUserID,pattern = areaUserIdReg))
}  
### plant ID
if(!is.null(plantUserIdReg)){
  dataset <- dataset %>% 
    mutate( plantID = str_extract(string = FileUserID,pattern = plantUserIdReg))
}
### genotype
if(!is.null(genotypeUserIdReg)){
  dataset <- dataset %>% 
    mutate( genotype = str_extract(string = dataset$FileUserID,pattern = genotypeUserIdReg))
}
### treatment
if(!is.null(treatmentUserIdReg)){
  dataset <- dataset %>% 
    mutate( treatment = str_extract(string = dataset$FileUserID,pattern = treatmentUserIdReg))
}

## transpose tibble for the writing 
dataset_t <-  as_tibble(t(dataset)) 
colnames(dataset_t) <- sp$names
dataset_t <- dataset_t %>% mutate(row_names=colnames(dataset)) %>% select (row_names, everything())


## write the tibble in the file 
### create directory if necessary
ifelse(!dir.exists(file.path(destDir)),
       dir.create(file.path(destDir)),
       "Dest directory already exists")
### write file
#write.xlsx(dataset,file = paste0(destDir,"/transposed_", outputFile))
write.xlsx(dataset_t,file = paste0(destDir,"/", outputFile))
