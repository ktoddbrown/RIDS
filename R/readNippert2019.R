#' Reads in Nippert 2019 (soil moisture with depth)
#'
#' Reads in data from Jesse Nippert. 2019. ASM01 Soil water content measured by neutron probe at Konza Prairie. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-knz/11/13.
#'
#' @param dataDir string that specifies data directory
#' @param verbose boolean flag to print out useful debugging statements
#'
#' @return A list that contains information including abstract, license, methods, data tables, and meta data for the data set
#'
#' @export
#'
#' @importFrom EML read_eml
#' @importFrom  readr read_csv
readNippert2019 <- function(dataDir = 'data/Nippert2019' , verbose = FALSE){

  ##### Download data files #####


  datafiles.ls <- list(data = file.path(dataDir, 'data.csv'),
                       package = file.path(dataDir, 'packageReport.xml'),
                       meta = file.path(dataDir, 'metadata.xml'))

  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-knz%2F11%2F13',
                      package = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Freport%2Feml%2Fknb-lter-knz%2F11%2F13',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-knz%2F11%2F13%2F75363a8b27cddd6b65f859f09686c889')

  if(!all(file.exists(unlist(datafiles.ls)))){
    downloadFilefun <- function(){
      download.file(url = download.ls$data, destfile = datafiles.ls$data)
      download.file(url = download.ls$package, destfile = datafiles.ls$package)
      download.file(url = download.ls$meta, destfile = datafiles.ls$meta)
      return(NULL)
    }
    tryCatch(downloadFilefun(), error = function(e) e)
  }else{
    print('Files downloaded...moving on')
  }
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop('Go download the data files manually')
  }


  ##### Read in the data #####
  Nippert.df <- read_csv('data/Nippert2019/data.csv')
  NippertMeta <- EML::read_eml('data/Nippert2019/metadata.xml')

  ##### parse the metadata #####

  NippertStudy.ls <- list(abstract = NippertMeta$dataset$abstract$section,
                            rights = NippertMeta$dataset$intellectualRights$section,
                            methods = NippertMeta$dataset$methods$methodStep$description$section)

  ##### pull Attributes #####
  Nippert_attribute <- data.frame(name=as.character(rep(NA, length=16)),
                                  description = as.character(rep(NA, length=16)),
                                  unit = as.character(NA, length=16), stringsAsFactors = FALSE)

  for(columnIndex in 1:16){ #go through each column

    attribute.ls <- NippertMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]

    #names and descriptions are easy to pull
    Nippert_attribute[columnIndex,1:2] <- c(attribute.ls$attributeName,
                                            attribute.ls$attributeDefinition)

    #Units are a bit harder
    if(columnIndex == 1 | columnIndex == 2 | columnIndex == 6 | columnIndex == 16){
      Nippert_attribute[columnIndex, 3] <- attribute.ls$measurementScale$nominal$nonNumericDomain$textDomain

    }else if(columnIndex == 3 | columnIndex == 8 | columnIndex == 9 | columnIndex == 10 | columnIndex ==  11 | columnIndex == 12 | columnIndex == 13 | columnIndex ==  14 | columnIndex == 15){
      Nippert_attribute[columnIndex, 3] <- attribute.ls$measurementScale$ratio$unit$customUnit

    }else if(columnIndex == 4 | columnIndex ==  5){
      Nippert_attribute[columnIndex, 3] <- attribute.ls$measurementScale$dateTime$formatString

    }else if(columnIndex == 7){
      Nippert_attribute[columnIndex, 3] <- attribute.ls$measurementScale$ratio$unit$standardUnit

    }
  }

  #return the data files and the metadata and file path to data
  return(list(filenames = datafiles.ls,
              meta = Nippert_attribute,
              studyInfo = NippertStudy.ls,
              data = Nippert.df))




}
