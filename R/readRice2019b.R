#' Read in Rice 2019b 
#'
#'  Reads in data from Charles Rice and Lydia Zeglin. 2019. PBB03 Belowground Plot Experiment: Belowground plot experiment: biomass and nutrient content of Roots. urn:node:LTER. https://pasta.lternet.edu/package/metadata/eml/knb-lter-knz/59/8.
#'  
#'  ABSTRACT: This study is a part of the Long-Term Ecological Research (LTER) program at the Konza Prairie. In this study, the effects of fire, aboveground biomass removal, and nutrient changes on aboveground and belowground plots. The goals of this experiment are to document short- and long-term reponses of plants and soils to fire, biomass removal, and nutrient additions; to provide understanding concerning the response of tallgrass prairies to fire, aboveground biomass removal, and nutrient addition. Samples of live and dead grass roots were taken in the late summer from 64 belowground plots, and the N and P content were determined for both live and dead grass roots. Additional measurements of N and P for forb rhizomes are also available in certain years. 
#'
#' @param dataDir string that specifies the data directory
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' 
#' @examples 
#' readRice2019b(dataDir = 'data/Rice2019b')
#' 
#' @export
#'
#' @importFrom EML read_eml
#' @importFrom readr read_csv
readRice2019b <- function(dataDir = 'data/Rice2019b'){
  
  ##### Download data files ####
  
  datafiles.ls <- list(data = file.path(dataDir, 'data.csv'),
                       meta = file.path(dataDir, 'meta.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-knz%2F59%2F8',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-knz%2F59%2F8%2Fe7fb423b3d42747d542932ac3bc705ca') 
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    try({
      download.file(url = download.ls$data, destfile = datafiles.ls$data, method="curl")
      download.file(url = download.ls$meta, destfile=datafiles.ls$meta, method = "curl")
    }
    )
  }
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-knz/59/8")
  }
  
  ##### Read in data ####
  
  Rice2019bData <- read_csv(datafiles.ls$data)
  Rice2019bMeta <- EML::read_eml(datafiles.ls$meta)
  
  ##### Pull study information ####
  Rice2019b.ls <- list(abstract = Rice2019bMeta$dataset$abstract$section,
                      rights = Rice2019bMeta$dataset$intellectualRights$section,
                      methods = Rice2019bMeta$dataset$methods$methodStep$description$section)
  
  ##### Pull column names and units ####
  Rice.attr <- data.frame(name=as.character(rep(NA, length=16)),
                          description = as.character(rep(NA, length=16)),
                          unit = as.character(NA, length=16), stringsAsFactors = FALSE)
  for(columnIndex in 1:16){ #go through each column
    if(columnIndex %in% c(1:16)){
      Rice.attr[columnIndex, 'name'] <- Rice2019bMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$attributeName
      
      Rice.attr[columnIndex, 'description'] <- Rice2019bMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$attributeDefinition
      
    }
    if(columnIndex == 3 | columnIndex == 9 | columnIndex == 10 | columnIndex == 12 | columnIndex == 13 | columnIndex == 15 | columnIndex == 16){
      Rice.attr[columnIndex, 'unit'] <- Rice2019bMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$measurementScale$ratio$unit$customUnit
    }
    if(columnIndex == 1 | columnIndex == 6){
      
      #leave Units blank
      
    } # ends if statement for columns 1, 6
    if(columnIndex == 2 | columnIndex == 7 | columnIndex == 8 | columnIndex == 11 | columnIndex == 14){
      Rice.attr[columnIndex, 'unit'] <- Rice2019bMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$measurementScale$ratio$unit$standardUnit
    }
    if(columnIndex == 4 | columnIndex == 5){
      Rice.attr[columnIndex, 'unit'] <- Rice2019bMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$measurementScale$dateTime$formatString
    }
  }# ends for-loop for columns
  
  return(list(filenames = datafiles.ls,
              studyInfo = Rice2019b.ls,
              meta = Rice.attr,
              data = Rice2019bData))
} # end function