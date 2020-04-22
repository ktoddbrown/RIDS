#' Read in Rice 2018 
#'
#'  Reads in data from Charles Rice. 2018. OMB01 Microbial biomass in the Belowground Plot Experiment at Konza Prairie (1989-1999). LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-knz/53/7.
#'  
#'  ABSTRACT: The purpose of this data set is to observe long-term variations in microbial biomass in belowground plots at Konza Prairie. These effects are due to annual burning, mowing, and nitrogen and phosphorus fertilization.
#'
#' @param dataDir string that specifies the data directory
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' 
#' @export
#'
#'@importFrom EML read_eml
readRice2018 <- function(dataDir = 'data/Rice2018'){
  
  ##### Download data files ####
  
  datafiles.ls <- list(data = file.path(dataDir, 'data.csv'),
                       meta = file.path(dataDir, 'meta.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-knz%2F53%2F7',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-knz%2F53%2F7%2F2dc20cee0cbb72c2a3f46fc8d33eba76') 
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    try({
      download.file(url = download.ls$data, destfile = datafiles.ls$data, method="curl")
      download.file(url = download.ls$meta, destfile=datafiles.ls$meta, method = "curl")
    }
    )
  }
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-knz/53/7")
  }
  
  ##### Read in data ####
  
  Rice2018Data <- read_csv(datafiles.ls$data)
  Rice2018Meta <- EML::read_eml(datafiles.ls$meta)
  
  ##### Pull study information ####
  Rice2018.ls <- list(abstract = Rice2018Meta$dataset$abstract$section,
                      rights = Rice2018Meta$dataset$intellectualRights$section,
                      methods = Rice2018Meta$dataset$methods$methodStep$description$section)
  
  ##### Pull column names and units ####
  Rice.attr <- data.frame(name=as.character(rep(NA, length=17)),
                          description = as.character(rep(NA, length=17)),
                          unit = as.character(NA, length=17), stringsAsFactors = FALSE)
  for(columnIndex in 1:17){ #go through each column
    if(columnIndex %in% c(1:17)){
      Rice.attr[columnIndex, 'name'] <- Rice2018Meta$dataset$dataTable$attributeList$attribute[[columnIndex]]$attributeName
      
      Rice.attr[columnIndex, 'description'] <- Rice2018Meta$dataset$dataTable$attributeList$attribute[[columnIndex]]$attributeDefinition
      
    }
    if(columnIndex == 3 | columnIndex == 4 | columnIndex == 5){
      Rice.attr[columnIndex, 'unit'] <- Rice2018Meta$dataset$dataTable$attributeList$attribute[[columnIndex]]$measurementScale$dateTime$formatString
    }
    if(columnIndex == 1 | columnIndex == 2 | columnIndex > 5){
      #Rice.attr[columnIndex, 'unit'] <- Rice2018Meta$dataset$dataTable[[1]]$attributeList$attribute[[columnIndex]]$measurementScale$ratio$unit$standardUnit
      
      #leave Units blank
      
    } # ends if statement for columns 1, 2, >5
  }# ends for-loop for columns
  
  return(list(filenames = datafiles.ls,
              studyInfo = Rice2018.ls,
              meta = Rice.attr,
              data = Rice2018Data))
} # end function