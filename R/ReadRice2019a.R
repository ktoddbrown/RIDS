  #' Read in Rice 2019a 
  #'
  #'  Reads in data from Charles Rice and Lydia Zeglin. 2019. PBB02 Belowground Plot Experiment: Biomass and nutrient content of Rhizomes. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-knz/58/8.
  #'  
  #'  ABSTRACT: This study is a part of the Long-Term Ecological Research (LTER) program at the Konza Prairie. In this study, the effects of fire, aboveground biomass removal, and nutrient changes on aboveground and belowground plots. The goals of this experiment are to document short- and long-term reponses of plants and soils to fire, biomass removal, and nutrient additions; to provide understanding concerning the response of tallgrass prairies to fire, aboveground biomass removal, and nutrient addition. Samples of live and dead rhizomes were taken in the late summer from 64 belowground plots, and the N and P content were determined for both live and dead rhizomes. Additional measurements of N and P for forb rhizomes are also available in certain years.
  #'
  #' @param dataDir string that specifies the data directory
  #'
  #' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
  #' 
  #' @examples 
  #' readRice2019a(dataDir = 'data/Rice2019a)
  #' 
  #' @export
  #'
  #' @importFrom EML read_eml
  #' @importFrom readr read_csv
  readRice2019a <- function(dataDir = 'data/Rice2019a'){
    
    ##### Download data files ####
    
    datafiles.ls <- list(data = file.path(dataDir, 'data.csv'),
                         meta = file.path(dataDir, 'meta.xml')) 
    
    download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-knz%2F58%2F8',
                        data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-knz%2F58%2F8%2F74d39d0611c275e9d5c02d6e47286b38') 
    
    if(!all(file.exists(unlist(datafiles.ls)))){
      try({
        download.file(url = download.ls$data, destfile = datafiles.ls$data, method = "curl")
        download.file(url = download.ls$meta, destfile=datafiles.ls$meta, method = "curl")
      }
      )
    }
    if(!all(file.exists(unlist(datafiles.ls)))){
      stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-knz/58/8")
    }
    
    ##### Read in data ####
    
    Rice2019aData <- read_csv(datafiles.ls$data)
    Rice2019aMeta <- EML::read_eml(datafiles.ls$meta)
    
    ##### Pull study information ####
    Rice2019a.ls <- list(abstract = Rice2019aMeta$dataset$abstract$section,
                        rights = Rice2019aMeta$dataset$intellectualRights$section,
                        methods = Rice2019aMeta$dataset$methods$methodStep$description$section)
    
    ##### Pull column names and units ####
    Rice.attr <- data.frame(name=as.character(rep(NA, length=16)),
                            description = as.character(rep(NA, length=16)),
                            unit = as.character(NA, length=16), stringsAsFactors = FALSE)
    Rice.attr <- data.frame(name=as.character(rep(NA, length=16)),
                            description = as.character(rep(NA, length=16)),
                            unit = as.character(NA, length=16), stringsAsFactors = FALSE)
    for(columnIndex in 1:16){ #go through each column
      if(columnIndex %in% c(1:16)){
        Rice.attr[columnIndex, 'name'] <- Rice2019aMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$attributeName
        
        Rice.attr[columnIndex, 'description'] <- Rice2019aMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$attributeDefinition
        
      }
      if(columnIndex == 3 | columnIndex == 9 | columnIndex == 10 | columnIndex == 12 | columnIndex == 13 | columnIndex == 15 | columnIndex == 16){
        Rice.attr[columnIndex, 'unit'] <- Rice2019aMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$measurementScale$ratio$unit$customUnit
      }
      if(columnIndex == 1 | columnIndex == 6){
        
        #leave Units blank
        
      } # ends if statement for columns 1, 6
      if(columnIndex == 2 | columnIndex == 7 | columnIndex == 8 | columnIndex == 11 | columnIndex == 14){
        Rice.attr[columnIndex, 'unit'] <- Rice2019aMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$measurementScale$ratio$unit$standardUnit
      }
      if(columnIndex == 4 | columnIndex == 5){
        Rice.attr[columnIndex, 'unit'] <- Rice2019aMeta$dataset$dataTable$attributeList$attribute[[columnIndex]]$measurementScale$dateTime$formatString
      }
    }# ends for-loop for columns
    
    return(list(filenames = datafiles.ls,
                studyInfo = Rice2019a.ls,
                meta = Rice.attr,
                data = Rice2019aData))
  } # end function