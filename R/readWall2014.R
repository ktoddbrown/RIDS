#' Read in Wall 2014 (soil carbon content in McMurdo Dry Valleys)
#'
#'  Reads in data from Diana Wall and Ross Virginia. 2014. McMurdo Dry Valleys Soil Elevational Transect Experiment. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-mcm/4003/5. 
#'  looks at soil carbon content in Antarctica, a continent with tundra climate. This data set studied the number of soil organisms (nematodes, rotifers and tardigrades), divided by species, sex and maturity was monitored at 3 elevations, initially in Taylor Valley (1993) then Garwood and Miers Valleys (2012). In this study, the organic and inorganic carbon content was also measured of the soils.
#'
#' @param dataDir string that specifies the data directory
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
#' @examples
#'  
#' @importFrom EML read_eml
readWall2014 <- function(dataDir = 'data/Wall2014'){
  
  ##### Download data files ####
  
  datafiles.ls <- list(soil_elevations = file.path(dataDir, 'soil_elevations.csv'),
                       meta = file.path(dataDir, 'metadata.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-mcm%2F4003%2F5',
                      soil_elevations = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-mcm%2F4003%2F5%2F4cb8f51882580e3dcaad294617cba14d')
                
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    try({
      download.file(url = download.ls$soil_elevations, destfile = datafiles.ls$soil_elevations)
      download.file(url = download.ls$meta, destfile=datafiles.ls$meta)
    }
    )
  }
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-mcm/4003/5")
  }
  
  ##### Read in data ####
  
  Wall_meta <- EML::read_eml(datafiles.ls$meta)
  soil_elevations.df <- read_csv(datafiles.ls$soil_elevations, skip = 27)
  
  ##### Pull study information ####
  WallStudy.ls <- list(abstract = Wall_meta$dataset$abstract$section,
                       rights = Wall_meta$dataset$intellectualRights$section,
                       methods = Wall_meta$dataset$methods$methodStep$description$section)
  

  
  ##### Pull column names and units ####
  Wall_attribute <- pullAttributes(eml_filename = datafiles.ls$meta)
  
  return(list(filenames = datafiles.ls,
              studyInfo = WallStudy.ls,
              meta = Wall_attribute,
              data1 = soil_elevations.df))
  
} # end function