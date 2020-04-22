#' Read in Brown 2013 (soil carbon content in Yukon River Basin Fire and Permafrost Study)
#'
#'  Reads in data from Dana Rachel Brown and Bonanza Creek LTER. 2013. Yukon River Basin Fire and Permafrost Study: Soil physical and chemical characteristics by soil layer and year (2009-2012). LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-bnz/549/4.
#'  This data set looks at soil carbon content from Alaska, with a tundra climate. This data set examined the effects of fire on permafrost across three soil landscapes in interior Alaska: rocky uplands, silty uplands, and sandy lowlands. This dataset includes information from each intensive study site on soil stratigraphy, texture, bulk density, moisture, water storage, pH, electrical conductivity, organic and inorganic carbon content, and ice content.
#'
#' @param dataDir string that specifies the data directory
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
#' @examples
#' 
#' @importFrom EML read_eml
readBrown2013 <- function(dataDir = 'data/Brown2013'){
  
  ##### Download data files ####
  
  datafiles.ls <- list(YRB_cores = file.path(dataDir, 'YRB_cores.csv'),
                       meta = file.path(dataDir, 'metadata.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-bnz%2F549%2F4',
                      YRB_cores = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-bnz%2F549%2F4%2F5b8f51bc03f3e816bb41700b53010e0e')
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    try({
      download.file(url = download.ls$YRB_cores, destfile = datafiles.ls$YRB_cores)
      download.file(url = download.ls$meta, destfile=datafiles.ls$meta)
    }
    )
  }
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1228/2")
  }
  
  ##### Read in data ####
  
  YRB_cores.csv <- read_csv(datafiles.ls$YRB_cores)
  Brown_meta <- EML::read_eml(datafiles.ls$meta)
  
  ##### Pull study information ####
  BrownStudy.ls <- list(abstract = Brown_meta$dataset$abstract$para,
                         rights = Brown_meta$dataset$intellectualRights$section,
                         methods = Brown_meta$dataset$methods$methodStep$description$section)
  
  ##### Pull column names and units ####
  Brown_attribute <- pullAttributes(eml_filename = datafiles.ls$meta)
  
  return(list(filenames = datafiles.ls,
              studyInfo = BrownStudy.ls,
              meta = Brown_attribute,
              data1 = YRB_cores.csv))
} # end function
