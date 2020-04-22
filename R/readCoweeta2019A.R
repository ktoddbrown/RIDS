#' Read in Coweeta 2019 Carbon and Nitrogen data
#' 
#'
#'  Reads in data from Coweeta Long Term Ecological Research Program and Jennifer D. Knoepp. 2019. Soil percent carbon and nitrogen from 9 hillslopes sites in Macon County, North Carolina, within the Upper Little Tennessee River Basin. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-cwt/1148/13.
#'  
#'
#'
#' @param dataDir string that specifies the data directory
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#' 
#' @importFrom EML read_eml
#' @importFrom readr read_delim

readCar_Nit <- function(dataDir = 'data/Coweeta2019C_N'){
  
  ##### Download data files ####
  
  datafilescarbonNitrogen.ls <- list(metadata = file.path(dataDir, 'metadata.xml'),
                                  packageReport = file.path(dataDir, 'packageReport.xml'),
                                  data = file.path(dataDir, 'data.csv'), 
                                  KML = file.path(dataDir, 'KML.kml'))
  
  downloadcarbonNitrogen.ls <- list(metadata = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-cwt%2F1148%2F13',
                                 packageReport = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Freport%2Feml%2Fknb-lter-cwt%2F1148%2F13',
                                 data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-cwt%2F1148%2F13%2F20fa8b4ab429674a30d4e501f7cc2a6c',
                                 KML = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-cwt%2F1148%2F13%2Fc7d5be259dcab2e6fb0108bea07d0705')
  
  if(!all(file.exists(unlist(datafilescarbonNitrogen.ls)))){
    try({
      download.file(url = downloadcarbonNitrogen.ls$data, destfile = datafilescarbonNitrogen.ls$data, method = 'libcurl')
      download.file(url = downloadcarbonNitrogen.ls$packageReport, destfile = datafilescarbonNitrogen.ls$packageReport, method = 'libcurl')
      download.file(url = downloadcarbonNitrogen.ls$meta, destfile = datafilescarbonNitrogen.ls$meta, method = 'libcurl')
      download.file(url = downloadcarbonNitrogen.ls$KML, destfile = datafilescarbonNitrogen.ls$KML, method = 'libcurl')
    }
    )
  }
  if(!all(file.exists(unlist(datafilescarbonNitrogen.ls)))){
    stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-cwt/1148/13")
  }
  
  ##### Read in data ####
  
  carbonNitrogenData <- read_delim(datafilescarbonNitrogen.ls$data, delim = '\t')
  carbonNitrogenMeta <- EML::read_eml(datafilescarbonNitrogen.ls$metadata)
  
  ##### Pull study information ####
  carbonNitrogenStudy.ls <- list(abstract = carbonNitrogenMeta$dataset$abstract,
                              rights = carbonNitrogenMeta$dataset$intellectualRights,
                              methods = carbonNitrogenMeta$dataset$methods)
  
  ##### Pull column names and units ####
  carbonNitrogen_attribute <- pullAttributes(eml_filename = datafilescarbonNitrogen.ls$metadata)
  
  return(list(filenames = datafilescarbonNitrogen.ls,
              studyInfo = carbonNitrogenStudy.ls,
              meta = carbonNitrogen_attribute,
              data = carbonNitrogenData))
} # end function
