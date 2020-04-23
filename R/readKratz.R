#' Read in Kratz 2005 (Mercury concentration in fish vs. hydrological properties)
#'
#'Reads in data from Timothy Kratz. Timothy Kratz. 2005. Landscape Position Project at North Temperate Lakes LTER: Fish Mercury Level 1998 - 1999. LTER Network Member Node. knb-lter-ntl.99.7. 
#'
#' @param dataDir string that specifies the data directory
#' @param verbose a boolean flag that helps us decode
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information
#' 
#' @examples 
#' readKratz(dataDir = 'data/Kratz2005', verbose = FALSE)
#' 
#' @export
#' 
#' @importFrom EML read_eml
readKratz <- function(dataDir = 'data/Kratz2005', verbose = FALSE){
  
  datafiles.ls <- list(meta = file.path(dataDir, 'Kratz_metadata.xml'),
                       package = file.path(dataDir, 'Kratz_packageReport.xml'),
                       data = file.path(dataDir, 'Kratz_data.csv'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-ntl%2F99%2F7',
                      package = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Freport%2Feml%2Fknb-lter-ntl%2F99%2F7',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-ntl%2F99%2F7%2Flpp_hglvl_data')
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    try({
      download.file(url = download.ls$meta, destfile = datafiles.ls$meta)
      download.file(url = download.ls$package, destfile = datafiles.ls$package)
      download.file(url = download.ls$data, destfile=datafiles.ls$data)
    }
    )
  }
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-ntl/99/7")
  }
  
  ##### Reading in the data ####
  
  KratzMeta.ls <- EML::read_eml(datafiles.ls$meta)
  KratzData.csv <- read_csv(datafiles.ls$data)
  
  ##### Parsing the attribute table ####
  
  #Use the pullAttribute.R script created by the professor
  KratzAttribute.df <- pullAttributes(KratzMeta.ls, verbose = FALSE)
  
  return(list(filenames = datafiles.ls,
              studyInfo = KratzMeta.ls,
              meta = KratzAttribute.df,
              data = KratzData.csv)) 
}#end of readKratz.R
