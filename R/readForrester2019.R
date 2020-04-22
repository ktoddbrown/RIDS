#' Read in Forrester 2019 (soil moisture with albedo shifts and snow depth)
#'
#'  Reads in data from Chiara Forrester and Niwot Ridge LTER. 2019. Soil moisture and snowdepth measurements in the Black Sand experiment for East Knoll, Audubon, Lefty, Soddie and Trough, 2018 - ongoing. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-nwt/181/2.
#'
#' @param dataDir string that specifies the data directory
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
#' @importFrom EML read_eml
readForrester2019 <- function(dataDir = 'data/Forrester2019', verbose = FALSE){
  
  ##### Download data files ####
  
  datafiles.ls <- list(data1 = file.path(dataDir, 'https___pasta.lternet.edu_package_data_eml_knb-lter-nwt_181_2_0a4cde4c5e6cf67d5bbcbf73f1750033.csv'),
                       data2 = file.path(dataDir, 'https___pasta.lternet.edu_package_data_eml_knb-lter-nwt_181_2_2a66891095dcc0cbdc7dbd9c4da4f42d.csv'),
                       meta = file.path(dataDir, 'https___pasta.lternet.edu_package_metadata_eml_knb-lter-nwt_181_2.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-nwt%2F181%2F2',
                      data2 = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-nwt%2F181%2F2%2F2a66891095dcc0cbdc7dbd9c4da4f42d',
                      data1 = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-nwt%2F181%2F2%2F0a4cde4c5e6cf67d5bbcbf73f1750033')
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    try({
      download.file(url = download.ls$data1, destfile = datafiles.ls$data1)
      download.file(url = download.ls$data2, destfile = datafiles.ls$data2)
      download.file(url = download.ls$meta, destfile=datafiles.ls$meta)
    }
    )
  }
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-nwt/181/2")
  }
  
  ##### Read in data ####
  
  ForrestData1 <- read_csv(datafiles.ls$data1)
  ForrestData2 <- read_csv(datafiles.ls$data2)
  Forrest_meta <- EML::read_eml(datafiles.ls$meta)
  
  ##### Pull study information ####
  ForrestStudy.ls <- list(abstract = Forrest_meta$dataset$abstract$para,
                           rights = Forrest_meta$dataset$intellectualRights$para,
                           methods = Forrest_meta$dataset$methods$methodStep$description$para)
  
  ##### Pull column names and units ####
  Forrest_attribut <- pullAttributes(eml_filename = datafiles.ls$meta)
  
  return(list(filenames = datafiles.ls,
         studyInfo = ForrestStudy.ls,
         meta = Forrest_attribut,
         data1 = ForrestData1, data2 = ForrestData2))
} # end function