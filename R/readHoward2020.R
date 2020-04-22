#' Read in Howard 2020 (soil carbon content in Florida Bay and Brazil)
#'
#'  Reads in data from Jason L Howard and James W Fourqurean. 2020. Organic and inorganic data for soil cores from Brazil and Florida Bay seagrasses to support Howard et al 2018, CO2 released by carbonate sediment production in some coastal areas may offset the benefits of seagrass “Blue Carbon” storage, Limnology and Oceanography, DOI: 10.1002/lno.10621. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1228/2. 
#'  This data set looks at soil carbon content in Brazil, a country with a tropical climate.This data set measures soils' organic and inorganic carbon from Florida Bay and Brazilian seagrass meadows for the top 1 m of soil. Instrumental analyses and loss on ignition at 500C were used to measure C content of downcore slices.
#'
#' @param dataDir string that specifies the data directory
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
#' @examples
#' 
#' @importFrom EML read_eml
readHoward2020 <- function(dataDir = 'data/Howard2020'){
  
  ##### Download data files ####
  
  datafiles.ls <- list(brazil_cores = file.path(dataDir, 'brazil_cores.csv'),
                       Fl_bay_cores = file.path(dataDir, 'Fl_bay_cores'),
                       meta = file.path(dataDir, 'metadata.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-fce%2F1228%2F2',
                      Fl_bay_cores = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-fce%2F1228%2F2%2Fea42cda71af1f5725280ef22d3e29aca',
                      brazil_cores = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-fce%2F1228%2F2%2F06cf57a513ea48bcf94e93b2df94f030')
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    try({
      download.file(url = download.ls$brazil_cores, destfile = datafiles.ls$brazil_cores)
      download.file(url = download.ls$Fl_bay_cores, destfile = datafiles.ls$Fl_bay_cores)
      download.file(url = download.ls$meta, destfile=datafiles.ls$meta)
    }
    )
  }
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop("Data downloaded manually from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1228/2")
  }
  
  ##### Read in data ####
  
  HowardData1 <- read_csv(datafiles.ls$brazil_cores)
  HowardData2 <- read_csv(datafiles.ls$Fl_bay_cores)
  Howard_meta <- EML::read_eml(datafiles.ls$meta)
  
  ##### Pull study information ####
  HowardStudy.ls <- list(abstract = Howard_meta$dataset$abstract$para,
                          rights = Howard_meta$dataset$intellectualRights$para,
                          methods = Howard_meta$dataset$methods$methodStep)
  
  ##### Pull column names and units ####
  Howard_attribute <- pullAttributes(eml_filename = datafiles.ls$meta)
  
  return(list(filenames = datafiles.ls,
              studyInfo = HowardStudy.ls,
              meta = Howard_attribute,
              data1 = HowardData1, data2 = HowardData2))
} # end function