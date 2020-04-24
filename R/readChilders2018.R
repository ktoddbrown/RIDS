#' Read in Childers2018 data (Shark River Slough Water Quality Data)
#' 
#' Reads in data from various wetlands sites in the Shark River Slough, Everglades National Park. 2018. Water quality data that measures salinity, total nitrogen and total phosphorus.
#'
#' @param dataDir string that specifies the data directory
#' @param verbose boolean flag to print out useful debugging statements
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
#' @importFrom EML read_EML
#' @importFrom CSV read_csv
readChilders2018 <- function(dataDir = 'data/Childers2018', verbose = FALSE){
  
  ####Download the files####
  
  #Path for the files to be downloaded
  
  datafiles.ls <- list(data = file.path('data/Childers2018/Childers2018_data.csv'),
                       package = file.path('data/Childers2018/Childers2018_packageReport.xml'),
                       meta = file.path('data/Childers2018/Childers2018_metadata.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1072/11',
                      package = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/report/eml/knb-lter-fce/1072/11',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/data/eml/knb-lter-fce/1072/11/618e65f08d54aa803bf905da4dd8b1d5')
  
  #Downloads the files from each URL
  if(!all(file.exists(unlist(datafiles.ls)))){ 
    downloadFileFun <- function(){
      download.file(url = download.ls$data, destfile = datafiles.ls$data)
      download.file(url = download.ls$package, destfile = datafiles.ls$package)
      download.file(url = download.ls$meta, destfile=datafiles.ls$meta)
      return(NULL)
    }
    tryCatch(downloadFileFun(), error = function(e) e)
  }else{
    #print('Files download...moving on')
  }
  #Message that prints if the files weren't downloaded
  if(!all(file.exists(unlist(datafiles.ls)))){ 
    stop("Go download the Childers data files from here: Evelyn Gaiser and Daniel Childers||Global Institute of Sustainability| School of Sustainability. 2018. Water Quality Data (Extensive) from the Shark River Slough, Everglades National Park (FCE), from October 2000 to Present. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1072/11.")
  }
  
  #### Read in the data ####
  
  Childers2018.df <- read_csv(file.path(datafiles.ls$data))
  ChildersMeta2018.eml <- EML::read_eml(datafiles.ls$meta)
  
  #### Pull the study information ####
  
  ChildersStudy2018.ls <- list( abstract = ChildersMeta2018.eml$dataset$abstract$para,
                                  rights = ChildersMeta2018.eml$dataset$intellectualRights$para,
                                  method = ChildersMeta2018.eml$dataset$methods$methodStep$description$para)
  
  #### Pull column names and units ####
  ChildersAttribute2018.df <- pullAttributes(eml_filename = datafiles.ls$meta)
  
  return(list(filenames = datafiles.ls, 
              data = Childers2018.df,
              studyInfo = ChildersStudy2018.ls,
              meta = ChildersAttribute2018.df))
}  
