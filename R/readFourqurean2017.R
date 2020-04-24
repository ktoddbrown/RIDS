#' Read in Fourqurean2017 data (Florida Bay Productivity)
#' 
#' Reads in data from the Florida Bay, Everglades National Park. 2017. Productivity data of Thalassia testudinum (seagrass), various productivity indicatoins including leaf number, leaf mass and short shoot productivity. 
#'
#' @param dataDir string that specifies the data directory
#' @param verbose boolean flag to print out useful debugging statements
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
#' @importFrom EML read_EML
#' @importFrom CSV read_csv
readFourqurean2017 <- function(dataDir = 'data/Fourqurean2017', verbose = FALSE){
  
  ####Download the files####
  
  #Path for the files to be downloaded
  
  datafiles.ls <- list(data = file.path('data/Fourqurean2017/Fourqurean2017_data.csv'),
                       package = file.path('data/Fourqurean2017/Fourqurean2017_packageReport.xml'),
                       meta = file.path('data/Fourqurean2017/Fourqurean2017_metadata.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1130/4',
                      package = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/report/eml/knb-lter-fce/1130/4',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/data/eml/knb-lter-fce/1130/4/bc7b9dbe79dc89cf5258cba152573376')
  
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
    stop("Go download the Hinzman data files from here: James Fourqurean. 2017. Florida Bay Productivity Data, Everglades National Park (FCE), South Florida from September 2000 to Present. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1130/4.")
  }
  
  #### Read in the data ####
  
  Fourqurean2017.df <- read_csv(file.path(datafiles.ls$data))
  FourqureanMeta2017.eml <- EML::read_eml(datafiles.ls$meta)
  
  #### Pull the study information ####
  
  FourqureanStudy2017.ls <- list( abstract = FourqureanMeta2017.eml$dataset$abstract$para,
                                  rights = FourqureanMeta2017.eml$dataset$intellectualRights$para,
                                  method = FourqureanMeta2017.eml$dataset$methods$methodStep$description$para)
  
  #### Pull column names and units ####
  FourqureanAttribute2017.df <- pullAttributes(eml_filename = datafiles.ls$meta)
  
  return(list(filenames = datafiles.ls, 
              data = Fourqurean2017.df,
              studyInfo = FourqureanStudy2017.ls,
              meta = FourqureanAttribute2017.df))
}  