#' Read in Fourqurean2019 (Florida Bay Nutrient Level Data)
#' 
#' Reads in data from the Florida Bay, Everglades National Park. 2019. Nutrient levels in various seagrass species, namely the Thalassia testudinum. Nutrients monitered are nitrogen, phosphorus, and carbon.
#'
#' @param dataDir string that specifies the data directory
#' @param verbose boolean flag to print out useful debugging statements
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
#' @importFrom EML read_EML
#' @importFrom CSV read_csv
readFourqurean2019 <- function(dataDir = 'data/Fourqurean2019', verbose = FALSE){
  
  ####Download the files####
  
  #Path for the files to be downloaded
  
  datafiles.ls <- list(data = file.path('data/Fourqurean2019/Fourqurean2019_data.csv'),
                       package = file.path('data/Fourqurean2019/Fourqurean2019_packageReport.xml'),
                       meta = file.path('data/Fourqurean2019/Fourqurean2019_metadata.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1128/6',
                      package = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/report/eml/knb-lter-fce/1128/6',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https://pasta.lternet.edu/package/data/eml/knb-lter-fce/1128/6/fd59e02955a4d6e5db093f8f823dd027')
  
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
    stop("Go download the Fourqurean2019 data files from here: James Fourqurean. 2019. Florida Bay Nutrient Data, Everglades National Park (FCE), South Florida from August 2008 to Present. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1128/6.")
  }
  
  #### Read in the data ####
  
  Fourqurean2019.df <- read_csv(file.path(datafiles.ls$data))
  FourqureanMeta2019.eml <- EML::read_eml(datafiles.ls$meta)
  
  #### Pull the study information ####
  
  FourqureanStudy2019.ls <- list( abstract = FourqureanMeta2019.eml$dataset$abstract$para,
                                  rights = FourqureanMeta2019.eml$dataset$intellectualRights$para,
                                  method = FourqureanMeta2019.eml$dataset$methods$methodStep$description$para)
  
  #### Pull column names and units ####
  FourqureanAttribute2019.df <- pullAttributes(eml_filename = datafiles.ls$meta)
  
  return(list(filenames = datafiles.ls, 
              data = Fourqurean2019.df,
              studyInfo = FourqureanStudy2019.ls,
              meta = FourqureanAttribute2019.df))
}