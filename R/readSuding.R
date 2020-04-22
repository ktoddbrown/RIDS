#' Extended Growing Season
#' 
#' Read in data from Katharine N Suding, Jane G Smith, and Niwot Ridge LTER. 2020. Black sand extended growing season experiment aboveground net primary productivity, from 2019 to ongoing, yearly. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-nwt/265/1. 
#'
#'
#' 
#' @param dataDir string that specifies the data directory
#' @param verbose a boolean flag that turns on lots of comments
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#' 
#' @importFrom EML. read.eml

readSuding <- function(dataDir = 'data/Suding2019', verbose = FALSE){
  #dataDir <- 'data/Suding2019'
  #verbose <- FALSE
  Sudfiles.ls <- list(data = file.path(dataDir, 'data.csv'),
                       package = file.path(dataDir, 'packageReport.xml'),
                       meta = file.path(dataDir, 'metadata.xml'))
  
  SudingDown.ls <- list(meta = 'https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-nwt%2F265%2F1.xml',
                      package = 'https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Freport%2Feml%2Fknb-lter-nwt%2F265%2F1.xml',
                      data = 'https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-nwt%2F265%2F1%2F9a2e3f9fc5a38f5831065088ee9a75bf.csv')
  
  if(!all(file.exists(unlist(Sudfiles.ls)))){
    downloadFileFun <- function(){
      download.file(url = SudingDown.ls$data, destfile = Sudfiles.ls$data)
      download.file(url = SudingDown.ls$package, destfile = Sudfiles.ls$package)
      download.file(url = SudingDown.ls$meta, destfile= Sudfiles.ls$meta)
      return(NULL)
    }
    
    tryCatch(downloadFileFun(), error = function(e) e)
  }else{
    if(verbose){
      print('Files downloaded...moving on')
    }
  }
  
  #check to see if we can download an otherwise tell the user to go download the files
  if(!all(file.exists(unlist(Sudfiles.ls)))){
    stop("Go download the Suding data files from here: Katharine N Suding, Jane G Smith, and Niwot Ridge LTER. 2020. Black sand extended growing season experiment aboveground net primary productivity, from 2019 to ongoing, yearly. LTER Network Member Node.")
  }
 
  ##Reading data##   
  Suding.df <- read_csv(Sudfiles.ls$data)
  SudingMeta.eml <- EML::read_eml(Sudfiles.ls$meta)  
    
  ##Study info##  
  SudingStudy.ls <- list(abstract = SudingMeta.eml$dataset$abstract$para,
                           rights = SudingMeta.eml$dataset$intellectualRights$para,
                           methods = SudingMeta.eml$dataset$methods$methodStep$description$para)
  
  # Pull column names and units
  SudingAtt.df <- pullAttributes(eml_filename = Sudfiles.ls$meta)
  
  # return the datafiles and the metadata and file path to data the data files
  return(list(filenames = Sudfiles.ls,
              meta = SudingAtt.df, 
              studyInfo = SudingStudy.ls, 
              data = Suding.df))
}
