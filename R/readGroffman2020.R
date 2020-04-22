#' Read in Groffman 2012 (microbial biomass, C & N cycling, soil)
#'
#'   Reads in data from Peter M Groffman, Lisa Martel, and Jorge Duran. 2020. Hubbard Brook Experimental Forest: microbial biomass and activity at climate gradient plots, 2010-2012. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-hbr/197/4.
#' 
#' These data were gathered as part of the Hubbard Brook Ecosystem Study (HBES). The HBES is a collaborative effort at the Hubbard Brook Experimental Forest, which is operated and maintained by the USDA Forest Service, Northern Research Station.
#'
#' @param dataDir string that specifies the data directory
#' @param verbose a boolean flag that turns on lots of comments
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#' 
#' @importFrom EML read_eml
#'
readGroffman2020 <- function(dataDir = 'data/Groffman2012', verbose = FALSE){
  
    
  ##### Download data files ####
  datafiles.ls <- list(data = file.path(dataDir, 'Groffman_data.csv'),
                       package = file.path(dataDir, 'Groffman_packageReport.xml'),
                       meta = file.path(dataDir, 'Groffman_metadata.xml'))
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-hbr%2F197%2F4',
                      package = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Freport%2Feml%2Fknb-lter-hbr%2F197%2F4',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-hbr%2F197%2F4%2Ff6a818339216aa8ad7174fe7bf5d370e')
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    downloadFileFun <- function(){
      download.file(url = download.ls$data, destfile = datafiles.ls$data)
      download.file(url = download.ls$package, destfile = datafiles.ls$package)
      download.file(url = download.ls$meta, destfile=datafiles.ls$meta)
      return(NULL)
    }
    tryCatch(downloadFileFun(), error = function(e) e)
  }else{
    if(verbose){
      print('Files downloaded...moving on')
    }
  }

  #check to see if we can download an otherwise tell the user to go download the files
  if(!all(file.exists(unlist(datafiles.ls)))){
    stop("Go download the Groffman data files from here: Peter M Groffman, Lisa Martel, and Jorge Duran. 2020. Hubbard Brook Experimental Forest: microbial biomass and activity at climate gradient plots, 2010-2012.")
  }
  
  ##### Read in the data ####
  Groffman.df <- read_csv(datafiles.ls$data)
  GroffmanMeta.eml <- EML::read_eml(datafiles.ls$meta)
  
  ##### study info ####
  
  GroffmanStudy.ls <- list(abstract = GroffmanMeta.eml$dataset$abstract$para,
                           rights = GroffmanMeta.eml$dataset$intellectualRights$para,
                           methods = GroffmanMeta.eml$dataset$methods$methodStep$description$para)
  
  ##### Parse the metadata ####
  attribute.df <- pullAttributes(eml_filename = datafiles.ls$meta)
  
  # return the datafiles and the metadata and file path to data the data files
  return(list(filenames = datafiles.ls,
              meta = attribute.df, 
              studyInfo = GroffmanStudy.ls, 
              data = Groffman.df))
}