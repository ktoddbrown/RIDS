#' reading in Gaiser2009(2)
#'
#' reading ind data collected in NE shark slough, biogeochemcial data ['https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/knb-lter-fce/1060/8']
#'
#' @param dataDir path to the data directory
#' @param verbose 
#'
#' @return a list with the data, meta data, file information, and citation.
#' @export
#' @importFrom EML read_eml
readGaiser2 <- function(dataDir = 'data/Gaiser2009(2)', verbose = FALSE){
  
  
  ##### Download data files ####
  datafiles2.ls <- list(data = file.path(dataDir, 'data.txt'),
                       package = file.path(dataDir, 'packageReport.xml'),
                       meta = file.path(dataDir, 'metaData.xml'))
  
  download2.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-fce%2F1109%2F4.xml',
                      package = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Freport%2Feml%2Fknb-lter-fce%2F1109%2F4.xml',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-fce%2F1109%2F4%2Fb29ca2e3d8d2de81f62f13f73a4c574b.csv')
  
  if(!all(file.exists(unlist(datafiles2.ls)))){
    downloadFileFun <- function(){
      download.file(url = download.ls$data, destfile = datafiles2.ls$data)
      download.file(url = download.ls$package, destfile = datafiles2.ls$package)
      download.file(url = download.ls$meta, destfile=datafiles2.ls$meta)
      return(NULL)
    }
    tryCatch(downloadFileFun(), error = function(e) e)
  }else{
    if(verbose){
      print('Files downloaded...moving on')
    }
  }
  
  #check to see if we can download an otherwise tell the user to go download the files
  if(!all(file.exists(unlist(datafiles2.ls)))){
    stop("Go download the Gaiser data files from here: Evelyn Gaiser and Leonard Scinto. 2009. Biogeochemical data collected from Northeast Shark Slough, Everglades National Park (FCE LTER) from September 2006 to September 2008. LTER Network Member Node.")
  }
  
  ##### Read in the data ####
  Gaiser2.df <- read_csv('data/Gaiser2009(2)/data.txt', na = c("", "NA", "-9999.00", "-9999.0", "-9999"))
  GaiserMeta2.eml <- EML::read_eml('data/Gaiser2009(2)/packageReport.xml')
  
  
  ##### study info ####
  
  GaiserStudy2.ls <- list(abstract = GaiserMeta2.eml$dataset$abstract$para,
                         rights = GaiserMeta2.eml$dataset$intellectualRights$para,
                         methods = GaiserMeta2.eml$dataset$methods$methodStep$description$para)
  
  ##### Parse the metadata ####
  attribute2.df <- pullAttributes(eml_filename = datafiles2.ls$meta)
  
  #return the datafiles and the metadata and file path to data the data files
  return(list(filenames = datafiles2.ls,
              meta = attribute2.df, 
              studyInfo = GaiserStudy2.ls, 
              data = Gaiser2.df))
}



