#' Read in Kelley 2019 (thaw index of permafrost)
#'
#'  Reads in data from Allison K Kelley, Elaine F. Pegoraro, Marguerite Mauritz, Jack A. Hutchings, Susan M.N. Natali, et al. 2019. Eight Mile Lake Research Watershed, Thaw Gradient: Seasonal thaw depth 2004-2019. LTER Network Member Node.
#'
#' @param dataDir string that specifies the data directory
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
#' @examples
#' 
#' @importFrom EML read_eml
#' 

readKelley2019 <- function(dataDir= 'data/Kelley2019', verbose= FALSE){

#### Download data ####  

dataFiles.ls <- list(data = file.path(dataDir, 'data.csv'),
                     meta = file.path(dataDir, 'metadata.xml'))

downloadFiles.ls <- list(data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-bnz%2F519%2F16%2F361c5c01b8c00c1b00005bbf460be3c8',
                         meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-bnz%2F519%2F16')

if(!all(file.exists(c(dataFiles.ls$data,dataFiles.ls$meta)))){
  download.file(url = downloadFiles.ls$data, 
                destfile = dataFile$data, method='curl') 
  download.file(url=  downloadFiles.ls$meta, 
                destfile = dataFiles.ls$meta, method='curl')
}else{
  print('Files downloaded...moving on')
}


#### Read data ####

Kelley2019.df <- read_csv(dataFiles.ls$data)
Kelley2019.eml <- EML::read_eml(dataFiles.ls$meta)


#### Parse metadata ####

attribute.df <- pullAttributes(eml_filename = dataFiles.ls$meta)


#### Study data ####

Kelley2019.ls <- list(filenames= dataFiles.ls, 
                      liscense = Kelley2019.eml$dataset$intellectualRights$section,
                      abstract = Kelley2019.eml$dataset$abstract$para,
                      metadata = attribute.df,
                      data = Kelley2019.df)


#### Return data ####

return(Kelley2019.ls)

}


