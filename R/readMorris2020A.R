#' Read in Moriss 2020a (Porewater)
#'
#'  downloaded from https://search.dataone.org/view/https://pasta.lternet.edu/package/metadata/eml/edi/136/5
#'   
#'   ABSTRACT:Porewater nutrient concentrations were measured as a component of a long-term project seeking to understand how salt marsh primary production and sediment chemistry respond to anthropogenic (e.g. eutrophication) and natural (e.g. sea-level rise) environmental change. Feedbacks between plants, sediments, nutrients and flooding were investigated with particular attention to mechanisms that keep marshes in equilibrium with sea level. Other data collected as part of the project include aboveground macrophyte biomass, plant density, marsh surface elevation and annual above ground primary productivity. These data have been used to develop the Marsh Equilibrium Model, an important tool for coastal resource managers.
#'
#' Sampling occurred at Spartina alterniflora-dominated salt marsh sites in North Inlet, a relatively pristine estuary near Georgetown, SC on the SE coast of the United States. North Inlet is a tidally-dominated, bar-built estuary, with a semi-diurnal mixed tide and a tidal range of 1.4m. The 25-km2 estuary is comprised of about 20.5 km2 of intertidal salt marsh and mudflats, and 4.5 km2 of open water. Sampling began at two locations in December 1993, and at three additional locations in January 1994. Sampling occurred approximately monthly through 2019. Sampling occured at a sixth location from 2006 to 2010. The site was a dieback site that had recovered by 2010. At the other sites, the study is on-going.
#' 
#' Porewater was collected at multiple depths from diffusion samplers and was analyzed for sulfide, chloride, ammonium, phosphate , and iron concentrations. There are five sampling locations at three sites. Two locations are in the low marsh; three locations are in the high marsh. One high marsh location had control sampling plots in addition to plots fertilized with nitrogen and phosphorus.
#' 
#' 
#'
#' @param dataDir string that specifies the data directory
#' @param verbose a boolean flag that turns on lots of comments
#'
#' @return a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
#' @export
#'
readMorris2020A <- function(dataDir = 'data/Morris2020Porewater', verbose = FALSE){
  
  #Download data files
  if(!all(file.exists(c('data/Morris2020Porewater/data1.0.csv', 'data/Morris2020Porewater/Metadata2.0.xml', 'data/Morris2020Porewater/package.xml')))){
    #if they don't exist, this download them onto you machine
    download.file(url = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fedi%2F136%2F5.xml', 
                  destfile = 'data/Morris20202Porewater/Metadata2.0')
    download.file(url = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Freport%2Feml%2Fedi%2F136%2F5.xml', 
                  destfile = 'data/Morris2020Porewater/package')
    download.file(url=  'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fedi%2F136%2F5%2F7fbe07a9a364e73cdaae941b0362021d.csv', 
                  destfile = 'data/Morris2020Porewater/data1.0')
  }else{
    print('Files downloaded...moving on')
  }
  
  datafiles.ls <- list(data = 'data/Morris2020Porewater/data1.0.csv',
                       package = 'data/Morris2020Porewater/package.xml',
                       meta = 'data/Morris2020Porewater/Metadata2.0.xml')
  
  download.ls <- list(meta = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fmetadata%2Feml%2Fknb-lter-hbr%2F197%2F4',
                      package = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Freport%2Feml%2Fknb-lter-hbr%2F197%2F4',
                      data = 'https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-hbr%2F197%2F4%2Ff6a818339216aa8ad7174fe7bf5d370e')
  
  if(!all(file.exists(unlist(datafiles.ls)))){
    download.file(url = download.ls$data, destfile = datafiles.ls$data)
    download.file(url = download.ls$package, destfile = datafiles.ls$package)
    download.file(url = download.ls$meta, destfile=datafiles.ls$meta)
  }
    #read in the data 
    Morris.df <- read_csv('data/Morris2020Porewater/data1.0.csv')
    
    MorrisMeta.eml <- EML::read_eml('data/Morris2020Porewater/Metadata2.0.xml')
    
    #Parse the meta data
    MorrisStudy.a <- list(abstract = MorrisMeta.eml$dataset$abstract$para,
                          rights = MorrisMeta.eml$dataset$intellectualRights$para,
                          methods = MorrisMeta.eml$dataset$methods$methodStep$description$para)
    
    #sets up data frame to be populated by the function
    attribute.df <- data.frame(name=as.character(rep(NA, length=18)),
                               description = as.character(rep(NA, length=18)),
                               unit = as.character(NA, length=18), stringsAsFactors = FALSE)
    
    
    for(columnIndex in 1:18){ #go through each column
      
      attribute.ls <- MorrisMeta.eml$dataset$dataTable$attributeList$attribute[[columnIndex]]
      
      attribute.df[columnIndex,1:2] <- c(attribute.ls$attributeName,
                                         attribute.ls$attributeDefinition)
      
      if( columnIndex == 3){
        #do nothing, column is a time string, pringts 'YYYY'
      }else if(columnIndex == 1) {
        #there are three location codes in column index 1
        attribute.df[columnIndex, 3] <- sprintf('%s = %s; %s = %s; %s = %s',
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[1]]$code,
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[1]]$definition,                                           
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[2]]$code,
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[2]]$definition,                                           
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[3]]$code,
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[3]]$definition)
        
      }else if(columnIndex == 4){
        #do nothing, 'month samples were collected' unimportant
      }else if(columnIndex == 5){
        #do nothing, 'day samples were collected' unimportant
        #   attribute.df[columnIndex, 3] <- attribute.ls$measurementScale$ratio$unit$customUnit
      }else if(columnIndex == 2) {
        
        attribute.df[columnIndex, 3] <- sprintf('%s = %s; %s = %s',
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[1]]$code,
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[1]]$definition,                                           
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[2]]$code,
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[2]]$definition)           
        
      }
      else if(columnIndex == 6) {
        #there are four factor values that have defined control vocabulary
        attribute.df[columnIndex, 3] <- sprintf('%s = %s; %s = %s',
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[1]]$code,
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[1]]$definition,                                           
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[2]]$code,
                                                attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[2]]$definition)           
        
      }
      else if(columnIndex == 7 | columnIndex == 9 | columnIndex == 11 | columnIndex == 13 | columnIndex == 15  | columnIndex==17) {
        #extracts units from meta data
        attribute.df[columnIndex, 3] <- attribute.ls$measurementScale$ratio$unit
        
      }
      else if(columnIndex == 8 | columnIndex == 10 | columnIndex == 12 | columnIndex == 14 | columnIndex == 16 | columnIndex == 18) {
        #Data flags for different compounds
        attribute.df[columnIndex, 3] <- attribute.ls$measurementScale$nominal$nonNumericDomain$textDomain
      }
    }  
        
        #returns data files and meta data  file path to data the data files
        return(list(filenames = datafiles.ls,
                    meta = attribute.df,
                    StudyInfo = MorrisStudy.a,
                    data = Morris.df))
}
    


