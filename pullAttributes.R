#' Pull attribute metadata
#' 
#' From the EML provided by DataOne, we pull the column names, long definitions, and units.
#'
#' @param eml_filename a set of charatures identifying the EML file downloaded from DataOne
#' @param verbose TRUE/FALSE to print out a whole bunch of useful(?) statements for debugging mostly
#'
#' @return a dataframe with the dataTable index, column names, descriptions, and units
#' @export
#'
#' @importFrom EML read_eml
#'
pullAttributes <- function(eml_filename, verbose = FALSE){
  
  eml.ls <- EML::read_eml(eml_filename)
  #eml.ls <- EML::read_eml('data/Forrester2019/https___pasta.lternet.edu_package_metadata_eml_knb-lter-nwt_181_2.xml')
  #eml.ls <- EML::read_eml('data/Groffman2012/Groffman_metadata.xml')
  
  
  ##The above lapply is the same as the for-loop below, but faster and shorter
  #attributeCount <- 0
  #for(dataTable.ls in eml.ls$dataset$dataTable){
  #  attributeCount <- length(dataTable.ls) + attributeCount
  #}
  
  if('attributeList' %in% names(eml.ls$dataset$dataTable)){
    dataTableList <- list(eml.ls$dataset$dataTable)
    dataTableCount <- 1
  }else{
    dataTableList <- eml.ls$dataset$dataTable
    dataTableCount <- length(eml.ls$dataset$dataTable)
  }
  
  #figure out how bit the attribute data frame needs to be
  attributeCount <- sum(unlist(lapply(dataTableList, function(xx){
    length(xx$attributeList$attribute)
  })))
  
  attribute <- data.frame(dataTabelIndex = as.numeric(rep(NA, length=attributeCount)),
                          name=as.character(rep(NA, length=attributeCount)),
                          description = as.character(rep(NA, length=attributeCount)),
                          unit = as.character(NA, length=attributeCount), stringsAsFactors = FALSE)
  
  rowIndex <- 1
  dataTableIndex <- 1
  #figure out which datatable we are in
  for(dataTable in dataTableList) {
    #dataTable <- eml.ls$dataset$dataTable[[1]] ##dev case
    
    #figure out which column we are in
    for(columnAttr in dataTable$attributeList$attribute){    #columnIndex <- NA
      #columnAttr <- dataTable$attributeList$attribute[[8]]
      
      ###Names and descriptions are in regular places universally true
      #attribute[columnIndex, 'name'] <- elm.ls$dataset$dataTable[[datatableIndex]]$attributeList$attribute[[columnIndex]]$attributeName
      #attribute[columnIndex, 'description'] <- elm.ls$dataset$dataTable[[datatableIndex]]$attributeList$attribute[[columnIndex]]$attributeDefinition
      
      attribute[rowIndex, 'dataTabelIndex'] <- dataTableIndex
      attribute[rowIndex, 'name'] <- columnAttr$attributeName
      attribute[rowIndex, 'description'] <- columnAttr$attributeDefinition
      
      ##Units on the other hands are special
      
      if('dateTime' %in% names(columnAttr$measurementScale)){
        if(verbose) print('dataTime found')
        ##2 Dates
        #attribute[columnIndex, 'unit'] <- meta$dataset$dataTable[[1]]$attributeList$attribute[[columnIndex]]$measurementScale$dateTime$formatString
        attribute[rowIndex, 'unit'] <- columnAttr$measurementScale$dateTime$formatString
      }else if('ratio' %in% names(columnAttr$measurementScale)){
        if(verbose) print('ratio found')
        if('standardUnit' %in% names(columnAttr$measurementScale$ratio$unit)){
          if(verbose) print('standardUnit found')
        ##3 Standard units
        #attribute[columnIndex, 'unit'] <- meta$dataset$dataTable[[1]]$attributeList$attribute[[columnIndex]]$measurementScale$ratio$unit$standardUnit
          attribute[rowIndex, 'unit'] <- columnAttr$measurementScale$ratio$unit$standardUnit
        }else if('customUnit' %in% names(columnAttr$measurementScale$ratio$unit)){
          if(verbose) print('customUnit found')
        ## 4 Custon Units
        #attribute[columnIndex, 'unit'] <- meta$dataset$dataTable[[dataTableIndex]]$attributeList$attribute[[7]]$measurementScale$ratio$unit$customUnit
          attribute[rowIndex, 'unit'] <- columnAttr$measurementScale$ratio$unit$customUnit
        }else{
          if(verbose) print('no match found')
        #do nothing... not sure what should go here. Good place to put a verbose warning.
        }#close unit check on columnAttr$measurementScale$ratio$unit
      }else if('nominal' %in% names(columnAttr$measurementScale)){
        if(verbose) print('nominal found')
        if('nonNumericDomain' %in% names(columnAttr$measurementScale$nominal)){
          if(verbose) print('nonNumericDomain found')
          if('enumeratedDomain' %in% names(columnAttr$measurementScale$nominal$nonNumericDomain)){
            if(verbose) print('enumeratedDomain found')
            ##5 factor keys
            #factorIndex <- NA
            #attribute[columnIndex, 3] <- sprintf('%s = %s; %s = %s; %s = %s; %s = %s',
            #attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[factorIndex]]$code,
            #attribute.ls$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition[[factorIndex]]$definition)
            if(is.list(columnAttr$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition) & #is a list
               is.null(names(columnAttr$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition))){ #without names
              attribute[rowIndex, 'unit'] <- paste0(unlist(lapply(columnAttr$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition,
                                                    function(xx){
                                                      paste0('[',xx$code, ']' ,' = ', xx$definition)
                                                    })), collapse = '; ')
            }else if(is.list(columnAttr$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition) & #is a list
                     ('code' %in% names(columnAttr$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition))){
              attribute[rowIndex, 'unit'] <- paste0('[',
                                          columnAttr$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition$code,
                                                    ']' ,' = ', 
                                    columnAttr$measurementScale$nominal$nonNumericDomain$enumeratedDomain$codeDefinition$definition)
            }else{
              if(verbose) print('unknown factor notation')
            }
          }#ennumeratedDomaine if-statement
        } #nonNumericalDomain if-statement
        
        ##1 Unitless
        #do nothing, columns are unitless
      }#close ratio check on columnAttr$measurementScale
      rowIndex <- rowIndex + 1
    } #attribute for-loop
    dataTableIndex <- dataTableIndex + 1
  } #data table for-loop
  
  return(attribute)
}