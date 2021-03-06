% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readForrester2019.R
\name{readForrester2019}
\alias{readForrester2019}
\title{Read in Forrester 2019 (soil moisture with albedo shifts and snow depth)}
\usage{
readForrester2019(dataDir = "data/Forrester2019", verbose = FALSE)
}
\arguments{
\item{dataDir}{string that specifies the data directory}

\item{verbose}{boolean flag to print out useful debugging statements}
}
\value{
a list that contains the tabular dataset, a tabular version of the meta-data, the file names of the local data copies, a list of study information (abstract, copy rights, method notes)
}
\description{
Reads in data from Chiara Forrester and Niwot Ridge LTER. 2019. Soil moisture and snowdepth measurements in the Black Sand experiment for East Knoll, Audubon, Lefty, Soddie and Trough, 2018 - ongoing. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-nwt/181/2.
}
