\name{map}
\alias{map}
\title{Create a \code{map} object}
\usage{
  map(sp, data)
}
\arguments{
  \item{sp}{an object of class
  \code{SpatialPolygonsDataFrame}.}

  \item{data}{an object of class \code{data.frame}.}
}
\value{
  An object of class \code{map}.
}
\description{
  \code{map} creates a \code{map} object from a
  \code{SpatialPolygonsDataFrame} and a \code{data.frame}.
  This object can then be used by several functions.
}
\author{
  Joel Gombin
}
\seealso{
  \code{\link{choropleth}}, \code{\link{Tps}},
  \code{\link{loess}}, \code{\link{flow}}
}

