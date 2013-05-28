\name{map-class}
\alias{map-class}
\alias{show}
\alias{show,map-method}
\title{S4 class for maps}
\description{
  The \code{map} class allows maps to be treated as
  objects, on which methods can be applied to transform
  them. It has several sub-classes, and new methods or
  sub-classes can be contributed to this package.
}
\section{Slots}{
  \describe{ \item{\code{sp}:}{An object of
  \code{SpatialPolygonsDataFrame} class, with at least an
  ID variable in the \code{data.frame}.}
  \item{\code{data}:}{A \code{data.frame}, with the data to
  be mapped.} }
}
\author{
  Joel Gombin
}

