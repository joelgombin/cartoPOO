\name{map.loess}
\alias{map.loess}
\alias{map.loess-class}
\title{S4 class for local-fitted (loess) maps}
\description{
  The \code{map.loess} class is a sub-class of the
  \code{map} class, for local-fitted (loess) maps.
}
\section{Slots}{
  \describe{ \item{\code{varname}:}{The name of the
  variable to be mapped, from \code{data}. Passed as a
  \code{character}.} \item{\code{sp.key}:}{The name of the
  ID variable from the \code{sp}.}
  \item{\code{data.key}:}{The name of the ID variable frome
  the \code{data}.} \item{\code{contours}:}{A
  \code{logical}, whether the contours should be drawn.}
  \item{\code{points}:}{A \code{logical}, whether the
  initial points should be drawn.}
  \item{\code{xrange}:}{\code{Numeric}, number of grid
  points on the \code{x} axis.}
  \item{\code{yrange}:}{\code{Numeric}, number of grid
  points on the \code{y} axis.} \item{\code{span}:}{A
  \code{numeric} which controls the degree of smoothing
  (see the \code{\link{loess}} function).}
  \item{\code{frame}:}{An object of the
  \code{SpatialPolygons}, with the external contour of the
  \code{SpatialPolygonsDataFrame}.} \item{\code{inter}:}{A
  \code{list} containing the pixels coordinates and
  estimated values.} \item{\code{df}:}{A \code{data.frame}
  containing the initial data.} \item{\code{palette}:}{A
  \code{character}, the name of the color palette to be
  used.} \item{\code{legend}:}{A \code{list} with the
  parameters of the legend. If it is an empty list, no
  legend will be drawn.} }
}
\author{
  Joel Gombin
}
\seealso{
  \code{\link{map}}, \code{\link{map.choropleth}},
  \code{\link{map.tps}}
}

