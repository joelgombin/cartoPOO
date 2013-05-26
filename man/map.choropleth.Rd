\name{map.choropleth}
\alias{map.choropleth}
\alias{map.choropleth-class}
\title{S4 class for choropleth maps}
\description{
  The \code{map.choropleth} class is a sub-class of the
  \code{map} class, for choropleth maps.
}
\section{Slots}{
  \describe{ \item{\code{varname}:}{The name of the
  variable to be mapped, from \code{data}. Passed as a
  \code{character}.} \item{\code{sp.key}:}{The name of the
  ID variable from the \code{sp}.}
  \item{\code{data.key}:}{The name of the ID variable frome
  the \code{data}.} \item{\code{cuts}:}{Vector of values
  used as breaks for the class intervals.}
  \item{\code{cut.method}:}{Method used to provide the
  class intervals. See \code{style} in the
  \code{\link{classIntervals}} function from the
  \code{classInt} package.} \item{\code{cols}:}{Vector of
  colors of the polygons to be plotted.}
  \item{\code{col}:}{Palette to be used.}
  \item{\code{NA.white}:}{If \code{TRUE}, \code{NA} values
  are represented in white, if \code{FALSE} with shades.}
  \item{\code{borders}:}{The color in which the borders of
  polygons should be drawn.} \item{\code{legend}:}{A list
  with the parameters of the legend. If it is an empty
  list, no legend will be drawn.}
  \item{\code{na.values}:}{A vector, the same length as the
  variable to be plotted, with logical values. \code{TRUE}
  when the variable to be plotted is \code{NA}.}
  \item{\code{palette}:}{Name of the color palette to use.}
  \item{\code{order}:}{Logical, "normal" or "reverse".} }
}
\author{
  Joel Gombin
}
\seealso{
  \code{\link{map}}, \code{\link{map.tps}},
  \code{\link{map.loess}}
}

