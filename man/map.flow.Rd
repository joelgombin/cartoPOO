\name{map.flow}
\alias{map.flow}
\alias{map.flow-class}
\title{S4 class for flow maps}
\description{
  The \code{map.flow} class is a sub-class of the
  \code{map} class, for flow maps.
}
\section{Slots}{
  \describe{ \item{\code{varname}:}{The name of the
  variable to be mapped, from \code{data}. Passed as a
  \code{character}.} \item{\code{sp.key}:}{The name of the
  ID variable from the \code{sp}.}
  \item{\code{data.orig}:}{The name of the origin ID
  variable from the \code{data.frame}.}
  \item{\code{data.dest}:}{The name of the destination ID
  variable from the \code{data.frame}.}
  \item{\code{arrow.type}:}{The name of the variable, if
  any, that categorizes the arrows.}
  \item{\code{arrow.colors}:}{The colors used to represent
  the arrows or the different types of arrows.} }
}
\author{
  Joel Gombin
}
\seealso{
  \code{\link{map}}
}

