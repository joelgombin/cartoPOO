setOldClass("Krig")


#' S4 class for maps
#' 
#' The \code{map} class allows maps to be treated as objects, on which methods can be applied to transform them. It has several sub-classes, and new methods or sub-classes can be contributed to this package.
#' 
#' @name map
#' @rdname map
#' @exportClass map
#' @author Joel Gombin
#' @aliases map-class
#' @section Slots:
#' \describe{
#'    \item{\code{sp}:}{An object of \code{SpatialPolygonsDataFrame} class, with at least an ID variable in the \code{data.frame}.}
#'    \item{\code{data}:}{A \code{data.frame}, with the data to be mapped.}
#'  }
#'  
setClass("map", 
         representation(
           sp = "SpatialPolygonsDataFrame",
           data = "data.frame"))


#' S4 class for choropleth maps
#' 
#' The \code{map.choropleth} class is a sub-class of the \code{map} class, for choropleth maps.
#' 
#' @name map.choropleth
#' @rdname map.choropleth
#' @exportClass map.choropleth
#' @author Joel Gombin
#' @aliases map.choropleth-class
#' @seealso \code{\link{map}}, \code{\link{map.tps}}, \code{\link{map.loess}}
#' @section Slots:
#' \describe{
#'    \item{\code{varname}:}{The name of the variable to be mapped, from \code{data}. Passed as a \code{character}.}
#'    \item{\code{sp.key}:}{The name of the ID variable from the \code{sp}.}
#'    \item{\code{data.key}:}{The name of the ID variable frome the \code{data}.}
#'    \item{\code{cuts}:}{Vector of values used as breaks for the class intervals.}
#'    \item{\code{cut.method}:}{Method used to provide the class intervals. See \code{style} in the \code{\link{classIntervals}} function from the \code{classInt} package.}
#'    \item{\code{cols}:}{Vector of colors of the polygons to be plotted.}
#'    \item{\code{col}:}{Palette to be used.}
#'    \item{\code{NA.white}:}{If \code{TRUE}, \code{NA} values are represented in white, if \code{FALSE} with shades.}
#'    \item{\code{borders}:}{The color in which the borders of polygons should be drawn.}
#'    \item{\code{legend}:}{A list with the parameters of the legend. If it is an empty list, no legend will be drawn.}
#'    \item{\code{na.values}:}{A vector, the same length as the variable to be plotted, with logical values. \code{TRUE} when the variable to be plotted is \code{NA}.}
#'    \item{\code{palette}:}{Name of the color palette to use.}
#'    \item{\code{order}:}{Logical, "normal" or "reverse".}
#'  }
setClass(
  Class="map.choropleth",
  representation=representation(
    varname = "character",
    sp.key = "character",
    data.key = "character",
    cuts = "numeric",
    cut.method = "character",
    cols = "character",
    col = "character",
    NA.white= "logical",
    borders="character",
    legend = "list",
    na.values = "logical",
    palette = "character",
    order = "character"
    ), 
  contains="map"
)



#' S4 class for thin plate spline regressed maps
#' 
#' The \code{map.tps} class is a sub-class of the \code{map} class, for thin plate spline regressed maps. 
#' 
#' @name map.tps
#' @rdname map.tps
#' @exportClass map.tps
#' @author Joel Gombin
#' @aliases map.tps-class
#' @seealso \code{\link{map}}, \code{\link{map.choropleth}}, \code{\link{map.tps}}
#' @section Slots:
#' \describe{
#'    \item{\code{varname}:}{The name of the variable to be mapped, from \code{data}. Passed as a \code{character}.}
#'    \item{\code{sp.key}:}{The name of the ID variable from the \code{sp}.}
#'    \item{\code{data.key}:}{The name of the ID variable frome the \code{data}.}
#'    \item{\code{contours}:}{A \code{logical}, whether the contours should be drawn.}
#'    \item{\code{points}:}{A \code{logical}, whether the initial points should be drawn.}
#'    \item{\code{xrange}:}{\code{Numeric}, number of grid points on the \code{x} axis.}
#'    \item{\code{yrange}:}{\code{Numeric}, number of grid points on the \code{y} axis.}
#'    \item{\code{frame}:}{An object of the \code{SpatialPolygons}, with the external contour of the \code{SpatialPolygonsDataFrame}.}
#'    \item{\code{inter}:}{A \code{list} of class \code{Krig}, result of the \code{\link[fields]{Tps}} function.}
#'    \item{\code{donnees}:}{A \code{list} containing the pixels coordinates and estimated values.}
#'    \item{\code{col}:}{A \code{character}, the name of the color palette to use.}
#'    \item{\code{diverg}:}{A \code{logical}, whether the values are divergent or not.}
#'    \item{\code{diverg.zero}:}{A \code{numeric}, if the values are divergent, the value of the cutting point.}
#'    \item{\code{legend}:}{A \code{list} with the parameters of the legend. If it is an empty list, no legend will be drawn.}
#'  }
setClass(
  Class="map.tps",
  representation=representation(
    varname = "character",
    sp.key = "character",
    data.key = "character",
    contours = "logical",
    points="logical",
    xrange="numeric",
    yrange="numeric",
    frame="SpatialPolygons",
    inter="Krig",
    donnees="list",
    col="character",
    diverg = "logical", 
    diverg.zero = "numeric",
    legend = "list"
    ),
  contains="map"
)


#' S4 class for local-fitted (loess) maps
#' 
#' The \code{map.loess} class is a sub-class of the \code{map} class, for local-fitted (loess) maps. 
#' 
#' @name map.loess
#' @rdname map.loess
#' @exportClass map.loess
#' @author Joel Gombin
#' @aliases map.loess-class
#' @seealso \code{\link{map}}, \code{\link{map.choropleth}}, \code{\link{map.tps}}
#' @section Slots:
#' \describe{
#'    \item{\code{varname}:}{The name of the variable to be mapped, from \code{data}. Passed as a \code{character}.}
#'    \item{\code{sp.key}:}{The name of the ID variable from the \code{sp}.}
#'    \item{\code{data.key}:}{The name of the ID variable frome the \code{data}.}
#'    \item{\code{contours}:}{A \code{logical}, whether the contours should be drawn.}
#'    \item{\code{points}:}{A \code{logical}, whether the initial points should be drawn.}
#'    \item{\code{xrange}:}{\code{Numeric}, number of grid points on the \code{x} axis.}
#'    \item{\code{yrange}:}{\code{Numeric}, number of grid points on the \code{y} axis.}
#'    \item{\code{span}:}{A \code{numeric} which controls the degree of smoothing (see the \code{\link{loess}} function).}
#'    \item{\code{frame}:}{An object of the \code{SpatialPolygons}, with the external contour of the \code{SpatialPolygonsDataFrame}.}
#'    \item{\code{inter}:}{A \code{list} containing the pixels coordinates and estimated values.}
#'    \item{\code{df}:}{A \code{data.frame} containing the initial data.}
#'    \item{\code{palette}:}{A \code{character}, the name of the color palette to be used.}
#'    \item{\code{legend}:}{A \code{list} with the parameters of the legend. If it is an empty list, no legend will be drawn.}
#'  }

setClass(
  Class="map.loess",
  representation=representation(
    varname = "character",
    sp.key = "character",
    data.key = "character",
    contours = "logical",
    points="logical",
    xrange="numeric",
    yrange="numeric",
    span="numeric",
    frame="SpatialPolygons",
    inter="list",
    df="data.frame",
    palette = "character",
    legend = "list"
  ),
  contains="map"
)

#' S4 class for flow maps
#' 
#' The \code{map.flow} class is a sub-class of the \code{map} class, for flow maps. 
#' 
#' @name map.flow
#' @rdname map.flow
#' @exportClass map.flow
#' @author Joel Gombin
#' @aliases map.flow-class
#' @seealso \code{\link{map}}
#' @section Slots:
#' \describe{
#'    \item{\code{varname}:}{The name of the variable to be mapped, from \code{data}. Passed as a \code{character}.}
#'    \item{\code{sp.key}:}{The name of the ID variable from the \code{sp}.}
#'    \item{\code{data.orig}:}{The name of the origin ID variable from the \code{data.frame}.}
#'    \item{\code{data.dest}:}{The name of the destination ID variable from the \code{data.frame}.}
#'    \item{\code{arrow.type}:}{The name of the variable, if any, that categorizes the arrows.}
#'    \item{\code{arrow.colors}:}{The colors used to represent the arrows or the different types of arrows.}
#'    }

setClass(
  Class="map.flow",
  representation=representation(
    varname = "character",
    sp.key = "character",
    data.orig = "character",
    data.dest = "character",
    arrow.type = "character",
    arrows.colors = "character"
  ),
  contains="map"
)