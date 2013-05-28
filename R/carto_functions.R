

###### Methods for map objects ######


#' Create a \code{map} object
#' 
#' \code{map} creates a \code{map} object from a \code{SpatialPolygonsDataFrame} and a \code{data.frame}. This object can then be used by several functions.
#' 
#' @param sp an object of class \code{SpatialPolygonsDataFrame}.
#' @param data an object of class \code{data.frame}. 
#' @return An object of class \code{map}.
#' @export 
#' @seealso \code{\link{choropleth}}, \code{\link{Tps}}, \code{\link{loess}}, \code{\link{flow}} 
#' @author Joel Gombin


map <-function(sp, data) {
  new("map", sp=sp, data=data)
}


#' @export
#' @aliases show,map-method
#' @rdname map-class

setMethod("show", "map", function(object) {
  cat("A map object")
})

#' Create a choropleth map
#' 
#' This method allows to create a choropleth map. 
#' 
#' @param map an object of class 
#' @export
#' @docType methods
#' @rdname choropleth-methods
setGeneric("choropleth", function(map, ...) standardGeneric("choropleth"))

#' @aliases choropleth,map,ANY-method
#' @rdname choropleth-methods
setMethod(f="choropleth", 
          signature = "map",
          definition = function(map, varname, sp.key, data.key, cut.nb=6, cut.method="sd", palette="Greys", NA.white=TRUE, borders="black", order ="normal", margins=c(1,1,1,1)) {
            
            new("map.choropleth", sp=map@sp, data=map@data, varname=varname, sp.key=sp.key, data.key=data.key, cut.nb=cut.nb, cut.method=cut.method, palette=palette, NA.white=NA.white, borders=borders, order=order, margins=margins)
          })


###### Methods for map.choropleth objects ######


#' @export
#' @aliases show,map.choropleth-method
#' @rdname map.choropleth-class

setMethod("show", "map.choropleth", function(object) {
  cat("A map.choropleth object")
})


#' @export
#' @aliases initialize,map.choropleth-method
#' @rdname map.choropleth-class

setMethod(f="initialize",
          signature="map.choropleth",
          definition = function(.Object, sp, data, varname, sp.key, data.key, cut.nb, cut.method, palette, NA.white, borders, order, margins) {
  # merging datasets
  tmp <- data[, c(data.key, varname)]
  sp@data$rgrs.temp.sort.var <- 1:nrow(sp@data) # let's create an index to make sure we keep the initial order 
  sp@data <- merge(sp@data, tmp, by.x = sp.key, by.y = data.key, all.x = TRUE, all.y = FALSE, sort = FALSE)
  sp@data <- sp@data[order(sp@data$rgrs.temp.sort.var, na.last = TRUE), ]
  tmp.var <- na.omit(sp@data[, varname]) # removing NAs
  # discretization of the variable
  intervals <- classIntervals(tmp.var, cut.nb, cut.method)
  .Object@cuts <- intervals$brks
  .Object@cut.method <- attr(intervals, "style")
  value <- findInterval(sp@data[, varname], intervals$brks, all.inside = TRUE)
  
  # computing colors
  couleurs <- colorRampPalette(brewer.pal(6, palette))(length(intervals$brks)-1)
  if (order %in% "reverse") couleurs <- rev(couleurs)
  .Object@cols <- couleurs[value]
  .Object@col <- couleurs
  
  # taking care of NAs 
  na.values <- is.na(sp@data[, varname]) | is.nan(sp@data[,varname])
# bout de code à déplacer dans la méthode legend
#  na.leg <- FALSE
#  if (sum(na.values) > 0) {
#    if (NA.white==TRUE) {  
#      na.leg <- FALSE  
#    }
#    else {
#      na.leg <- TRUE
#    }
#  }
  
  
  .Object@sp <- sp
  .Object@data <- data
  .Object@varname <- varname
  .Object@sp.key <- sp.key
  .Object@data.key <- data.key
  .Object@NA.white <- NA.white
  .Object@legend <- list()
  .Object@borders <- borders
  .Object@na.values <- na.values
  .Object@palette <- palette
  .Object@order <- order
  .Object@margins <- margins
  return(.Object)
})

#' @export
#' @importFrom stats4 plot
#' @aliases plot,map.choropleth-method
#' @rdname map.choropleth-class

setMethod(
  f="plot",
  signature="map.choropleth",
  definition=function(x,y,...) {
    opar <- par(mar=x@margins)
    plot(x@sp, col = x@cols, border= x@borders, ...)
    if (sum(x@na.values) > 0) {
      if (x@NA.white==TRUE) {  
        plot(x@sp[x@na.values, ], col="transparent", border=x@borders, add = TRUE)  ## on représente les données manquantes en transparent plutôt qu'en rayé  
      }
      else {
        plot(x@sp[na.values, ], density=30, angle=45, border=x@borders, add = TRUE)
      }
    }
    if (length(x@legend) > 0) {
      plotLegend(x)
    }
    par(opar)
  }
)

#' Set map margins
#' 
#' This method allows to set margins for maps. 
#' 
#' @param map a map.
#' @param margins a vector of four numbers, the margins in number of lines. 
#' @export
#' @docType methods
#' @rdname margins-methods
setGeneric("margins", function(map, margins, ...) standardGeneric("margins"))

#' @export
#' @aliases margins,map.choropleth,numeric-method
#' @rdname margins-methods

setMethod("margins", c("map.choropleth","numeric"), function(map, margins) {
  map@margins <- margins
  return(map)
})
