

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
          definition = function(map, varname, sp.key, data.key, cut.nb=6, cut.method="sd", cut.breaks, palette="Greys", alpha=1, NA.white=TRUE, borders="black", order ="normal", margins=c(5.1,4.1,4.1,2.1)) {
            
            new("map.choropleth", sp=map@sp, data=map@data, varname=varname, sp.key=sp.key, data.key=data.key, cut.nb=cut.nb, cut.method=cut.method, cut.breaks, palette=palette, alpha=alpha, NA.white=NA.white, borders=borders, order=order, margins=margins)
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
          definition = function(.Object, sp, data, varname, sp.key, data.key, cut.nb, cut.method, cut.breaks, palette, alpha, NA.white, borders, order, margins) {
  # merging datasets
  tmp <- data[, c(data.key, varname)]
  sp@data$rgrs.temp.sort.var <- 1:nrow(sp@data) # let's create an index to make sure we keep the initial order 
  sp@data <- merge(sp@data, tmp, by.x = sp.key, by.y = data.key, all.x = TRUE, all.y = FALSE, sort = FALSE)
  sp@data <- sp@data[order(sp@data$rgrs.temp.sort.var, na.last = TRUE), ]
  tmp.var <- na.omit(sp@data[, varname]) # removing NAs
  # discretization of the variable
  intervals <- classIntervals(tmp.var, cut.nb, cut.method, fixedBreaks=cut.breaks)
  .Object@cuts <- intervals$brks
  .Object@cut.method <- attr(intervals, "style")
  value <- findInterval(sp@data[, varname], intervals$brks, all.inside = TRUE)
  
  # computing colors
  # couleurs <- colorRampPalette(brewer.pal(6, palette))(length(intervals$brks)-1)
  couleurs <- paste(colorRampPalette(brewer.pal(6, palette))(length(intervals$brks)-1), as.hexmode(round(alpha*255)),sep="") # test transparence
  if (order %in% "reverse") couleurs <- rev(couleurs)
  .Object@cols <- couleurs[value]
  .Object@col <- couleurs
  
  # taking care of NAs 
  na.values <- is.na(sp@data[, varname]) | is.nan(sp@data[,varname])
# bout de code à déplacer dans la méthode legend
#  
  
  
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
      if (x@legend$hist %in% TRUE) {
        plotHistLegend(x)
      }
      else  plotLegend(x)
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

#' Create a legend
#' 
#' This method allows to add a legend to a map. 
#' 
#' @param carte an object of class map or one of its subclasses
#' @export
#' @docType methods
#' @rdname mapLegend-methods
setGeneric(
  "mapLegend",
  function(carte, posleg="bottomleft", units="", format="%.2f", main="", title="", author="", sources="", font="", ...) {
    standardGeneric("mapLegend")
  }
)

#' @export
#' @aliases mapLegend,map.choropleth-method
#' @rdname mapLegend-methods
setMethod(
  f="mapLegend",
  signature="map.choropleth",
  definition=function(carte, posleg="bottomleft", units="", format="%.2f", main="", title="", author="", sources="", font="") {
    na.leg <- FALSE
    if (sum(carte@na.values) > 0) {
      if (carte@NA.white==TRUE) {  
        na.leg <- FALSE  
        }
      else {
        na.leg <- TRUE
      }
    }
    carte@legend <- list(posleg=posleg, units=units, format=format, main=main, title=title, author=author, sources=sources, font=font, na.leg=na.leg, hist=FALSE)
    return(carte)
  }
)

#' Plot a legend
#' 
#' This method allows to plot a legend, and is for internal use mainly. 
#' 
#' @param carte an object of class map or one of its subclasses
#' @export
#' @docType methods
#' @rdname plotLegend-methods

setGeneric(
  "plotLegend",
  function(carte, ...) {
    standardGeneric("plotLegend")
  }
)

#' @export
#' @aliases plotLegend,map.choropleth-method
#' @rdname plotLegend-methods
setMethod(
  f="plotLegend",
  signature="map.choropleth",
  definition=function(carte) {
    
    par(family=carte@legend$font)
    # emplacement
    usr <- par("usr")
    width <- (usr[2]-usr[1])/20
    height <- (usr[4]-usr[3])/5
    coord <- switch(carte@legend$posleg, 
                    bottomleft = c(usr[1],usr[3],usr[1] + width, usr[3] + height),
                    left = c(usr[1], ((usr[3] + usr[4])/2)-(height/2), usr[1] + width, ((usr[3] + usr[4])/2)+(height/2)),
                    topleft = c(usr[1], usr[4]-height, usr[1] + width, usr[4]),
                    bottom = c((usr[1]+usr[2]-width)/2, usr[3], (usr[1]+usr[2]+width)/2, usr[3] + height),
                    top = c((usr[1]+usr[2]-width)/2, usr[4] - height, (usr[1]+usr[2]+width)/2, usr[4]),
                    bottomright = c(usr[2] - width,usr[3],usr[2], usr[3] + height),
                    right = c(usr[2] - width, ((usr[3] + usr[4])/2)-(height/2), usr[2], ((usr[3] + usr[4])/2)+(height/2)),
                    topright = c(usr[2] - width,usr[4] - height,usr[2], usr[4]),
                    locator = function() {
                      loc <- as.data.frame(locator(2))
                      return(c(min(loc$x),min(loc$y), max(loc$x),max(loc$y)))
                    },
                    function(x) return(x)
    )
    align <- switch(carte@legend$posleg, 
                    bottomleft = ,
                    left = ,
                    topleft = ,
                    bottom = ,
                    top = "rb",
                    bottomright = ,
                    right = ,
                    topright = "lt",
                    locator = ,
                    "rb"
    )
    
    leg.align <- switch(carte@legend$posleg, 
                        bottomleft = ,
                        left = ,
                        topleft = ,
                        bottom = ,
                        top = 0,
                        bottomright = ,
                        right = ,
                        topright = 0.75,
                        locator = ,
                        0
    )
    
    # labels
    
    if (length(carte@cuts) > 10) {
      labels <- unlist(lapply(c(min(carte@cuts), max(carte@cuts)), function(x) sprintf(paste0(carte@legend$format,"%s"), x, carte@legend$units))) 
    } else {
      labels <- unlist(lapply(1:length(carte@cuts)-1, function(x) sprintf(paste0(carte@legend$format,"%s"," - ", carte@legend$format,"%s"), carte@cuts[x], carte@legend$units, carte@cuts[x+1], carte@legend$units)))
    }
    
    #titres/auteur/sources
    
    
    
    # NA 
    
    
    color.legend(coord[1],coord[2],coord[3],coord[4],legend=labels, rect.col = carte@col, align = align, gradient="y")
    text(x=(mean(coord[1],coord[3])),y=coord[4], labels=carte@legend$title, adj=c(leg.align,-0.5))
    
  }
)

#' Adds a legend as a histogram
#' 
#' This method allows to to add a histogram-legend to a plot. 
#' 
#' @param carte an object of class map or one of its subclasses
#' @export
#' @docType methods
#' @rdname mapHistogram-methods

setGeneric(
  "mapHistogram",
  function(carte, ...) {
    standardGeneric("mapHistogram")
  }
)

#' @export
#' @aliases mapHistogram,map.choropleth-method
#' @rdname mapHistogram-methods

setMethod(
  f="mapHistogram",
  signature="map.choropleth",
  definition=function(carte, data=NULL, posleg="bottomleft", units="", format="%.2f", main="", title="", author="", sources="", font="") {
    ## inspiré par http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2760860/
    
    carte@legend <- list(posleg=posleg, units=units, format=format, main=main, title=title, author=author, sources=sources, font=font,hist=TRUE)
    return(carte)
  }
)


#' Plots a legend as a histogram
#' 
#' This method plots a histogram-legend and is for internal use mainly. 
#' @details Currently, ploHistLegend only works with pdf or png devices, not with the X11 device.
#' @param carte an object of class map or one of its subclasses
#' @export
#' @docType methods
#' @rdname plotHistLegend-methods

setGeneric(
  "plotHistLegend",
  function(carte, ...) {
    standardGeneric("plotHistLegend")
  }
)

#' @export
#' @aliases plotHistLegend,map.choropleth-method
#' @rdname plotHistLegend-methods

setMethod(
  f="plotHistLegend",
  signature="map.choropleth",
  definition=function(carte) {
    
    # emplacement
    
    
    coord <- switch(carte@legend$posleg, 
                    bottomleft = c(0.05,0.30,0,0.30),
                    left = c(0.05,0.30,0.35,0.65),
                    topleft = c(0.05,0.30,0.70,1),
                    bottom = c(0.375,0.625,0,0.30),
                    top = c(0.375,0.625,0.70,1),
                    bottomright = c(0.70,0.95,0,0.30),
                    right = c(0.70,0.95,0.35,0.65),
                    topright = c(0.70,0.95,0.70,1),
                    locator = function() {
                      loc <- as.data.frame(locator(2))
                      return(c(min(loc$x), max(loc$x), min(loc$y), max(loc$y)))
                    },
                    function(x) return(x)
    )
    
    # labels
    labels <- unlist(lapply(1:length(carte@cuts), function(x) sprintf(paste0(carte@legend$format,"%s"), carte@cuts[x], carte@legend$units)))
    
    
    # calcul des fréquences cumulées
    
    fn <- ecdf(carte@data[,carte@varname])
    
    oldpar <- par()
    par(fig=coord,new=TRUE,mar=c(4,2.5,1,1)+0.1, lab=c(length(carte@cuts), length(carte@cuts), 7),las=1,family=carte@legend$font,cex=1)
    
    Ecdf(carte@data[,carte@varname], axes=FALSE,xlab="",subtitle=FALSE,ylab="",main=carte@legend$main,xlim=range(carte@cuts))    
    invisible(lapply(2:length(carte@cuts), function(x) rect(carte@cuts[x-1], -0.1,carte@cuts[x], 0, col=carte@col[x-1], lwd=0.5,xpd=TRUE)))
    invisible(lapply(2:length(carte@cuts), function(x) {lines(x=rep(carte@cuts[x],2),y=c(0,fn(carte@cuts[x])),lwd=0.5) ; lines(x=c(carte@cuts[1],carte@cuts[x]),y=rep(fn(carte@cuts[x]),2),lwd=0.5)}))
    text(carte@cuts, -0.15,srt=45,adj=1,labels=labels,xpd=TRUE, cex=0.5,lwd=0.5)
    mtext(1, text=carte@legend$title,line=2,cex=0.6)
    axis(side=2, at=c(0,fn(carte@cuts)[-1]), labels=round(c(0,fn(carte@cuts)[-1]),2),cex.axis=0.5,hadj=1,tcl=-0.2,lwd=0.5,line=0,pos=min(carte@cuts))
    mtext(2, text="Fréquence cumulée", line=2.5, las=0, adj=c(0.5,0.5),cex=0.6)
    
    
    par(oldpar)
    
    
    
    #titres/auteur/sources
    
    
    
    # NA 
    
    
    
    
    
    
  }
)
