

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

####### à reprendre

#' @export
#' @aliases initialize,map.loess-method
#' @rdname map.loess-class


setMethod(
  f="initialize",
  signature="map.loess",
  definition=function(.Object, sp, data, varname, sp.key, data.key,contours,points,xrange,yrange,span,frame,palette, order) {
    sp@data$n <- 1:nrow(sp@data)
    tmp <- merge(sp@data, data, by.x = sp.key, by.y=data.key, all.x=T, all.y=F)
    sp@data <- tmp[order(tmp$n,na.last=TRUE),]
    sp <- sp[!is.na(sp@data[,varname]),]
    coord <- coordinates(sp)
    df <- data.frame(z=rep(0,dim(coord)[1]))
    df$z <- as.numeric(sp@data[,varname]) #on récupère les valeurs de z
    df$x <- coord[,1]
    df$y <- coord[,2]
    b <- bbox(frame)
    inter <- loess(z ~ x + y + x*y, df, model=T, normalize = F, span = span, control=loess.control(surface="direct", statistics="approximate", trace.hat="approximate"))
    x <- rep(seq(b[1,1], b[1,2], length=xrange), times = yrange)
    y <- rep(seq(b[2,1], b[2,2], length=yrange), each = xrange)
    spInter <- SpatialPoints(data.frame(x,y), proj4string=CRS(proj4string(sp))) #on crée une grille pour éliminer les points extrapolés en dehors du cadre
    z <- predict(inter,coordinates(spInter))
    contour <- over(spInter, frame) #la fonction qui évalue
    dim(contour) <- c(xrange, yrange) #on transforme le vecteur en matrice
    dim(z) <- c(xrange, yrange)
    z[is.na(contour)]<-NA #on met des NA dans le résultat de SP pour les pixels en dehors du polygone
    inter <- list()
    inter$x <- seq(b[1,1], b[1,2], length=xrange)
    inter$y <- seq(b[2,1], b[2,2], length=yrange)
    inter$z <- z
    
    .Object@sp <- sp
    .Object@data <- data
    .Object@varname <- varname
    .Object@sp.key <- sp.key
    .Object@data.key <- data.key
    .Object@contours <- contours
    .Object@points <- points
    .Object@xrange <- xrange
    .Object@yrange <- yrange
    .Object@frame <- frame
    .Object@inter <- inter
    .Object@df <- df
    .Object@palette <- palette
    .Object@legend <- list()
    .Object@order <- order
    return(.Object)
  }
)

#' Create a smoothed map (loess method)
#' 
#' This method allows to create a map with the spatially-smoothed representation of a variable. 
#' 
#' @param map a map.
#' @export
#' @docType methods
#' @rdname map.loess-methods

setGeneric(
  name="map.loess",
  def=function(x, ...) {standardGeneric("map.loess")}
)

#' @export
#' @aliases show,map.choropleth-method
#' @rdname map.choropleth-class
setMethod(
  f="map.loess",
  signature="map.choropleth",
  definition=function(x, contours = FALSE,
                      points=FALSE,
                      xrange=40,
                      yrange=40,
                      span=0.75,
                      frame=NA,
                      palette="RdBu"
  ) 
  {
    x@sp@data <- x@sp@data[,-match(x@varname,names(x@sp@data))]
    if (!(class(frame) %in% c("SpatialPolygons","SpatialPolygonsDataFrame"))) {
      gpclibPermit()
      frame <- unionSpatialPolygons(x@sp, rep(1, length(x@sp)), avoidGEOS=T)
    }
    new(Class="choropleth.loess", sp=x@sp, data=x@data, varname=x@varname, sp.key=x@sp.key, data.key=x@data.key, order=x@order, contours=contours,points=points,xrange=xrange,yrange=yrange,span=span,frame=frame,palette=palette)
  }
)


#' @export
#' @aliases show,map.loess-method
#' @rdname map.loess-class

setMethod(
  f="plot",
  signature="map.loess",
  definition=function(x,y,...) {
    colors <- brewer.pal(6, x@palette) #couleur qu'on veut pour sa palette. Palette séquentielle ou divergente
    if (x@order %in% "reverse") colors <- rev(colors)
    pal <- colorRampPalette(colors)
    pal.n <- pal(99)
    image(x@inter, asp=1, xaxt="n", yaxt="n", bty="n", col=pal.n, useRaster=TRUE) #représente la grille interpolée (sans axes ni boîte)
    plot(x@frame, add=TRUE) #on représente aussi le polygone extérieur
    if (x@contours) contour(x@inter, add=T) #les contours des courbes
    if (x@points) points(x=x@df$x,y=x@df$y) #les points de mesure initiaux
    # carte.prop.legende2(at=seq(min(x@inter$z, na.rm=T), max(x@inter$z, na.rm=T),length.out=100), palette=pal.n, na.leg=F, posleg=x@posleg, unit = x@unit)
    if (length(x@legend) > 0) plotLegend(x)
  }
)


# TODO : créer un objet à partir de l'inset et distinguer sa représentation 
#' Create insets of some subparts of the map
#' 
#' This method allows to create small maps (insets) of some subparts of the main map  
#' 
#' @param x a map.
#' @param IDs List of vectors of the IDs of the polygons to be represented in each inset.
#' @param coords Either a list of lists of the coordinates where the insets should be, or "locator" to interactively select the coordinates.
#' @export
#' @docType methods
#' @rdname inset-methods
setGeneric(
  name="inset",
  def=function(x, IDs, coords, ...) {standardGeneric("inset")}
)

#' @export
#' @aliases show,inset-method
#' @rdname inset-methods
setMethod(
  f="inset",
  signature="map.choropleth",
  definition=function(x, IDs, coords) {
    # IDs est une liste de vecteurs d'identifiants. Chaque vecteur est un inset
    # coords prend soit la valeur "locator", soit est une liste de liste de coordonnées
    
    coordsOK <- list()
    if (!(coords %in% "locator")) {
      coordsOK <- coords
      c <- as.data.frame(coordsOK)
      b <- as.data.frame(t(bbox(x@sp)))
    } else {
      c <- b <- as.data.frame(t(bbox(x@sp)))
      coordsOK <- list()
    }
    plot(x, xlim=c(min(c$x,b$x)-0.05*(max(c$x,b$x)-min(c$x,b$x)), max(c$x,b$x)+0.05*(max(c$x,b$x)-min(c$x,b$x))), ylim=c(min(b$y,c$y)-0.05*(max(c$y,b$y)-min(c$y,b$y)),max(b$y,c$y)+0.05*(max(c$y,b$y)-min(c$y,b$y))))
    inset.n <- list()
    for (i in 1:length(IDs)) {
      sub <- x@sp[x@sp@data[,x@sp.key] %in% IDs[[i]],]
      b1 <- bbox(sub)
      rect(xleft=b1["x","min"],ybottom=b1["y","min"],xright=b1["x","max"],ytop=b1["y","max"],border="grey")
      if (coords %in% "locator") {
        cat(paste0("Coordinates for inset no. ",i))
        coordsOK[[i]] <- locator(2) 
      }
      coordsOK[[i]] <- as.data.frame(coordsOK[[i]])
      if ((b1["x","max"]-b1["x","min"])/(b1["y","max"]-b1["y","min"]) < (max(coordsOK[[i]]$x)-min(coordsOK[[i]]$x))/(max(coordsOK[[i]]$y)-min(coordsOK[[i]]$y))) {
        coordsOK[[i]][which.max(coordsOK[[i]]$x),"x"] <- coordsOK[[i]][which.min(coordsOK[[i]]$x),"x"] + (b1["x","max"]-b1["x","min"])/(b1["y","max"]-b1["y","min"]) * (max(coordsOK[[i]]$y)-min(coordsOK[[i]]$y))
      } else {
        coordsOK[[i]][which.max(coordsOK[[i]]$y),"y"] <- coordsOK[[i]][which.min(coordsOK[[i]]$y),"y"] + (b1["y","max"]-b1["y","min"])/(b1["x","max"]-b1["x","min"]) * (max(coordsOK[[i]]$x)-min(coordsOK[[i]]$x))
      }
      fx <- (b1["x","max"] - b1["x","min"]) / (max(coordsOK[[i]]$x)-min(coordsOK[[i]]$x))
      fy <- (b1["y","max"] - b1["y","min"]) / (max(coordsOK[[i]]$y)-min(coordsOK[[i]]$y))
      sub.df <- fortify(sub)
      sub.df$long <- min(coordsOK[[i]]$x) + (sub.df$long - b1["x","min"]) / fx
      sub.df$lat <- min(coordsOK[[i]]$y) + (sub.df$lat - b1["y","min"]) / fy
      p1 <- function(df) Polygon(df[,c("long","lat")])
      p2 <- function(l,id) Polygons(list(l),id)
      p3 <- function(l) SpatialPolygons(l)
      sub@polygons <- slot(p3(dlply(sub.df,"id",.fun=function(x) p2(p1(x[,c("long","lat")]),unique(x$id)))),"polygons")
      sub@data <- sub@data[,-match(x@varname,names(sub@data))]
      inset.n[[i]] <- choropleth(map(sub, x@data), varname=x@varname,sp.key=x@sp.key,data.key=x@data.key, cut.nb=length(x@cuts), cut.method=x@cut.method, palette=x@palette, borders=x@borders, NA.white=x@NA.white, order=x@order)
      inset.n[[i]]@cuts <- x@cuts
      inset.n[[i]]@col <- x@col
      inset.n[[i]]@cols <- x@cols[x@sp@data[,x@sp.key] %in% IDs[[i]]]
      plot(inset.n[[i]],add=TRUE)
      xmin <- min(coordsOK[[i]]$x)-(0.05*(max(coordsOK[[i]]$x)-min(coordsOK[[i]]$x)))
      ymin <- min(coordsOK[[i]]$y)-(0.05*(max(coordsOK[[i]]$y)-min(coordsOK[[i]]$y)))
      xmax <- max(coordsOK[[i]]$x)+(0.05*(max(coordsOK[[i]]$x)-min(coordsOK[[i]]$x)))
      ymax <- max(coordsOK[[i]]$y)+(0.05*(max(coordsOK[[i]]$x)-min(coordsOK[[i]]$x)))
      rect(xmin, ymin, xmax, ymax,border="grey")
      if ((xmax < b1["x","min"]) & (ymax < b1["y","min"])) {
        lines(x=c(xmin,b1["x","min"]), y=c(ymax,b1["y","max"]),col="grey")
        lines(x=c(xmax,b1["x","max"]), y=c(ymin,b1["y","min"]), col="grey")
      }
      if ((xmin < b1["x","min"]) & (xmax > b1["x","max"]) & (ymax < b1["y","min"])) {
        lines(x=c(xmin,b1["x","min"]), y=c(ymax,b1["y","min"]),col="grey")
        lines(x=c(xmax,b1["x","max"]), y=c(ymax,b1["y","min"]), col="grey")
      }
      if ((xmax < b1["x","min"]) & (((ymax > b1["y","min"]) & (ymax < b1["y","max"])) | ((ymin > b1["y","min"]) & (ymin < b1["y","max"])))) {
        lines(x=c(xmax,b1["x","min"]), y=c(ymax,b1["y","max"]),col="grey")
        lines(x=c(xmax,b1["x","min"]), y=c(ymin,b1["y","min"]), col="grey")
      }
      if ((xmax < b1["x","min"]) & (ymin > b1["y","max"])) {
        lines(x=c(xmin,b1["x","min"]), y=c(ymin,b1["y","min"]),col="grey")
        lines(x=c(xmax,b1["x","max"]), y=c(ymax,b1["y","max"]), col="grey")
      }
      if ((xmin > b1["x","max"]) & (ymin > b1["y","max"])) {
        lines(x=c(xmin,b1["x","max"]), y=c(ymin,b1["y","max"]),col="grey")
        lines(x=c(xmax,b1["x","max"]), y=c(ymin,b1["y","min"]), col="grey")
      }
      if ((xmin > b1["x","max"]) & (ymax > b1["y","max"]) & (ymin < b1["y","max"]) & (ymin > b1["y","min"])) {
        lines(x=c(xmin,b1["x","max"]), y=c(ymax,b1["y","max"]),col="grey")
        lines(x=c(xmin,b1["x","max"]), y=c(ymin,b1["y","min"]), col="grey")
      }
      if ((xmin > b1["x","max"]) & (ymin < b1["y","min"]) & ymax < b1["y","max"]) {
        lines(x=c(xmin,b1["x","min"]), y=c(ymin,b1["y","min"]),col="grey")
        lines(x=c(xmax,b1["x","max"]), y=c(ymax,b1["y","max"]), col="grey")
      }
      if ((xmin > b1["x","max"]) & (ymin < b1["y","min"]) & ymax > b1["y","max"]) {
        lines(x=c(xmin,b1["x","max"]), y=c(ymin,b1["y","min"]),col="grey")
        lines(x=c(xmin,b1["x","max"]), y=c(ymax,b1["y","max"]), col="grey")
      }
      if ((xmax < b1["x","min"]) & (xmin < b1["x","min"]) & (ymin < b1["y","min"]) & (ymax > b1["y","max"])) {
        lines(x=c(xmax,b1["x","min"]), y=c(ymin,b1["y","min"]), col="grey")
        lines(x=c(xmax,b1["x","min"]), y=c(ymax,b1["y","max"]), col="grey")
      }
    }
  }
)
