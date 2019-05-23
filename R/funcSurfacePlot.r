library(scales)
library(grDevices)


##############   Some surface functions   ###############

# Degenerate saddle from Anandkumar and Ge, (0,0) is a third order local 
# minimum but not fourth order local minimum
# surfaceFunction <- function(x,y) x*y^2+x^2

# Wine bottle bottom: (x^2+y^2???1)^2

# Cool collection: https://www.math.upenn.edu/~kazdan/312F12/Notes/max-min-notesJan09/max-min.pdf

##############  General function  ###############

funcSurfacePlot <- function(xRange = c(-5, 5), yRange = c(-5, 5), 
                            surfaceRes = 50, contourRes = 1000, 
                            surfaceFunction = function(x, y) x^2 + y^2,
                            point = c(0,0,0), addPoint = FALSE, 
                            addXZeroLine = FALSE, addContour = TRUE,
                            addContourPoint = FALSE, ...){
  
  ### General vectors and grids
  
  # Get step length from surface resolution
  xStepLength <- diff(xRange)/surfaceRes
  yStepLength <- diff(yRange)/surfaceRes
  
  # Create x and y vectors
  x.vec <- seq(xRange[1], xRange[2], xStepLength)
  y.vec <- seq(yRange[1], yRange[2], yStepLength) 
  
  # Create a grid of xy combinations for function evaluations
  xygrid <- expand.grid(x.vec, y.vec)
  x <- xygrid$Var1
  y <- xygrid$Var2
  
  # Create vectors for high resolution contours
  xContStepLength <- diff(xRange)/contourRes
  yContStepLength <- diff(yRange)/contourRes
  x.cont.vec <- seq(xRange[1], xRange[2], xContStepLength)
  y.cont.vec <- seq(yRange[1], yRange[2], yContStepLength)
  
  
  
  out.z <- outer(x.vec, y.vec, surfaceFunction)
  
  out.cont.z <- outer(x.cont.vec, y.cont.vec, surfaceFunction)
  zlim.low <- min(out.z) - diff(range(out.z))/2
  cont.list <- contourLines(x = x.cont.vec, y = y.cont.vec, 
                            z = out.cont.z, 
                            levels = seq(-40,40,4))
  
  col.pal<-colorRampPalette(c("lightskyblue1", "white", "lightpink"))
  # colors<-col.pal(100)
  colors <- cm.colors(100, alpha = 0.8)
  # height of facets
  z.facet.center <- (out.z[-1, -1] + out.z[-1, -ncol(out.z)] + 
                       out.z[-nrow(out.z), -1] + 
                       out.z[-nrow(out.z), -ncol(out.z)])/4
  # Range of the facet center on a 100-scale (number of colors)
  z.facet.range<-cut(z.facet.center, 100)
  
  pmat <- persp(x = x.vec, y = y.vec, z = out.z, phi = 25, theta = -25,
                xlab = "", ylab = "",
                zlab = "", shade = 0.2,
                zlim = c(zlim.low, max(out.z)), expand = 1, ...)
  
  if(addContour){
    cont <- lapply(cont.list, function(a){
      
      xContVals <- a$x
      yContVals <- a$y
      zContVals <- rep(zlim.low, length(xContVals))
      
      lines(trans3d(xContVals, yContVals, zContVals, 
                    pmat = pmat), col = "grey80")
      
    })
  }
  par(new=TRUE)
  
  persp(x = x.vec, y = y.vec, z = out.z, phi = 25, theta = -25,
        xlab = "Parameter \u03B8\u2081", ylab = "Parameter \u03B8\u2082",
        zlab = "-2log(Likelihood)", shade = 0.2, col=colors[z.facet.range],
        zlim = c(zlim.low, max(out.z)), expand = 1,
        family = "serif", ...)
  
  if(addPoint){
    points(trans3d(0,0,0, pmat = pmat), pch = 16, cex = 1.4, 
           col = "black", bg = "black")
    points(trans3d(0,0,0, pmat = pmat), pch = "*", cex = 2,
           col = "white", bg = "white")
  }
  if(addXZeroLine){
    lines(trans3d(x = 0, y = y.vec, z = rep(0, length(x.vec)), 
                  pmat = pmat), col = "black", lty = 1, lwd = 2.25)
    lines(trans3d(x = 0, y = y.vec, z = rep(0, length(x.vec)), 
                  pmat = pmat), col = "gray98", lty = 2, lwd = 1.5)
  }
}



#################################################
# 
# 
# ### General vectors and grids
# xrange <- c(-5,5)
# yrange <- c(-5,5)
# xygrid <- expand.grid(seq(xrange[1], xrange[2], 0.2), 
#                       seq(yrange[1], yrange[1], 0.2))
# x <- xygrid$Var1
# y <- xygrid$Var2
# x.vec <- seq(xrange[1], xrange[2], 0.2)
# y.vec <- seq(yrange[1], yrange[2], 0.2)
# # More resolution for contours
# x.cont.vec <- seq(xrange[1], xrange[2], 0.01)
# y.cont.vec <- seq(yrange[1], yrange[2], 0.01)
# 
# 
# 
# ### Minimum
# min.func <- function(x,y) x^2 + y^2
# 
# min.out.z <- outer(x.vec, y.vec, min.func)
# 
# min.out.cont.z <- outer(x.cont.vec, y.cont.vec, min.func)
# min.zlim.low <- min(min.out.z) - diff(range(min.out.z))/2
# min.cont.list <- contourLines(x = x.cont.vec, y = y.cont.vec, 
#                               z = min.out.cont.z, 
#                               levels = seq(-40,40,4))
# 
# col.pal<-colorRampPalette(c("lightskyblue1", "white", "lightpink"))
# # colors<-col.pal(100)
# colors <- cm.colors(100, alpha = 0.8)
# # height of facets
# z.facet.center <- (min.out.z[-1, -1] + min.out.z[-1, -ncol(min.out.z)] + 
#                      min.out.z[-nrow(min.out.z), -1] + 
#                      min.out.z[-nrow(min.out.z), -ncol(min.out.z)])/4
# # Range of the facet center on a 100-scale (number of colors)
# z.facet.range<-cut(z.facet.center, 100)
# 
# 
# cairo_pdf("minimum9.pdf", width = 8, height = 8)
# 
# min.pmat <- persp(x = x.vec, y = y.vec, z = min.out.z, phi = 25, theta = -25,
#                   xlab = "", ylab = "",
#                   zlab = "", shade = 0.2, border = NULL,
#                   zlim = c(min.zlim.low, max(min.out.z)), expand = 1)
# 
# 
# min.cont <- lapply(min.cont.list, function(a){
#   
#   xContVals <- a$x
#   yContVals <- a$y
#   zContVals <- rep(min.zlim.low, length(xContVals))
#   
#   lines(trans3d(xContVals, yContVals, zContVals, 
#                 pmat = min.pmat), col = "grey80")
#   
# })
# 
# par(new=TRUE)
# 
# persp(x = x.vec, y = y.vec, z = min.out.z, phi = 25, theta = -25,
#       xlab = "Parameter \u03B8\u2081", ylab = "Parameter \u03B8\u2082",
#       zlab = "-2log(Likelihood)", shade = 0.2, border = NULL,
#       zlim = c(min.zlim.low, max(min.out.z)), expand = 1,
#       family = "serif", col=colors[z.facet.range])
# 
# points(trans3d(0,0,0, pmat = min.pmat), pch = 16, cex = 1.4, 
#        col = "black", bg = "black")
# points(trans3d(0,0,0, pmat = min.pmat), pch = "*", cex = 2,
#        col = "white", bg = "white")
# 
# dev.off()
# shell.exec("minimum9.pdf")
# 
# ###
# 
# 
# ### Saddle
# sad.func <- function(x,y) x^2 - y^2
# 
# # Special vectors for saddle point view
# sad.x.vec <- x.vec
# sad.y.vec <- 
#   sad.x.cont.vec
# sad.y.cont.vec
# 
# sad.out.z <- outer(x.vec, y.vec, sad.func)
# 
# sad.out.cont.z <- outer(x.cont.vec, y.cont.vec, sad.func)
# sad.zlim.low <- min(sad.out.z) - diff(range(sad.out.z))
# sad.cont.list <- contourLines(x = x.cont.vec, y = y.cont.vec, 
#                               z = sad.out.cont.z, 
#                               levels = seq(-40,40,4))
# 
# cairo_pdf("saddle7.pdf", width = 8, height = 8)
# # win.metafile("saddle5.wmf", width = 8, height = 8)
# # CairoPNG("saddle5.png", units = "in", width = 8, height = 8, res = 300)
# # svg("saddle5.svg", width = 8, height = 8)
# # setEPS(width = 8, height = 8)
# # postscript("saddle5.eps")
# 
# sad.pmat <- persp(x = x.vec, y = y.vec, z = sad.out.z, phi = 25, theta = -25,
#                   xlab = "", ylab = "",
#                   zlab = "", shade = 0.2, border = NULL,
#                   zlim = c(sad.zlim.low, max(sad.out.z)), expand = 1)
# 
# sad.cont <- lapply(sad.cont.list, function(a){
#   
#   xContVals <- a$x
#   yContVals <- a$y
#   zContVals <- rep(sad.zlim.low, length(xContVals))
#   
#   lines(trans3d(xContVals, yContVals, zContVals, 
#                 pmat = sad.pmat), col = "grey80")
#   
# })
# 
# par(new=TRUE)
# 
# persp(x = x.vec, y = y.vec, z = sad.out.z, phi = 25, theta = -25,
#       xlab = "Parameter \u03B8\u2081", ylab = "Parameter \u03B8\u2082",
#       zlab = "-2log(Likelihood)", shade = 0.2, border = NULL,
#       zlim = c(sad.zlim.low, max(sad.out.z)), expand = 1,
#       family = "serif")
# 
# 
# points(trans3d(0,0,0, pmat = sad.pmat), pch = 16, cex = 1.4, 
#        col = "black", bg = "black")
# points(trans3d(0,0,0, pmat = sad.pmat), pch = "*", cex = 2,
#        col = "white", bg = "white")
# 
# dev.off()
# shell.exec("saddle7.pdf")
# # shell.exec("saddle5.wmf")
# # shell.exec("saddle5.pdf")
# # shell.exec("saddle5.eps")
# 
# ###
# 
# 
# 
# 
# ### Monkey saddle
# msad.the <- (-60 * pi) / (180)
# msad.func <- function(x, y) (x*cos(msad.the)-y*sin(msad.the))^3 - 3*(x*cos(msad.the)-y*sin(msad.the))*(x*sin(msad.the)+y*cos(msad.the))^2
# # msad.df = data.frame(x = x, y = y, z = msad.func(x, y))
# # msad.loess = loess(z ~ x*y, data = msad.df, 
# #                   degree = 2, span = 0.25)
# # msad.fit = expand.grid(list(x = seq(-5, 5, 0.2), y = seq(-5, 5, 0.2)))
# # msad.z = predict(msad.loess, newdata = msad.fit)
# msad.out.z <- outer(x.vec, y.vec, msad.func)
# msad.out.cont.z <- outer(x.cont.vec, y.cont.vec, msad.func)
# 
# msad.zlim.low <- min(msad.out.z) - diff(range(msad.out.z))
# 
# msad.cont.list <- contourLines(x = x.cont.vec, y = y.cont.vec, 
#                                z = msad.out.cont.z, nlevels = 30)
# 
# 
# cairo_pdf("saddle8.pdf", width = 8, height = 8)
# 
# msad.pmat <- persp(x = x.vec, y = y.vec, z = msad.out.z, phi = 25, theta = -25,
#                    xlab = "", ylab = "",
#                    zlab = "", shade = 0.2, border = NULL,
#                    zlim = c(msad.zlim.low, max(msad.out.z)), expand = 1)
# 
# msad.cont <- lapply(msad.cont.list, function(a){
#   
#   xContVals <- a$x
#   yContVals <- a$y
#   zContVals <- rep(msad.zlim.low, length(xContVals))
#   
#   lines(trans3d(xContVals, yContVals, zContVals, 
#                 pmat = msad.pmat), col = "grey80")
#   
# })
# 
# par(new=TRUE)
# 
# persp(x = x.vec, y = y.vec, z = msad.out.z, phi = 25, theta = -25,
#       xlab = "Parameter \u03B8\u2081", ylab = "Parameter \u03B8\u2082",
#       zlab = "-2log(Likelihood)", shade = 0.2, border = NULL,
#       zlim = c(msad.zlim.low, max(msad.out.z)), expand = 1,
#       family = "serif")
# 
# points(trans3d(0,0,0, pmat = sad.pmat), pch = 16, cex = 1.4, 
#        col = "black", bg = "black")
# points(trans3d(0,0,0, pmat = sad.pmat), pch = "*", cex = 2, 
#        col = "white", bg = "white")
# 
# dev.off()
# shell.exec("saddle8.pdf")
# 
# 
# 
# 
# ### Non-def
# 
# ndef.the <- (0 * pi) / (180)
# ndef.func <- function(x,y) (x*cos(ndef.the)-y*sin(ndef.the))^2
# # Do a grid just for the solutions...
# 
# ndef.df <- data.frame(x = x, y = y, z = ndef.func(x,y))
# # Use this tho...
# ndef.out.z <- outer(x.vec, y.vec, ndef.func)
# ndef.zlim.low <- min(ndef.out.z) - diff(range(ndef.out.z))
# # ndef.loess = loess(z ~ x*y, data = ndef.df, 
# #                   degree = 2, span = 0.25)
# # ndef.fit = expand.grid(list(x = seq(-5, 5, 0.2), y = seq(-5, 5, 0.2)))
# # ndef.z = predict(ndef.loess, newdata = ndef.fit)
# 
# ndef.cont.list <- contourLines(x = x.vec, y = y.vec, z = ndef.out.z, nlevels = 20)
# 
# cairo_pdf("nondef3.pdf", width = 8, height = 8)
# 
# # surface <- persp(z = ndef.z, phi = 20, theta = -50,
# #                  xlab = "X-Parameter", ylab = "Y-Parameter",
# #                  zlab = "-2log(Likelihood)", shade = 0.4, border = NULL)
# ndef.pmat <- persp(x = x.vec, y = y.vec, z = ndef.out.z, phi = 25, theta = -25,
#                    xlab = "", ylab = "",
#                    zlab = "", shade = 0.25, border = NULL,
#                    zlim = c(ndef.zlim.low, max(ndef.out.z)), expand = 1)
# 
# ndef.cont <- lapply(ndef.cont.list, function(a){
#   # a <- ndef.cont.list[[8]]
#   xContVals <- a$x
#   yContVals <- a$y
#   zContVals <- rep(ndef.zlim.low, length(xContVals))
#   
#   lines(trans3d(xContVals, yContVals, zContVals, 
#                 pmat = ndef.pmat), col = "grey80")
#   
# })
# 
# par(new=TRUE)
# 
# persp(x = x.vec, y = y.vec, z = ndef.out.z, phi = 25, theta = -25,
#       xlab = "Parameter \u03B8\u2081", ylab = "Parameter \u03B8\u2082",
#       zlab = "-2log(Likelihood)", shade = 0.25, border = NULL,
#       zlim = c(ndef.zlim.low, max(ndef.out.z)), expand = 1,
#       family = "serif")
# 
# lines(trans3d(x = 0, y = y.vec, z = rep(0, length(x.vec)), 
#               pmat = ndef.pmat), col = "black", lty = 1, lwd = 2.25)
# lines(trans3d(x = 0, y = y.vec, z = rep(0, length(x.vec)), 
#               pmat = ndef.pmat), col = "gray98", lty = 2, lwd = 1.5)
# # lines(trans3d(x = x.vec, y = (253436056753351*x.vec)/696308811079735, 
# #               z = rep(ndef.zlim.low, length(x.vec)), pmat = ndef.pmat),
# #       col = "black", lty = 1, lwd = 2)
# # lines(trans3d(x = x.vec, y = (253436056753351*x.vec)/696308811079735, 
# #               z = rep(ndef.zlim.low, length(x.vec)), pmat = ndef.pmat),
# #       col = "gray98", lty = 2)
# 
# 
# dev.off()
# shell.exec("nondef3.pdf")
# 
# 
# 
# 
# 
# 
# library(rgl)
# f <- function(x, y) x^2 + y^2
# x <- seq(-2, 2, len=50)
# y <- seq(-2, 2, len=50)
# z <- outer(x,y,f)
# persp3d(x,y,z, col="red",alpha=0.5)
# lines <- contourLines(x, y, z)
# for (i in seq_along(lines)) {
#   x <- lines[[i]]$x
#   y <- lines[[i]]$y
#   z <- rep(lines[[i]]$level, length(x))
#   lines3d(x, y, z)
# }
# 

# install.packages("geoR")
# require(geoR)
# data(elevation)
# elevation.fit = expand.grid(list(x = seq(10, 300, 1), y = seq(10, 300, 1)))
# z = predict(elevation.loess, newdata = elevation.fit)
# elevation.loess = loess(z ~ x*y, data = elevation.df,
#                         degree = 2, span = 0.25)
# elevation.df = data.frame(x = 50 * elevation$coords[,"x"],
#                           y = 50 * elevation$coords[,"y"], z = 10 * elevation$data)
# elevation.df = data.frame(x = 50 * elevation$coords[,"x"],
#                           y = 50 * elevation$coords[,"y"], z = 10 * elevation$data)
# elevation.loess = loess(z ~ x*y, data = elevation.df,
#                         degree = 2, span = 0.25)
# elevation.fit = expand.grid(list(x = seq(10, 300, 1), y = seq(10, 300, 1)))
# z = predict(elevation.loess, newdata = elevation.fit)
# elevation.fit$Height = as.numeric(z)
# wireframe(Height ~ x*y, data = elevation.fit,
#           xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
#           main = "Surface elevation data",
#           drape = TRUE,
#           colorkey = TRUE,
#           screen = list(z = -60, x = -60)
# )
# persp(seq(10, 300, 5), seq(10, 300, 5), z, phi = 45, theta = 45,
#       xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
#       main = "Surface elevation data"
# )
# z = predict(elevation.loess, newdata = elevation.fit)
# persp(seq(10, 300, 5), seq(10, 300, 5), z, phi = 45, theta = 45,
#       xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
#       main = "Surface elevation data"
# )
# class(z)
# summary(z)
# ncol(z)
# nrow(z)
# persp(z = z, phi = 45, theta = 45,
#       xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
#       main = "Surface elevation data"
# )
