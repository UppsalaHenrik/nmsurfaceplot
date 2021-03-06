#' createPlotlyObj
#' 
#' @param ofvVector Vector of OFV values for the plot.
#' @param xParamVals Vector of X axis values.
#' @param yParamVals Vector of Y axis values.
#' @param origVals The original model final estimate values.
#' @param plotOrigVals Whether or not to plot the original model final estimate
#'        as a point in the plot. Default is FALSE.
#' @param paramsToCompare A pair of parameters to plot OFV against. All other 
#'        parameters will be kept at original values
#' @param ofvScaling If true OFVs are scaled to between zero and one. Default 
#'        is FALSE.
#' @param zlab Label for Z axis.
#' @param plotTitle Plot title.
#' 
#' @export
#' 



createPlotlyObj <- function(ofvVector, xParamVals, yParamVals, 
                            origVals, plotOrigVals = FALSE,
                            paramsToCompare = c("Param1", "Param2"), 
                            ofvScaling = FALSE,
                            zlab = "Z: OFV", 
                            plotTitle = "OFV Surface"){
  
  xSide <- length(xParamVals)
  xlab <- paste("X:", paramsToCompare[1])
  
  ySide <- length(yParamVals)
  ylab <- paste("Y:", paramsToCompare[2])
  
  origOfv <- origVals[[3]]
  
  # Creating labels for the original values regardless whether ofvs are scaled or not. 
  labelVector <- paste("OFV =", format(ofvVector, digits = 8))
  origOfvLabel <- paste("OFV =", format(origOfv, digits = 8))
  
  
  # ofvScaling is an option implemented to help with the number handling in plotly. 
  # Values used to form the surface cannot be differentiated between below a certain 
  # number of significant digits. This is a limitation of plotly but can be worked
  # around by using this scaling
  if(ofvScaling){
    
    # Subtract smallest number. Handling origOfv first so it uses the 
    # values before conversion of ofvVector
    origOfv <- origOfv - min(ofvVector)
    ofvVector <- ofvVector - min(ofvVector)
    
    # Calculate a scaling factor that brings the smallest non-zero number up
    ofvScalingFactor <- 10^abs(min(log10(ofvVector[ofvVector > 0])))
    
    # Scale it so that the numbers become easier to handle
    ofvVector <- ofvVector * ofvScalingFactor
    origOfv <- origOfv * ofvScalingFactor
    
    # Scale it to a fraction of the maximum
    origOfv <- origOfv/max(ofvVector)
    ofvVector <- ofvVector/max(ofvVector)
    
    # Change the z axis label
    zlab = paste0(zlab, ", scaled")
  }
  
  
  ofvMatrix <- matrix(ofvVector, 
                      nrow = ySide, 
                      ncol = xSide, 
                      byrow = TRUE)
  labelMatrix <- matrix(labelVector, 
                        nrow = ySide, 
                        ncol = xSide, 
                        byrow = TRUE)
  
  p <- plot_ly(x = xParamVals,
               y = yParamVals,
               z = ofvMatrix, 
               type = "surface",
               text = labelMatrix
  ) %>%
    layout(title = plotTitle,
           scene = list(
             xaxis = list(title = xlab),
             yaxis = list(title = ylab),
             zaxis = list(title = zlab))
    )
  
  if(plotOrigVals){
    p <- add_trace(x = origVals[[1]], 
                   y = origVals[[2]], 
                   z = origOfv, 
                   type = "scatter3d",
                   text = origOfvLabel,
                   marker = list(opacity = 0.65,
                                 color = "#FF0000",
                                 symbol = "x"
                   )
    )
  }
  
  return(p)
}
