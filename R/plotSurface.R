#' plotSurface
#' 
#' Plots a surface using Plotly and returns the URL
#' 
#' @param plotlyUsername Plotly online user name. Not needed if environment 
#'        variables already set. See plotly instructions.
#' @param plotlyKey Plotly online key. Not needed if environment variables 
#'        already set. See plotly instructions.
#' @param modFilePath Model file to use. The called function createRawresInput 
#'        assumes that there is an ext file with the same base file name.
#' @param paramsToCompare Parameters to compare. A vector of two parameter names
#'        following the NONMEM ext file standard names. Default is c("THETA1", 
#'        "THETA2"). Model file parameter labels will be removed.
#' @param resol Resolution on each axis. Default is 10 and will use 10^2 = 100 
#'        sets of parameter values, NONMEM runs, and ofv values to create the 
#'        plot.
#' @param ofvScaling If true OFVs are scaled to between zero and one. Default 
#'        is FALSE.
#' @param absolute Whether or not to take the absolute value of all parameter 
#'        values before constructing the input file. Limits applied to the 
#'        parameters to be plotted against may still be negative, making the 
#'        plot stretch into negative space.  Default is FALSE.
#' @param cleanLevel PsN clean script will be run on the parallel retries 
#'        folder. See psn_clean documentation. Default is 4, the highest 
#'        cleaning level.
#' @param origVals Whether or not to plot the original model final estimate as 
#'        a point in the plot.
#' @param estStatement The estimation statement to use for OFV evaluation. Remember 
#'        to include the MAXEVALS=0 option. Default is $EST METHOD=COND NOABORT 
#'        MAXEVALS=0 PRINT=9999
#' @param ... Further options to createRawresInput
#' 
#' @export


plotSurface <- function(plotlyUsername, plotlyKey, modFilePath, 
                        paramsToCompare = c("THETA1", "THETA2"), 
                        resol = 10, ofvScaling = FALSE, absolute = FALSE,
                        slurm_partition = "standard", cleanLevel = 4, 
                        plotOrigVals = FALSE, 
                        estStatement = paste("$EST METHOD=COND",
                                             "NOABORT MAXEVALS=0",
                                             "PRINT=9999"),
                        ...){
  
  require(plotly)
  Sys.setenv("plotly_username" = plotlyUsername)
  Sys.setenv("plotly_api_key" = plotlyKey)
  
  newModFileName <- prepModFile(modFilePath, 
                                estStatement = paste("$EST METHOD=COND",
                                                     "NOABORT MAXEVALS=0",
                                                     "PRINT=9999"))
  
  
  print("Creating the rawres input file")
  rawresInputList <- createRawresInput(modFilePath = modFilePath, 
                                       paramsToCompare = paramsToCompare, 
                                       resol = resol, absolute = absolute, ...)
  
  print("Running Parallel retries")
  dirName <- runParaRetries(newModFileName, rawres_input = rawresInputList[[1]], 
                            clean = 3, slurm_partition = slurm_partition)
  
  print("Parsing OFVs")
  rawresPath <- findRawres(dirName)
  rawres <- parseRawres(rawresPath, cols = c(paramsToCompare, "ofv"), 
                        skipRows = 1)

  xParamValsInput <- sort(rawresInputList[[2]])
  xParamValsOutput <- sort(unique(rawres[[paramsToCompare[1]]]))

  yParamValsInput <- sort(rawresInputList[[3]])
  yParamValsOutput <- sort(unique(rawres[[paramsToCompare[2]]]))
  
  
  # Checking if input and output parameter values are the same (NONMEM does 
  # change them sometimes)
  sapply(seq_along(xParamValsInput), function(x){
    
    if(!identical(xParamValsInput[x], xParamValsOutput[x])){
      paramMessage <- paste("Input and output values are different:\n", 
                            "Input ", x, ":", xParamValsInput[x],
                            "Output ", x, ":", xParamValsOutput[x],
                            "\nUsing input values") 
      print(paramMessage)
    }
  })
  sapply(seq_along(yParamValsInput), function(x){
    
    if(!identical(yParamValsInput[x], yParamValsOutput[x])){
      paramMessage <- paste("Input and output values are different:\n", 
                            "Input ", x, ":", yParamValsInput[x],
                            "Output ", x, ":", yParamValsOutput[x],
                            "\nUsing input values") 
      print(paramMessage)
    }
  })
  
  plotTitle <- paste0("OFV Surface for ", modFilePath, ", ", resol,
                      "x", resol, "resolution. Retries folder ", dirName)
  
  origVals <- rawresInputList[[4]]
  
  
  # If absolute is set to true the original parameter values are made positive.
  # All the values will be generated around these values. Limits may still be 
  # negative, making the plot stretch into negative space.
  if(absolute){
    origVals[paramsToCompare] <- abs(origVals[paramsToCompare])
  }
  
  print("Creating Plotly plot")
  plotlyObj <- createPlotlyObj(ofvVector = rawres[["ofv"]], 
                               xParamVals = xParamValsInput, 
                               yParamVals = yParamValsInput, 
                               origVals = origVals,
                               plotOrigVals = plotOrigVals,
                               paramsToCompare = paramsToCompare,
                               ofvScaling = ofvScaling,
                               plotTitle = plotTitle)
  
  plotly_POST(plotlyObj, fileopt = "new")

  # Clean up using the psn_clean 
  print(paste("Cleaning up", dirName, "with level =", cleanLevel))
  runPsnClean(dirName, level = cleanLevel, interact = FALSE)
  
  return(list(plotlyObj, dirName))
}