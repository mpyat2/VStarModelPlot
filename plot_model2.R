########################################################################################################
#
# based on https://sourceforge.net/p/vstar/code/HEAD/tree/trunk/script/plot_model.R (dbenn@computer.org)
#
# GUI by mpyat2@gmail.com [Maksym Pyatnytskyy, PMAK (AAVSO)]
#
# This version uses "tcltk" library directly, omitting gWidgets, which is somewhat buggy
#
########################################################################################################
# This program is distributed
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
########################################################################################################


library("grDevices")
library("tools")
library("tcltk")

vstarplot <- function()
{
  vspVersion        <- "Version 0.07"
  cat("Starting VStar model plot "); cat(vspVersion); cat("\n")

## Configuration options ###############################################################################
  plotWidth         <- 9     # plot pane, inched
  plotHeight        <- 6     # plot pane, inches
  
  pngWidhPx         <- 1500   # saved PNG image width, in pixels
  pngHeightPx       <- 1000   # saved PNG image height, in pixels
  pngResolution     <- 144
  
  defaultAxisXtitle <- "JD" 
  defaultAxisYtitle <- "Magnitude"

  dataVstarColorDefault <- "green"
  modelVstarColorDefault <- "red"

  debugOutput       <- FALSE
########################################################################################################
  
  vstrEquationDefault <- "#copy an equation from VStar and paste here\n";
  separatorNames <- c("Tab", "Comma", "Semicolon")
  
  mainWin <- NULL
  plotOptionsWin <- NULL
  
  #data holders
  dataXY <- NULL      # one series
  modelXY <- NULL     # one series
  vstarModels <- NULL # list of VStar equations

  #options
  separatorName <- tclVar(separatorNames[1])
  drawErrorBars <- tclVar(FALSE)
  showModel <- tclVar(FALSE)
  showCurves <- tclVar(FALSE)
  
  ## Plot options
  plotHeader <- ""
  tcl_plotHeader <- tclVar("")
  axisXtitle <- ""
  tcl_axisXtitle <- tclVar("")
  axisYtitle <- ""
  tcl_axisYtitle <- tclVar("")
  axisXmin <- ""
  tcl_axisXmin <- tclVar("")
  axisXmax <- ""
  tcl_axisXmax <- tclVar("")
  axisYmin <- ""
  tcl_axisYmin <- tclVar("")
  axisYmax <- ""
  tcl_axisYmax <- tclVar("")
  dataVstarColor <- NA
  dlg_dataVstarColor <- NA
  modelVstarColor <- NA
  dlg_modelVstarColor <- NA
  dataVstarMarkerFill <- TRUE
  tcl_dataVstarMarkerFill <- tclVar(dataVstarMarkerFill)
  modelVstarMarkerFill <- TRUE
  tcl_modelVstarMarkerFill <- tclVar(modelVstarMarkerFill)
 
  plotOptionsErrorLabel <- NULL

  vstarplot_envir <- environment()

  showError <- function(msg)
  {
    if (!is.null(mainWin))
      tkmessageBox(message = msg, title = "Error", icon = "error", parent = mainWin)
    else
      tkmessageBox(message = msg, title = "Error", icon = "error")
  }
  
  confirmDialog <- function(msg)
  {
    result <- ""    
    if (!is.null(mainWin))
      result <- tclvalue(tkmessageBox(title = "Confirm", message = msg, icon = "question", type = "yesno", default = "no", parent = mainWin))
    else
      result <- tclvalue(tkmessageBox(title = "Confirm", message = msg, icon = "question", type = "yesno", default = "no"))
    return(result)
  }

  confirmDialog2 <- function(msg)
  {
    result <- ""
    if (!is.null(mainWin))
      result <- tclvalue(tkmessageBox(title = "Confirm", message = msg, icon = "question", type = "yesnocancel", default = "cancel", parent = mainWin))
    else
      result <- tclvalue(tkmessageBox(title = "Confirm", message = msg, icon = "question", type = "yesnocancel", default = "cancel"))
    return(result)
  }
  
  getOpenTextFile <- function()
  {
    result <- tclvalue(tkgetOpenFile(filetypes = "{{Text files (.txt)} {*.txt}} {{All files} {*}}", parent = mainWin))
    return(result)
  }
  
  getOpenTextOrCSVFile <- function()
  {
    result <- tclvalue(tkgetOpenFile(filetypes = "{{Text, CSV files (*.txt, *.tsv, *.csv)} {.txt .tsv .csv}} {{All files} {*}}", parent = mainWin))
    return(result)
  }
  
  getSaveTextFile <- function()
  {
    result <- tclvalue(tkgetSaveFile(filetypes = "{{Text files (*.txt)} {.txt}} {{All files} {*}}", confirmoverwrite = FALSE, parent = mainWin))
    return(result)
  }
  
  getSavePngFile <- function()
  {
    result <- tclvalue(tkgetSaveFile(filetypes = "{{PNG files (*.png)} {.png}} {{All files} {*}}", confirmoverwrite = FALSE, parent = mainWin))
    return(result)
  }  

  getOpenWorkspaceFile <- function()
  {
    result <- tclvalue(tkgetOpenFile(filetypes = "{{VStarPlot files (*.vsmodelplot)} {.vsmodelplot}} {{All files} {*}}", parent = mainWin))
    return(result)
  }  
    
  getSaveWorkspaceFile <- function()
  {
    result <- tclvalue(tkgetSaveFile(filetypes = "{{VStarPlot files (*.vsmodelplot)} {.vsmodelplot}} {{All files} {*}}", confirmoverwrite = FALSE, parent = mainWin))
    return(result)
  }

  # Open plotting device with white (nontransparent) background
  openDevice = function()
  {
    if (debugOutput) { cat("openDevice\n"); cat("List of devices\n"); print(dev.list()) }
    if (length(dev.list()) < 1) 
    {
      #if (debugOutput) cat("Calling dev.new()\n")
      #dev.new(width = plotWidth, height = plotHeight, bg = "white", noRStudioGD = TRUE)
      # Calling dev.new under Windows in non-interactive mode does not initialise screen device (pdf initialized instead).
      if (debugOutput) cat("Calling X11()\n")
      X11(width = plotWidth, height = plotHeight, bg = "white") # X11() works under Windows also, it this case it is a synonym of windows()
      #tkraise(mainWin)
      tkfocus(mainWin)
    }
  }

  # "Empty" plot
  clearPlot <- function()
  {
    openDevice()
    plot(0, 0, xlab = "", ylab = "")
  }

  # Draw lightcurve, model, and model equations
  drawPlot <- function()
  {
    if (debugOutput) cat("drawPlot\n")
    # Plot only if lightcurve is defined.
    if (is.null(dataXY)) return()
    
    plotXlim <- NULL
    plotYlim <- NULL

    okFlag <- FALSE
    tryCatch({
      dataX = unlist(dataXY[1], use.names = FALSE)
      dataY = unlist(dataXY[2], use.names = FALSE)
      if ((class(dataX) != "numeric") || (class(dataY) != "numeric")) stop("X, Y: Data must be numeric")
      plotXlim <- range(dataX)
      plotYlim <- rev(range(dataY))
      okFlag <- TRUE
    }, error = function(ex) { showError(paste("Cannot extract data.", ex, sep = "\n")) })
    if (!okFlag) return()
      
    openDevice() # Open graphic device with predefined parameters
    okFlag <- FALSE
    tryCatch({
      if (is.numeric(axisXmin)) plotXlim[1] <- axisXmin
      if (is.numeric(axisXmax)) plotXlim[2] <- axisXmax
      if (is.numeric(axisYmin)) plotYlim[1] <- axisYmin
      if (is.numeric(axisYmax)) plotYlim[2] <- axisYmax
      if (dataVstarMarkerFill) plotBg <- dataVstarColor else plotBg <- NA
      xrange = plotXlim[2] - plotXlim[1]
      #Custom X axis if xrange < 5
      plot(x = dataX, y = dataY, type = "p", col = dataVstarColor, bg = plotBg, 
        pch = 21, xlab = axisXtitle, ylab = axisYtitle, xlim = plotXlim, ylim = plotYlim, main = plotHeader,
        xaxt = ifelse(xrange < 5 && xrange > 0, "n", "s"))
      if (xrange < 5 && xrange > 0) {
        roundFactor = ifelse(xrange < 1, 100.0, 10.0)
        plotXlim1round = round(plotXlim[1] * roundFactor) / roundFactor
        xstep = round(((plotXlim[2] - plotXlim1round) / 5) * roundFactor) /  roundFactor
        if (xstep > 0) {
          n_tics = round((plotXlim[2] - plotXlim1round) / xstep) + 1
          l =  plotXlim1round + xstep * (0:(n_tics-1))
          axis(1, at = l, labels = sprintf("%.2f", l))
          #print(xstep); print(n_tics); print(roundFactor); print(plotXlim, digits = 14); print(l, digits = 14)
        }
      }
      okFlag <- TRUE
    }, error = function(ex) { showError(paste("Cannot plot data.", ex, sep = "\n")) })
    if (!okFlag) return()
    
    # enabling additional controls on success
    tkconfigure(buttonSaveData, state = "normal")
    tkconfigure(buttonLoadModel, state = "normal")
    tkconfigure(buttonPlotEquation, state = "normal")
    tkconfigure(buttonLoadEquation, state = "normal")
    tkconfigure(buttonSavePlot, state = "normal")
    tkconfigure(buttonPlotOptions, state = "normal")
      
    # Errorbars
    if ((length(dataXY) > 2) && (tclvalue(drawErrorBars) != "0"))
    {
      okFlag <- FALSE
      tryCatch({ 
        dataE = unlist(dataXY[3], use.names = FALSE)
        if (class(dataE) != "numeric") stop("Uncertainties: Data must be numeric")
        okFlag <- TRUE
      }, error = function(ex) { showError(paste("Cannot extract uncertainties.", ex, sep = "\n")) })
      if (okFlag) 
        arrows(dataX, dataY - dataE, dataX, dataY + dataE, length = 0.05, angle = 90, code = 3, col = "gray")
      else
        tclvalue(drawErrorBars) <- FALSE	
    }
      
    # Model (points)
    if (!is.null(modelXY) && (tclvalue(showModel) != "0"))
    {
      okFlag <- FALSE
      tryCatch({
        dataX = unlist(modelXY[1], use.names = FALSE)
        dataY = unlist(modelXY[2], use.names = FALSE)
        if ((class(dataX) != "numeric") || (class(dataY) != "numeric")) stop("X, Y: Data must be numeric")
        okFlag <- TRUE
      }, error = function(ex) { showError(paste("Cannot extract model.", ex, sep = "\n")) })
      if (okFlag) 
      {
        if (modelVstarMarkerFill) plotBg <- modelVstarColor else plotBg <- NA
        points(dataX, dataY, col = modelVstarColor, bg = plotBg, pch = 21)
        tkconfigure(checkBoxShowModel, state = "normal")
        tkconfigure(buttonSaveModel, state = "normal")
      }
      else
        tclvalue(showModel) <- FALSE
    }
      
    # Model (curve(s))
    # There can be several curves, for example, main curve plus individual harmonics 
    if (!is.null(vstarModels) && (tclvalue(showCurves) != "0"))
    {
      jds = seq(plotXlim[1], plotXlim[2], (plotXlim[2] - plotXlim[1]) / 1000)
      for (i in 1:length(vstarModels))
      {
        # Trying to isolate an enviromment, in which user-defined models are calculated, from working environment.
        # Not an ideal solution, however it is better than non-isolated at all.
        # A "parallel" environment is a child of vstarplot() parent (usually global environment)
        okFlag <- FALSE
        temp_env <- new.env(parent = parent.env(vstarplot_envir))
        tryCatch({
          temp_env$jds <- jds 
          temp_env$vstarModel <- vstarModels[i]
          with(temp_env, 
          {
            model <- NULL
            modelColor <- "blue"
            eval(parse(text = paste(vstarModel))) # must contain function "model"
            lines(jds, model(jds), col = modelColor) # can generate exception if modelColor defined incorrectly, for example
          })
          okFlag <- TRUE
        }, error = function(ex) {showError(paste("Error while plotting model curve.", ex, sep = "\n")) },
           finally = { remove("temp_env") })
        if (!okFlag) return()
      }
    }
  }
  
  # Separator (used in data files)
  getSeparator <- function()
  {
    s <- tclvalue(separatorName)
    if (s == "Tab") return("\t")
    if (s == "Comma") return (",")
    if (s == "Semicolon") return (";")
    return("\t")
  }
  
  safeSetDrawErrorBars <- function(v)
  {
    if (length(v) == 1 && mode(v) == "logical" && !is.na(v)) tclvalue(drawErrorBars) <- v
  }
  
  color2hex <- function(v)
  {
    color_rgb <- col2rgb(v)
    result <- rgb(color_rgb[1], color_rgb[2], color_rgb[3], maxColorValue = 255)
    return(result)
  }
  
  is.valid.color <- function(v)
  {
    result <- try(col2rgb(v), silent=TRUE)
    return(!("try-error" %in% class(result)))
  }
  
  safeSetDataVstarColor <- function(v)
  {
    if (length(v) == 1 && is.valid.color(v)) dataVstarColor <<- v
  }
  
  safeSetModelVstarColor <- function(v)
  {
    if (length(v) == 1 && is.valid.color(v)) modelVstarColor <<- v
  }  
  
  safeSetDataVstarMarkerFill <- function(v)
  {
    if (length(v) == 1 && mode(v) == "logical" && !is.na(v)) dataVstarMarkerFill <<- v
  }
  
  safeSetModelVstarMarkerFill <- function(v)
  {
    if (length(v) == 1 && mode(v) == "logical" && !is.na(v)) modelVstarMarkerFill <<- v
  }  
  
  safeSetSeparator <- function(sName)
  {
    if(length(sName) == 1 && sName %in% separatorNames) tclvalue(separatorName) <- sName
  }

  getVstarEquation <- function()
  {
    result <- tclvalue(tkget(textVStarEquation, "1.0", "end"))
    if (is.null(result) || is.na(result)) result <- ""
    # tkget could return extra \n. Trim all trailing whilespaces (including \n) and add one \n.
    if (nchar(result)) result <- paste(trimws(result, which = "right"), "\n", sep = "")
    return(result)
  }
  
  setVstarEquation <- function(v)
  {
    tkdelete(textVStarEquation, "1.0", "end")
    if (!is.null(v) && !is.na(v) && length(v) > 0) tkinsert(textVStarEquation, "1.0", v[1])
  }
  
  # Load data from file
  loadDataHandler <- function() 
  {
    dataFileName <- getOpenTextOrCSVFile()
    if (nchar(dataFileName))
    {
      clearWorkspace(FALSE) # do not reset options (checkboxes etc)
      #tclvalue(drawErrorBars) <<- FALSE
      clearPlot()
      tryCatch({
        dataXY <<- read.table(dataFileName, sep=getSeparator())
      }, error = function(ex) { showError(paste("Error loading data.", ex, sep = "\n")) })
      drawPlot()
    }
  }
  
  # Load model data from file
  loadModelHandler <- function()
  {
    dataFileName <- getOpenTextOrCSVFile()
    if (nchar(dataFileName))
    {
      modelXY <<- NULL
      tkconfigure(checkBoxShowModel, state = "disabled")
      tkconfigure(buttonSaveModel, state = "disabled")
      tclvalue(showModel) <- FALSE
      tryCatch({
        modelXY <<- read.table(dataFileName, sep=getSeparator())
        tclvalue(showModel) <- TRUE
      }, error = function(ex) { showError(paste("Error loading model data.", ex, sep = "\n")) })
      drawPlot()
    }
  }
  
  # Add new equation to vstarModels vector
  addEquation <- function(s)
  {
    okFlag <- FALSE
    # Trying to isolate an enviromment, in which user-defined models are calculated, from working environment.
    temp_env <- new.env(parent = parent.env(vstarplot_envir))
    tryCatch({
      temp_env$vstarModel <- s
      with(temp_env, 
      {
        model <- NULL
        eval(parse(text = vstarModel))
        if (mode(model) != "function") stop(message = "There must be 'model' function defined in the expression: Chunk of code is ignored.")
      }) 
      okFlag <- TRUE
    }, error = function(ex) { showError(paste("Parse error: Chunk of code is ignored.\nError message:\n", ex, sep = "\n")) },
       finally = { remove("temp_env") })
    if (!okFlag) return()
    
    # simplified: do not check for identical models. length(NULL) == 0 !!
    vstarModels[length(vstarModels) + 1] <<- s
  }
  
  # There can be several equations, divided by special comment line (starts on '#$' symbols)
  # This function complelely replaces vstarModels vector 
  parseVstarEquation <- function()
  {
    vstarModels <<- NULL
    s = getVstarEquation()
    # 's' may contain several expressions separated by special comment #$
    lines = unlist(strsplit(s, "\n"))
    if (length(lines) < 1) return()
    codechunk <- ""
    for (line_n in 1:length(lines))
    {
      line = lines[line_n]
      line <- trimws(line)
      if (substr(line, 1, 2) == "#$") 
      {
        codechunk <- trimws(codechunk)
        if (nchar(codechunk)) addEquation(codechunk)
        codechunk <- ""
      }
      else
      {
        if (nchar(line))
        {
          if (substr(line, 1, 1) != "#") # ignore comments
          {
            if (codechunk == "") codechunk = line else codechunk = paste(codechunk, line, sep="\n")
          }
        }
      }
    }
    codechunk <- trimws(codechunk)
    if (nchar(codechunk)) addEquation(codechunk)
    codechunk <- ""
  }
  
  parseVstarEquationAndDraw <- function()	
  {
    parseVstarEquation()
    if (!is.null(vstarModels))
    {
      tkconfigure(checkBoxShowCurves, state = "normal")
      tclvalue(showCurves) <- TRUE 
    }
    drawPlot()
  }
  
  # Draw equations (curves)
  plotEquationHandler = function() 
  {
    parseVstarEquationAndDraw()
  }

  # Load equation from a file and plot
  loadEquationHandler <- function()
  {
    okFlag <- FALSE
    textFileName <- getOpenTextFile()
    if (nchar(textFileName))
    {
      tryCatch({
        v <- paste0(readLines(con=textFileName, warn=FALSE), collapse ="\n")
        v <- paste(v, "\n", sep = "")
        setVstarEquation(v)
        okFlag <- TRUE
      }, error = function(ex) { showError(paste("Error reading file.", ex, sep = "\n")) })
    }
    if (!okFlag) return()
    parseVstarEquationAndDraw()
  }
  
  # Save model to external file
  saveDataOrModel <- function(table)
  {
    if (!is.null(table))
    {
      dataFileName <- getSaveTextFile()
      if (nchar(dataFileName))
      {
        if (file_ext(dataFileName) == "") dataFileName = paste(dataFileName, ".txt", sep="")
        if (!file.exists(dataFileName) || (confirmDialog(paste("File", dataFileName, "exists. Overwrite?", sep="\n")) == "yes"))
          tryCatch({
            write.table(table, file=dataFileName, sep=getSeparator(), col.names=FALSE, row.names=FALSE, na="")
          }, error = function(ex) { showError(paste("Error saving file.", ex, sep = "\n")) })
      }
    }
    else
      ShowError("Nothing to save!")
  }
  
  saveDataHandler <- function()
  {
    saveDataOrModel(dataXY) 
  }
  
  saveModelHandler <- function()
  {
    saveDataOrModel(modelXY) 
  }  

  # Save plot to PNG file
  savePlotHandler <- function()
  {
    imageFileName <- getSavePngFile()
    if (nchar(imageFileName))
    {
      if (file_ext(imageFileName) == "") imageFileName = paste(imageFileName, ".png", sep="")
      if (!file.exists(imageFileName) || (confirmDialog(paste("File", imageFileName, "exists. Overwrite?", sep="\n")) == "yes"))
      {
        tryCatch({
          dev.copy(png, file = imageFileName, width = pngWidhPx, height = pngHeightPx, res = pngResolution)
          dev.off()
        }, error = function(ex) { showError(paste("Error saving file.", ex, sep = "\n")) })
      }
    }
  }

  clearWorkspace <- function(resetOptios)
  {
    dataXY <<- NULL
    modelXY <<- NULL
    vstarModels <<- NULL
    tkconfigure(buttonSaveData, state = "disabled")
    tkconfigure(buttonSaveModel, state = "disabled")
    tkconfigure(buttonLoadModel, state = "disabled")
    tkconfigure(buttonPlotEquation, state = "disabled")
    tkconfigure(buttonLoadEquation, state = "disabled")
    tkconfigure(checkBoxShowModel, state = "disabled")
    tkconfigure(checkBoxShowCurves, state = "disabled")
    tkconfigure(buttonSavePlot, state = "disabled")
    tkconfigure(buttonPlotOptions, state = "disabled")
    plotHeader <<- ""
    axisXtitle <<- defaultAxisXtitle
    axisYtitle <<- defaultAxisYtitle
    axisXmin <<- ""
    axisXmax <<- ""
    axisYmin <<- ""
    axisYmax <<- ""
    dataVstarColor <<- dataVstarColorDefault
    modelVstarColor <<- modelVstarColorDefault
    if (resetOptios)
    {
      tclvalue(drawErrorBars) <- FALSE
      tclvalue(showModel) <- TRUE
      tclvalue(showCurves) <- TRUE
      setVstarEquation(vstrEquationDefault)
      tclvalue(separatorName) <- separatorNames[1]
    }
  }
  
  # Save data, model, equations, and some options into an external file (R-format)
  saveWorkspace <- function(wsFileName)
  {
    temp_env <- new.env()
    tryCatch({
      temp_env$dataXY <- dataXY
      temp_env$modelXY <- modelXY
      temp_env$vstarEquation <- getVstarEquation()
      temp_env$separatorName <- tclvalue(separatorName)
      temp_env$plotHeader <- plotHeader
      temp_env$axisXtitle <- axisXtitle
      temp_env$axisYtitle <- axisYtitle
      temp_env$axisXmin <- axisXmin
      temp_env$axisXmax <- axisXmax
      temp_env$axisYmin <- axisYmin
      temp_env$axisYmax <- axisYmax
      temp_env$drawErrorBars <- tclvalue(drawErrorBars) != "0"
      temp_env$dataVstarColor <- dataVstarColor
      temp_env$modelVstarColor <- modelVstarColor
      temp_env$dataVstarMarkerFill <- dataVstarMarkerFill
      temp_env$modelVstarMarkerFill <- modelVstarMarkerFill
      save(file=wsFileName, envir = temp_env, list = c("dataXY", "modelXY", "vstarEquation", "separatorName", "plotHeader", "axisXtitle", "axisYtitle", "axisXmin", "axisXmax", "axisYmin", "axisYmax", "drawErrorBars", "dataVstarColor", "modelVstarColor", "dataVstarMarkerFill", "modelVstarMarkerFill"))
    },finally = {remove("temp_env")})
  }
  
  # Load data, model, equations, and some options from an external file (R-format)
  loadWorkspace <- function(wsFileName)
  {
    temp_env <- new.env()
    tryCatch({
      load(file=wsFileName, envir=temp_env)
      if(exists("dataXY", envir=temp_env)) dataXY <<- temp_env$dataXY else dataXY <<- NULL
      if(exists("modelXY", envir=temp_env)) modelXY <<- temp_env$modelXY else modelXY <<- NULL
      if(exists("vstarEquation", envir=temp_env)) setVstarEquation(temp_env$vstarEquation)
      if(exists("separatorName", envir=temp_env)) safeSetSeparator(temp_env$separatorName)
      if(exists("plotHeader", envir=temp_env)) plotHeader <<- temp_env$plotHeader else plotHeader <<- NULL
      if(exists("axisXtitle", envir=temp_env)) axisXtitle <<- temp_env$axisXtitle else axisXtitle <<- NULL
      if(exists("axisYtitle", envir=temp_env)) axisYtitle <<- temp_env$axisYtitle else axisYtitle <<- NULL
      if(exists("axisXmin", envir=temp_env)) axisXmin <<- temp_env$axisXmin else axisXmin <<- NULL
      if(exists("axisXmax", envir=temp_env)) axisXmax <<- temp_env$axisXmax else axisXmax <<- NULL
      if(exists("axisYmin", envir=temp_env)) axisYmin <<- temp_env$axisYmin else axisYmin <<- NULL
      if(exists("axisYmax", envir=temp_env)) axisYmax <<- temp_env$axisYmax else axisYmax <<- NULL
      if(exists("drawErrorBars", envir=temp_env)) safeSetDrawErrorBars(temp_env$drawErrorBars)
      if(exists("dataVstarColor", envir=temp_env)) safeSetDataVstarColor(temp_env$dataVstarColor)
      if(exists("modelVstarColor", envir=temp_env)) safeSetModelVstarColor(temp_env$modelVstarColor)
      if(exists("dataVstarMarkerFill", envir=temp_env)) safeSetDataVstarMarkerFill(temp_env$dataVstarMarkerFill)
      if(exists("modelVstarMarkerFill", envir=temp_env)) safeSetModelVstarMarkerFill(temp_env$modelVstarMarkerFill)
    },finally = {remove("temp_env")})
    if (is.null(plotHeader) || is.na(plotHeader)) plotHeader <<- ""
    if (is.null(axisXtitle) || is.na(axisXtitle)) axisXtitle <<- defaultAxisXtitle
    if (is.null(axisYtitle) || is.na(axisYtitle)) axisYtitle <<- defaultAxisYtitle
    if (is.null(axisXmin) || is.na(axisXmin)) axisXmin <<- ""
    if (is.null(axisXmax) || is.na(axisXmax)) axisXmax <<- ""
    if (is.null(axisYmin) || is.na(axisYmin)) axisYmin <<- ""
    if (is.null(axisYmax) || is.na(axisYmax)) axisYmax <<- ""
  }
  
  saveWorkspaceHandler <- function()
  {
    wsFileName <- getSaveWorkspaceFile()
    if (nchar(wsFileName))
    {
      if (file_ext(wsFileName) == "") wsFileName = paste(wsFileName, ".vsmodelplot", sep="")
      if (!file.exists(wsFileName) || (confirmDialog(paste("File", wsFileName, "exists. Overwrite?", sep="\n")) == "yes"))
      {
        tryCatch({
          saveWorkspace(wsFileName)
        }, error = function(ex) { showError(paste("Error saving workspace.", ex, sep = "\n")) })
      }
    }
  }

  loadWorkspaceHandler <- function()
  {
    wsFileName <- getOpenWorkspaceFile()
    if (nchar(wsFileName))
    {
      clearWorkspace(TRUE)
      clearPlot()
      tryCatch({
        loadWorkspace(wsFileName)
      }, error = function(ex) { showError(paste("Error loading workspace.", ex, sep = "\n")) })
      if (!is.null(dataXY))
      {
        drawPlot()        
        parseVstarEquationAndDraw()
      }
    }
  }
  
  clearWorkspaceHandler <- function()
  {
    clearWorkspace(TRUE)
    clearPlot()
  }

  plotOptionsDeleteHandler <- function()
  {
    if (!is.null(plotOptionsWin))
    {
      tkgrab.release(plotOptionsWin)
      tkdestroy(plotOptionsWin)
      plotOptionsWin <<- NULL
      plotOptionsErrorLabel <<- NULL
    }
  }

  safeConvertToInt <- function(s)
  {
    s <- trimws(s)
    if (!nchar(s)) return("")
    return(suppressWarnings(as.numeric(s))) # can be NA
  }
  
  plotOptionsUpdateHandler <- function()
  {
    tkconfigure(plotOptionsErrorLabel, text = "")
    plotHeader <<- tclvalue(tcl_plotHeader)
    axisXtitle <<- tclvalue(tcl_axisXtitle)
    axisYtitle <<- tclvalue(tcl_axisYtitle)
    axisXmin <<- tclvalue(tcl_axisXmin)
    axisXmax <<- tclvalue(tcl_axisXmax)
    axisYmin <<- tclvalue(tcl_axisYmin)
    axisYmax <<- tclvalue(tcl_axisYmax)
    #
    axisXmin <<- safeConvertToInt(axisXmin)
    axisXmax <<- safeConvertToInt(axisXmax)
    axisYmin <<- safeConvertToInt(axisYmin)
    axisYmax <<- safeConvertToInt(axisYmax)
    #
    if (is.na(axisXmin) || is.na(axisXmax) || is.na(axisYmin) || is.na(axisYmax))
    {
      tkconfigure(plotOptionsErrorLabel, text = "At least one of axes boundaries is invalid!")
      if (is.na(axisXmin)) axisXmin <<- ""
      if (is.na(axisXmax)) axisXmax <<- ""
      if (is.na(axisYmin)) axisYmin <<- ""
      if (is.na(axisYmax)) axisYmax <<- ""
    }
    #
    dataVstarColor <<- dlg_dataVstarColor
    modelVstarColor <<- dlg_modelVstarColor
    #
    dataVstarMarkerFill <<- tclvalue(tcl_dataVstarMarkerFill) != "0"
    modelVstarMarkerFill <<- tclvalue(tcl_modelVstarMarkerFill) != "0"
    #
    drawPlot()
  }

  plotOptionsDataColor <- function()
  {
    color <- color2hex(dlg_dataVstarColor) # convert color into #xxxxxx because tk_chooseColor interprets colors differently
    color <- tclvalue(.Tcl(paste("tk_chooseColor", .Tcl.args(initialcolor = color, title = "Choose a color", parent = plotOptionsWin))))
    if (nchar(color) > 0)
    {
      dlg_dataVstarColor <<- color
      plotOptionsUpdateHandler()
    }
  }
  
  plotOptionsModelColor <- function()
  {
    color <- color2hex(dlg_modelVstarColor) # convert color into #xxxxxx because tk_chooseColor interprets colors differently
    color <- tclvalue(.Tcl(paste("tk_chooseColor", .Tcl.args(initialcolor = color, title = "Choose a color", parent = plotOptionsWin))))
    if (nchar(color) > 0)
    {
      dlg_modelVstarColor <<- color
      plotOptionsUpdateHandler()
    }
  }
  
  plotOptionsDataMarkerFill <- function()
  {
    plotOptionsUpdateHandler()
  }
  
  plotOptionsModelMarkerFill <- function()
  {
    plotOptionsUpdateHandler()
  }  
  
  plotOptionsHandler <- function()
  {
    #https://wiki.tcl-lang.org/page/Modal+dialogs
    if(as.logical(tkwinfo("viewable", mainWin))) 
    {
      plotHeader -> tclvalue(tcl_plotHeader)
      plotHeader -> tclvalue(tcl_plotHeader)
      axisXtitle -> tclvalue(tcl_axisXtitle)
      axisYtitle -> tclvalue(tcl_axisYtitle)
      as.character(axisXmin) -> tclvalue(tcl_axisXmin)
      as.character(axisXmax) -> tclvalue(tcl_axisXmax)
      as.character(axisYmin) -> tclvalue(tcl_axisYmin)
      as.character(axisYmax) -> tclvalue(tcl_axisYmax)
      dataVstarColor ->> dlg_dataVstarColor
      modelVstarColor ->> dlg_modelVstarColor
      #
      plotOptionsWin <<- tktoplevel()
      tkwm.protocol(plotOptionsWin, "WM_DELETE_WINDOW", plotOptionsDeleteHandler)
      tkwm.title(plotOptionsWin, "Plot Options")
      x <- tkwinfo("x", mainWin)
      y <- tkwinfo("y", mainWin)
      tkwm.geometry(plotOptionsWin, paste("+", tclvalue(x), "+", tclvalue(y), sep=""))
      frame <- ttkframe(plotOptionsWin, padding = c(4, 4, 12, 12))
      tkpack(frame, expand = TRUE, fill = "both")
      
      label <- ttklabel(frame, text = "Title")
      tkgrid(label, row = 0, column = 0, sticky = "w")        
      entry = ttkentry(frame, textvariable = tcl_plotHeader)
      tkgrid(entry, row = 0, column = 1, columnspan = 2, sticky = "ew")

      label <- ttklabel(frame, text = "X title")
      tkgrid(label, row = 1, column = 0, sticky = "w")
      entry = ttkentry(frame, textvariable = tcl_axisXtitle)
      tkgrid(entry, row = 1, column = 1, columnspan = 2, sticky = "ew")        

      label <- ttklabel(frame, text = "Y title")
      tkgrid(label, row = 2, column = 0, sticky = "w")
      entry = ttkentry(frame, textvariable = tcl_axisYtitle)
      tkgrid(entry, row = 2, column = 1, columnspan = 2, sticky = "ew")        

      label <- ttklabel(frame, text = "X Min/Max")
      tkgrid(label, row = 3, column = 0, sticky = "w")
      entry = ttkentry(frame, textvariable = tcl_axisXmin)
      tkgrid(entry, row = 3, column = 1, sticky = "ew")
      entry = ttkentry(frame, textvariable = tcl_axisXmax)
      tkgrid(entry, row = 3, column = 2, sticky = "ew")   

      label <- ttklabel(frame, text = "Y Min/Max")
      tkgrid(label, row = 4, column = 0, sticky = "w")        
      entry = ttkentry(frame, textvariable = tcl_axisYmin)
      tkgrid(entry, row = 4, column = 1, sticky = "ew")
      entry = ttkentry(frame, textvariable = tcl_axisYmax)
      tkgrid(entry, row = 4, column = 2, sticky = "ew")                

      plotOptionsErrorLabel <<- ttklabel(frame, text = "", foreground = "red")
      tkgrid(plotOptionsErrorLabel, row = 5, column = 0, columnspan = 3, sticky = "news")                
      
      frame2 <- ttkframe(frame, padding = c(0, 0, 0, 12))
      tkgrid(frame2, row = 6, column = 2, rowspan = 2, columnspan = 2, sticky = "e")
      
      checkBox <- ttkcheckbutton(frame2, text = "Fill", variable = tcl_dataVstarMarkerFill, command = plotOptionsDataMarkerFill)
      tkgrid(checkBox, row = 0, column = 0, sticky = "e")
      
      button <- ttkbutton(frame2, text = "Data Color", command = plotOptionsDataColor)
      tkgrid(button, row = 0, column = 1, sticky = "e")
      
      checkBox <- ttkcheckbutton(frame2, text = "Fill", variable = tcl_modelVstarMarkerFill, command = plotOptionsModelMarkerFill)
      tkgrid(checkBox, row = 1, column = 0, sticky = "e")

      button <- ttkbutton(frame2, text = "Model Color", command = plotOptionsModelColor)
      tkgrid(button, row = 1, column = 1, sticky = "e")
  
      button <- ttkbutton(frame, text = "Update", command = plotOptionsUpdateHandler)
      tkgrid(button, row = 8, column = 1, sticky = "e")
      
      button <- ttkbutton(frame, text = "Close", command = plotOptionsDeleteHandler)
      tkgrid(button, row = 8, column = 2, sticky = "e")
      
      tkbind(plotOptionsWin , "<Return>" , plotOptionsUpdateHandler)
      tkbind(plotOptionsWin , "<Escape>" , plotOptionsDeleteHandler)
      
      tkgrab(plotOptionsWin)
      tkwm.transient(plotOptionsWin, mainWin)
      #tkraise(plotOptionsWin)
      tkfocus(plotOptionsWin)
      if (debugOutput) print("before tkwait.window(plotOptionsWin)")
      tkwait.window(plotOptionsWin)
      if (debugOutput) print("after tkwait.window(plotOptionsWin)")
    }
  }
 
  exitHandler <- function()
  {
    plotOptionsDeleteHandler() # just in case
    if (!interactive() || (.Platform$GUI == "RStudio"))
    {
      result <- confirmDialog("Exit application?")
      if (result == "yes")
      {
        if (exists("graphics.off")) 
        {
          if (debugOutput) cat("Calling graphics.off()\n")
          graphics.off()
          #tkmessageBox(message = "After graphics.off()")
        }
        tkdestroy(mainWin)
        mainWin <<- NULL
      }
    }
    else
    {
      result <- confirmDialog2("Quit R on exit?")
      if (result == "cancel") return()
      if (exists("graphics.off")) graphics.off()
      tkdestroy(mainWin)
      mainWin <<- NULL
      if (result == "yes") 
        quit(save = "no")
      else
        cat("\nType 'q()' to quit R.\n")
    }
  }

  addScrollbars <- function(parent, widget)
  {
    xscr <- ttkscrollbar(parent, orient = "horizontal",
                         command = function(...) tkxview(widget, ...))
    yscr <- ttkscrollbar(parent, orient = "vertical" ,
                         command = function(...) tkyview(widget, ...))
    tkconfigure(widget,
                xscrollcommand = function(...) tkset(xscr, ...),
                yscrollcommand = function(...) tkset(yscr, ...))
    tkgrid(widget, row = 0, column = 0, sticky = "news")
    tkgrid(yscr, row = 0, column = 1, sticky = "ns")
    tkgrid(xscr, row = 1, column = 0, sticky = "ew")
    tkgrid.columnconfigure(parent, 0, weight = 1)
    tkgrid.rowconfigure(parent, 0, weight = 1)    
  }

################################################################################
# Begin of the main program
################################################################################

  mainWin <- tktoplevel()
  tkwm.protocol(mainWin, "WM_DELETE_WINDOW", exitHandler)
  tkwm.title(mainWin, paste("VStar model plot", vspVersion))
  frame <- ttkframe(mainWin, padding = c(4, 4, 12, 12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  equation_frame <- ttklabelframe(frame, text = "VStar equation(s)")
  button_frame <- ttklabelframe(frame, text = "Data & Model")
  tkpack(equation_frame, expand = TRUE, side = "left", fill = "both")
  tkpack(button_frame, side = "right", fill = "both")
  
  textVStarEquation <- tktext(equation_frame, wrap = "none", height = 15, width = 60)
  addScrollbars(equation_frame, textVStarEquation)

  
  buttonLoadData <- ttkbutton(button_frame, text = "Load data")
  checkBoxShowUncertainties <- ttkcheckbutton(button_frame, text = "Show uncertainties", variable = drawErrorBars)
  buttonSaveData <- ttkbutton(button_frame, text = "Save data")
  buttonLoadModel <- ttkbutton(button_frame, text = "Load model")
  checkBoxShowModel <- ttkcheckbutton(button_frame, text = "Show model", variable = showModel)
  buttonSaveModel <- ttkbutton(button_frame, text = "Save model")
  
  radioFrame <- ttklabelframe(button_frame, text = "Column separator")
  radioColumnDelimiter1 <- ttkradiobutton(radioFrame, variable = separatorName, text = separatorNames[1], value = separatorNames[1])
  radioColumnDelimiter2 <- ttkradiobutton(radioFrame, variable = separatorName, text = separatorNames[2], value = separatorNames[2])
  radioColumnDelimiter3 <- ttkradiobutton(radioFrame, variable = separatorName, text = separatorNames[3], value = separatorNames[3])

  buttonPlotOptions <- ttkbutton(button_frame, text = "Plot options")
  buttonSavePlot <- ttkbutton(button_frame, text = "Save plot")

  buttonLoadEquation <- ttkbutton(button_frame, text = "Equation from file")
  buttonPlotEquation <- ttkbutton(button_frame, text = "Update curve(s)")
  checkBoxShowCurves <- ttkcheckbutton(button_frame, text = "Show curve(s)", variable = showCurves)
  
  buttonLoadWorkspace <- ttkbutton(button_frame, text = "Load workspace")
  buttonSaveWorkspace <- ttkbutton(button_frame, text = "Save workspace")
  buttonClearWorkspace <- ttkbutton(button_frame, text = "Clear workspace")

  buttonExit <- ttkbutton(button_frame, text = "Exit")  
  
  labelSep1 <- ttklabel(button_frame, text = "")
  labelSep2 <- ttklabel(button_frame, text = "")

  tkconfigure(buttonLoadData, command = loadDataHandler)
  tkconfigure(buttonLoadModel, command = loadModelHandler)
  tkconfigure(checkBoxShowUncertainties, command = function() { drawPlot() })
  tkconfigure(checkBoxShowModel, command = function() { drawPlot() })
  tkconfigure(buttonSaveData, command = saveDataHandler)
  tkconfigure(buttonSaveModel, command = saveModelHandler)
  tkconfigure(buttonLoadEquation, command = loadEquationHandler)
  tkconfigure(buttonPlotEquation, command = plotEquationHandler)
  tkconfigure(checkBoxShowCurves, command = function() { drawPlot() })
  tkconfigure(buttonSavePlot, command = savePlotHandler)
  tkconfigure(buttonPlotOptions, command = plotOptionsHandler)
  tkconfigure(buttonSaveWorkspace, command = saveWorkspaceHandler)
  tkconfigure(buttonLoadWorkspace, command = loadWorkspaceHandler)
  tkconfigure(buttonClearWorkspace, command = clearWorkspaceHandler)
  tkconfigure(buttonExit, command = exitHandler)
  
  tkgrid(buttonLoadData, row = 0, column = 0, sticky = "w")
  tkgrid(buttonLoadModel, row = 1, column = 0, sticky = "w")
  tkgrid(checkBoxShowUncertainties, row = 0, column = 1, sticky = "w", padx = 10)
  tkgrid(checkBoxShowModel, row = 1, column = 1, sticky = "w", padx = 10)
  tkgrid(buttonSaveData, row = 0, column = 2, sticky = "e", padx = 10)
  tkgrid(buttonSaveModel, row = 1, column = 2, sticky = "e", padx = 10)
  
  tkgrid(radioFrame, row = 3, column = 0, rowspan = 3, sticky = "w")
  tkgrid(radioColumnDelimiter1, row = 0, column = 0, sticky = "w")
  tkgrid(radioColumnDelimiter2, row = 1, column = 0, sticky = "w")
  tkgrid(radioColumnDelimiter3, row = 2, column = 0, sticky = "w")
  
  tkgrid(buttonPlotOptions, row = 3, column = 2, sticky = "e", padx = 10, pady = 10)
  tkgrid(buttonSavePlot, row = 4, column = 2, sticky = "e", padx = 10, pady = 10)
  
  tkgrid(labelSep1, row = 5, column = 0)
  
  tkgrid(buttonLoadEquation, row = 6, column = 0, sticky = "w")
  tkgrid(buttonPlotEquation, row = 7, column = 0, sticky = "w")
  tkgrid(checkBoxShowCurves, row = 7, column = 1, sticky = "w", padx = 10)
  
  tkgrid(labelSep2, row = 8, column = 0)
  
  tkgrid(buttonLoadWorkspace, row = 9, column = 0, sticky = "w")
  tkgrid(buttonSaveWorkspace, row = 10, column = 0, sticky = "w")
  tkgrid(buttonClearWorkspace, row = 10, column = 1, sticky = "w")
  tkgrid(buttonExit, row = 10, column = 2, sticky = "e", padx = 10)
  
  clearWorkspace(TRUE)
  
  if (debugOutput) print(search())
  
  #tkraise(mainWin)
  tkfocus(mainWin)
  if (!interactive()) 
  {
    cat("Rscript mode\n")
    tkwait.window(mainWin) #RScript
  }
}

vstarplot()
