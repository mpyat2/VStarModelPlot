# based on https://sourceforge.net/p/vstar/code/HEAD/tree/trunk/script/plot_model.R (dbenn@computer.org)
#
# GUI by mpyat2@gmail.com [Maksym Pyatnytskyy, PMAK (AAVSO]
#
vstarplot = function ()
{
	print("Starting VStar model plot")
	library("tools")
	library(gWidgets)
	options("guiToolkit"="tcltk")
	#options("guiToolkit"="RGtk2")

## Configuration options #######################################################
	vspVersion        <- "Version 0.03"
	
	plotWidth         <- 9		# plot pane, inched
	plotHeight        <- 6		# plot pane, inches

	pngWidhPx         <- 900	# saved PNG image width, in pixels
	pngHeightPx       <- 600	# saved PNG image height, in pixels

	defaultAxisXtitle <- "JD" 
	defaultAxisYtitle <- "Magnitude"
################################################################################

	buttonLoadDataCaption <- "Load data"
	buttonLoadModelCaption <- "Load model"
	buttonSaveDataCaption <- "Save data"
	buttonSaveModelCaption <- "Save model"
	vstrEquationDefault <- "#copy an equation from VStar and paste here\n"; 
	separatorNames <- c("Tab", "Comma", "Semicolon")
	
	mainWin <- NULL
	plotOptionsWin <- NULL
	
	#data holders
	dataXY <- NULL		# one series
	modelXY <- NULL		# one series
	vstarModels <- NULL	# list of VStar equations

	separatorName <- ""
	vstarEquation <- ""
	drawErrorBars <- TRUE
	showModel <- TRUE
	showCurves <- TRUE
	
	
	## Plot options controls
	plotHeader <- ""
	editPlotHeader <- NULL
	axisXtitle <- ""
	editAxisXtitle <- NULL
	axisYtitle <- ""
	editAxisYtitle <- NULL

	# Open plotting device with white (nontransparent) background
	openDevice = function()
	{
		if (length(dev.list()) < 1) dev.new(width=plotWidth, height=plotHeight, unit="in", bg="white")
	}

		
	# "Empty" plot
	clearPlot = function()
	{
		openDevice()
		plot(0, 0, xlab="", ylab="")
	}

		
	# Draw lightcurve, model, and model equations
	drawPlot = function()
	{
	  # Plot only if lightcurve is defined.
	  #print("dataXY")
	  #print(dataXY)
	  if (!is.null(dataXY))
		{
			okFlag = FALSE
			tryCatch({
				dataX = unlist(dataXY[1], use.names=FALSE)
				dataY = unlist(dataXY[2], use.names=FALSE)
				if ((class(dataX) != "numeric") || (class(dataY) != "numeric"))
				{
					stop("Data must be numeric")
				}
				dataXrange = range(dataX)
				plotYlim = rev(range(dataY))
				okFlag = TRUE
			}, error = function(ex) { gmessage(message=paste("Cannot extract data.", ex), parent=mainWin) })
			if (!okFlag) return()

			openDevice() # Open device with predefined parameters
			plot(x=dataX, y=dataY, col="green", xlab=axisXtitle, ylab=axisYtitle, ylim=plotYlim, main=plotHeader)
			points(dataX, dataY, col = "green", bg = "green", pch = 21)
			enabled(buttonSaveData) <- TRUE
			enabled(buttonLoadModel) <- TRUE
			enabled(buttonPlotEquation) <- TRUE
			enabled(buttonSavePlot) <- TRUE
			enabled(buttonPlotOptions) <- TRUE

			# Errorbars
			if ((length(dataXY) > 2) && getDrawErrorBars())
			{
				okFlag = FALSE
				tryCatch({ 
					dataE = unlist(dataXY[3], use.names=FALSE)
					if (class(dataE) != "numeric")
					{
						stop("Data must be numeric")
					}
					okFlag = TRUE
				}, error = function(ex) { gmessage(message=paste("Cannot extract uncertainties.", ex), parent=mainWin) })
				if (okFlag) 
				{
					arrows(dataX, dataY-dataE, dataX, dataY+dataE, length=0.05, angle=90, code=3, col="gray")
				}
				else
				{
					setDrawErrorBars(FALSE)	
				}
			}
			
			# Model (points)
			if (!is.null(modelXY) && getShowModel())
			{
				okFlag = FALSE
				tryCatch({
					dataX = unlist(modelXY[1], use.names=FALSE)
					dataY = unlist(modelXY[2], use.names=FALSE)
					if ((class(dataX) != "numeric") || (class(dataY) != "numeric"))
					{
						stop("Data must be numeric")
					}
					okFlag = TRUE
				}, error = function(ex) { gmessage(message=paste("Cannot extract model.", ex), parent=mainWin) })
				if (okFlag) 
				{
					points(dataX, dataY, col = "red", bg = "red", pch = 21)
					enabled(checkBoxShowModel) <- TRUE
					enabled(buttonSaveModel) <- TRUE
				}
				else
				{
					setShowModel(FALSE)
				}
			}

			# Model (curve(s))
			# There are can be several curves, for example, main curve plus individual harmonics 
			if (!is.null(vstarModels) && getShowCurves())
			{
				#jds = seq(dataXrange[1], dataXrange[2], 0.1)
				jds = seq(dataXrange[1], dataXrange[2], (dataXrange[2] - dataXrange[1]) / 1000)
				for (i in 1:length(vstarModels))
				{
					model <- NULL
					okFlag = FALSE
					tryCatch({
						eval(parse(text=paste(vstarModels[i]))) # must contain function "model"
						okFlag = TRUE
					}, error = function(ex) {gmessage(message=paste("Error while plotting curves.", ex), parent=mainWin) })
					if (!okFlag) return()
					lines(jds, model(jds), col="blue")
				}
			}
		}
	}		
	

	# Separator (used in data files)
	getSeparator = function()
	{
		separatorName <<- svalue(radioColumnDelimiter)
		if (separatorName == "Tab") 
			separator = "\t" 
		else 
		if (separatorName == "Comma")
			separator = ","
		else
			separator = ";"
		return(separator)
	}
	
	setSeparatorName = function(v)
	{
	  separatorName <<- v
	  if (separatorName %in% separatorNames)
	  {
	    svalue(radioColumnDelimiter) <- separatorName
	  }
	}
	
	getDrawErrorBars = function()
	{
		drawErrorBars <<- svalue(checkBoxErrorBars)
		return(drawErrorBars)
	}

	setDrawErrorBars = function(v)
	{
		drawErrorBars <<- v
		svalue(checkBoxErrorBars) <- drawErrorBars
	}

	getShowModel = function()
	{
		showModel <<- svalue(checkBoxShowModel)
		return(showModel)
	}

	setShowModel = function(v)
	{
		showModel <<- v
		svalue(checkBoxShowModel) <- showModel
	}

	getShowCurves = function()
	{
		showCurves <<- svalue(checkBoxShowCurves)
		return(showCurves)
	}

	setShowCurves = function(v)
	{
		showCurves <<- v
		svalue(checkBoxShowCurves) <- showCurves
	}

	getVstarEquation = function()
	{
		vstarEquation <<- svalue(equationText)
		if (is.null(vstarEquation) || is.na(vstarEquation)) vstarEquation <<- ""
		if (vstarEquation != "" && vstarEquation[length(vstarEquation)] != "\n") vstarEquation <<- paste(vstarEquation, "\n", sep="")
		return(vstarEquation)
	}

	setVstarEquation = function(v)
	{
		vstarEquation <<- v
		svalue(equationText) <- vstarEquation
	}
	

	# Load data or model
	loadDataHandler = function(h,...) 
	{
		dataFileName <- ""
		tryCatch({ 
			dataFileName <- gfile(filter = list("Text files" = list(patterns = c("*.txt", "*.csv")), "All files" = list(patterns = c("*")))) 
		}, error = function(ex) { })
		if (!is.null(dataFileName) && !is.na(dataFileName) && dataFileName != "")
		{
			## Read observation or model data.
			tryCatch({
				if(svalue(h$obj) == buttonLoadDataCaption)
				{
				  clearWorkspace(FALSE) # do not reset options (checkboxes etc)
				  clearPlot()
					dataXY <<- read.table(dataFileName, sep=getSeparator())
				}
				else
				if(svalue(h$obj) == buttonLoadModelCaption)
				{
					modelXY <<- NULL
					enabled(checkBoxShowModel) <- FALSE
					enabled(buttonSaveModel) <- FALSE
					setShowModel(FALSE)
					modelXY <<- read.table(dataFileName, sep=getSeparator())
					setShowModel(TRUE)
				}
			}, error = function(ex) { 
				gmessage(message=paste("Cannot read data.", ex), parent=mainWin) 
			})
			clearPlot()
			drawPlot()
		}
	}

	# Add new equation to vstarModels vector
	addEquation = function(s)
	{
	  okFlag = FALSE
	  model <- NULL
	  tryCatch({ 
	    eval(parse(text=s))
	    if (mode(model) != "function")
	    {
	      stop(message="There must be 'model' function defined in the expression: Chunk of code is ignored.")
	    }
	    okFlag = TRUE
	  }, error = function(ex) { gmessage(message=paste("Parse error: Chunk of code is ignored.\nError message:\n", ex), parent=mainWin) } )
	  if (!okFlag) return()
	  
	  if (is.null(vstarModels))
	  {
	    vstarModels[1] <<- s
	  }
	  else
	  {
	    found = FALSE
	    for (i in 1:length(vstarModels))
	    {
	      if (vstarModels[i] == s)
	      {
	        found = TRUE
	        break
	      }
	    }
	    if (!found)
	    {
	      vstarModels[length(vstarModels) + 1] <<- s
	    }
	    else
	    {
	      #
	    }
	  }
	}

	# There can be several equations, divided by special comment line (starts on '#$' symbols)
	# This function complelely replaces vstarModels vector 
	parseVstarEquation = function()
	{
	  vstarModels <<- NULL
	  s = getVstarEquation()
	  # 's' may contain several expressions divided by special comment #$
	  
	  lines = unlist(strsplit(s, "\n"))
	  if (length(lines) < 1) return()
	  codechunk <- ""
	  for (line_n in 1:length(lines))
	  {
	    line = lines[line_n]
	    trimws(line)
	    if (substr(line, 1, 2) == "#$") 
	    {
	      if (codechunk != "") addEquation(codechunk)
	      codechunk <- ""
	    }
	    else
	    {
	      if (line != "")
	      {
	        if (codechunk == "") codechunk = line else codechunk = paste(codechunk, line, sep="\n")
	      }
	    }
	  }
	  if (codechunk != "")  addEquation(codechunk)
	  codechunk <- ""
	}
	
	# Draw equations (curves)
	plotEquationHandler = function(h,...) 
	{
	  parseVstarEquation()
    if (!is.null(vstarModels))
	  {
	    enabled(checkBoxShowCurves) <- TRUE
		  setShowCurves(TRUE)
		  clearPlot()
		  drawPlot()
	  }
	}
	
	# Load equation from a file and plot
	loadEquationHandler = function(h,...)
	{
	  textFileName <- ""
	  tryCatch({ 
	    textFileName <- gfile(filter = list("Text files" = list(patterns = c("*.txt")), "All files" = list(patterns = c("*"))) ) 
	  }, error = function(ex) { })
	  if (!is.null(textFileName) && !is.na(textFileName) && textFileName != "")
	  {
	    tryCatch({
	      setVstarEquation(readLines(con=textFileName, warn=FALSE))
	    }, error = function(ex) { 
	      gmessage(message=paste("Cannot read file.", ex), parent=mainWin) 
	    })
	  }
	  parseVstarEquation()
	  if (!is.null(vstarModels))
	  {
	    enabled(checkBoxShowCurves) <- TRUE
	    setShowCurves(TRUE)
	    clearPlot()
	    drawPlot()
	  }
	}
	
	# Save data or model to external file
	saveDataHandler = function(h,...)
	{
    dataFileName <- ""
    tryCatch({
	    dataFileName <- gfile(type = "save", filter = list("Text files" = list(patterns = c("*.txt")), "All files" = list(patterns = c("*")))) 
    }, error = function(ex) { })
    if (!is.null(dataFileName) && !is.na(dataFileName) && dataFileName != "")
    {
      if (file_ext(dataFileName) == "") dataFileName = paste(dataFileName, ".txt", sep="")
      if (!file.exists(dataFileName) || (gconfirm(paste("File", dataFileName, "exists. Overwrite?", sep="\n"), parent=mainWin)))
      {
        tryCatch({
          if(svalue(h$obj) == buttonSaveDataCaption)
          {
	          if (!is.null(dataXY))
	            write.table(dataXY, file=dataFileName, sep=getSeparator(), col.names=FALSE, row.names=FALSE, na="")
            else
              gmessage(message="Nothing to save!", parent=mainWin)
          }
          else
          if(svalue(h$obj) == buttonSaveModelCaption)
          {
            if (!is.null(modelXY))
              write.table(modelXY, file=dataFileName, sep=getSeparator(), col.names=FALSE, row.names=FALSE, na="")
            else
              gmessage(message="Nothing to save!", parent=mainWin)
          }
        }, error = function(ex) { gmessage(message=paste("ERROR:", ex), parent=mainWin) } )
      }
    }
	}

	# Save plot to PNG file
	savePlotHandler = function(h,...)
	{
		imageFileName <- ""
		tryCatch({ 
			imageFileName <- gfile(type = "save", filter = list("PNG files" = list(patterns = c("*.png")), "All files" = list(patterns = c("*"))) )
		}, error = function(ex) { })
		if (!is.null(imageFileName) && !is.na(imageFileName) && imageFileName != "")
		{
			if (file_ext(imageFileName) == "") imageFileName = paste(imageFileName, ".png", sep="")
			if (!file.exists(imageFileName) || (gconfirm(paste("File", imageFileName, "exists. Overwrite?", sep="\n"), parent=mainWin)))
			{
				tryCatch({
					dev.copy(png, file=imageFileName, width=pngWidhPx, height=pngHeightPx)
					dev.off()
				}, error = function(ex) { gmessage(message=paste("ERROR:", ex), parent=mainWin) } )
			}
		}
	}
	
	# Reset all controls and clear data structures
	clearWorkspace = function(resetOptios)
	{
	  dataXY <<- NULL
	  modelXY <<- NULL
	  vstarModels <<- NULL
	  enabled(buttonSaveData) <- FALSE	  
	  enabled(buttonSaveModel) <- FALSE
	  enabled(buttonLoadModel) <- FALSE
	  enabled(buttonPlotEquation) <- FALSE
	  enabled(checkBoxShowModel) <- FALSE
	  enabled(checkBoxShowCurves) <- FALSE
	  enabled(buttonSavePlot) <- FALSE
	  enabled(buttonPlotOptions) <- FALSE
	  if (!is.null(plotOptionsWin))
	  {
	    dispose(plotOptionsWin)
	  }
	  plotHeader <<- ""
	  axisXtitle <<- defaultAxisXtitle
	  axisYtitle <<- defaultAxisYtitle
	  if (resetOptios)
	  {
	    setDrawErrorBars(TRUE)
	    setShowModel(TRUE)
	    setShowCurves(TRUE)
	    setVstarEquation(vstrEquationDefault)
	    setSeparatorName("")
	  }
	}
	
	# Save data, model, equations, and some options into an external file (R-format)
	saveWorkspace = function(wsFileName)
	{
	  getVstarEquation()
	  getSeparator()
	  #getDrawErrorBars()
	  save(file=wsFileName, list = c("dataXY", "modelXY", "vstarEquation", "separatorName", "plotHeader", "axisXtitle", "axisYtitle"))
	}
	
	# Load data, model, equations, and some options from an external file (R-format)
	loadWorkspace = function(wsFileName)
	{
	  clearWorkspace(TRUE)
	  clearPlot()
	  temp_env = new.env()
	  tryCatch({
	    load(file=wsFileName, envir=temp_env)
	    if(exists("dataXY", envir=temp_env)) dataXY <<- temp_env$dataXY else dataXY <<- NULL
	    if(exists("modelXY", envir=temp_env)) modelXY <<- temp_env$modelXY else modelXY <<- NULL
	    if(exists("vstarEquation", envir=temp_env)) vstarEquation <<- temp_env$vstarEquation else vstarEquation <<- ""
	    if(exists("separatorName", envir=temp_env)) separatorName <<- temp_env$separatorName else separatorName <<- ""
	    #if(exists("drawErrorBars", envir=temp_env)) drawErrorBars <<- temp_env$drawErrorBars else drawErrorBars <<- ""
	    if(exists("plotHeader", envir=temp_env)) plotHeader <<- temp_env$plotHeader else plotHeader <<- ""
	    if(exists("axisXtitle", envir=temp_env)) axisXtitle <<- temp_env$axisXtitle else axisXtitle <<- NULL
	    if(exists("axisYtitle", envir=temp_env)) axisYtitle <<- temp_env$axisYtitle else axisYtitle <<- NULL
    },finally = {remove("temp_env")})
	  if (is.null(vstarEquation)) vstarEquation <<- ""
	  if (is.null(separatorName)) separatorName <<- ""
	  if (is.null(plotHeader)) plotHeader <<- ""
	  if (is.null(axisXtitle)) axisXtitle <<- defaultAxisXtitle
	  if (is.null(axisYtitle)) axisYtitle <<- defaultAxisYtitle
	  #if (is.null(drawErrorBars)) drawErrorBars <<- TRUE
	}
	
	# Update controls and plot
	updateWorkspace = function()
	{
	  setSeparatorName(separatorName)
	  setVstarEquation(vstarEquation)
	  setDrawErrorBars(drawErrorBars)
	  if (!is.null(dataXY))
	  {
	    setDrawErrorBars(TRUE)
	    if (!is.null(modelXY))
	    {
	      setShowModel(TRUE)
	    }
	    if (!is.null(vstarModels))
	    {
	      enabled(checkBoxShowCurves) <- TRUE
	      setShowCurves(TRUE)
	    }
	    parseVstarEquation()
	    if (!is.null(vstarModels))
	    {
	      enabled(checkBoxShowCurves) <- TRUE
	      setShowCurves(TRUE)
	    }	          
	    clearPlot()
	    drawPlot()
	  }
	}
	
	saveWorkspaceHandler = function(h,...)
	{
	  wsFileName <- ""
	  tryCatch({ 
	    wsFileName <- gfile(type = "save", filter = list("VStarPlot files" = list(patterns = c("*.vsmodelplot")), "All files" = list(patterns = c("*"))) )
	  }, error = function(ex) { })
	  if (!is.null(wsFileName) && !is.na(wsFileName) && wsFileName != "")
	  {
	    if (file_ext(wsFileName) == "") wsFileName = paste(wsFileName, ".vsmodelplot", sep="")
	    if (!file.exists(wsFileName) || (gconfirm(paste("File", wsFileName, "exists. Overwrite?", sep="\n"), parent=mainWin)))
	    {
	      tryCatch({
	        saveWorkspace(wsFileName)
	      }, error = function(ex) { gmessage(message=paste("ERROR:", ex), parent=mainWin) } )
	    }
	  }
	}

	loadWorkspaceHandler = function(h,...)
	{
	  wsFileName <- ""
	  tryCatch({ 
	    wsFileName <- gfile(filter = list("VStarPlot files" = list(patterns = c("*.vsmodelplot")), "All files" = list(patterns = c("*"))) )
	  }, error = function(ex) { })
	  if (!is.null(wsFileName) && !is.na(wsFileName) && wsFileName != "")
	  {
      tryCatch({
	      loadWorkspace(wsFileName)
        updateWorkspace()        
      },
	    error = function(ex) { gmessage(message=paste("ERROR:", ex), parent=mainWin) }, 
      warning = function(wn) { }
      )
	  }
	}
	
	plotOptionsDispose = function(h,...)
	{
	  #print("plotOptionsWin disposing")
	  plotOptionsWin <<- NULL
	  if (!is.null(editPlotHeader))
	  {
	    plotHeader <<- svalue(editPlotHeader)
	    editPlotHeader <<- NULL
	  }
	  if (!is.null(editAxisXtitle))
	  {
	    axisXtitle <<- svalue(editAxisXtitle)
	    editAxisXtitle <<- NULL
	  }
	  if (!is.null(editAxisYtitle))
	  {
	    axisYtitle <<- svalue(editAxisYtitle)
	    editAxisYtitle <<- NULL
	  }
	}
	
	plotOptionsHandler = function(h,...)
	{
	  if (is.null(plotOptionsWin)) 
	  {
	    plotOptionsWin <<- gwindow("Plot options", parent = mainWin, handler = plotOptionsDispose, height=50, width=100, visible=FALSE)
	    layout <- glayout(container=plotOptionsWin)
	    layout[1,1] <- "Title"
	    editPlotHeader <<- gedit(plotHeader, container=layout)
	    layout[1,2] <- editPlotHeader
	    
	    layout[2,1] <- "X axis title"
	    editAxisXtitle <<- gedit(axisXtitle, container=layout)
	    layout[2,2] <- editAxisXtitle

	    layout[3,1] <- "Y axis title"
	    editAxisYtitle <<- gedit(axisYtitle, container=layout)
	    layout[3,2] <- editAxisYtitle

	    layout[4,2] <- gbutton("Update", container=layout, handler = function(h,...) 
	      { 
	        plotHeader <<-  svalue(editPlotHeader)
	        axisXtitle <<-  svalue(editAxisXtitle)
	        axisYtitle <<-  svalue(editAxisYtitle)
	        drawPlot() 
        })
      visible(plotOptionsWin) <<- TRUE
	  }
	}
	
	mainWinDispose = function(h,...)
	{
	  graphics.off()
	  print("Type 'q()' to quit R.")
	}

	exitHandler = function(h,...) 
	{
	  dispose(mainWin)
	}

################################################################################
# Begin of main program
################################################################################

  mainWin <- gwindow(paste("VStar model plot", vspVersion), visible=FALSE, handler = mainWinDispose)

	gp <- ggroup(horizontal=TRUE, container=mainWin, expand=TRUE, fill=TRUE)
	
	tmp <- gframe("VStar equation(s)", container=gp, expand=TRUE, fill=TRUE)
	equationText <- gtext(NULL, text=vstarEquation, wrap=FALSE, width=400, height=200, container=tmp, expand=TRUE, fill=TRUE)
	
	tmp <- gframe("Data & Model", container=gp, expand=FALSE, fill=TRUE)
	layout <- glayout(container=tmp, spacing=0)
	layout[1,1] <- gbutton(buttonLoadDataCaption, container=layout, handler = loadDataHandler)
	checkBoxErrorBars <- gcheckbox("Show Uncertainties", checked=drawErrorBars, container=layout, handler = function(h,...) { drawPlot() } )
	layout[1,2] <- checkBoxErrorBars
	buttonSaveData <- gbutton(buttonSaveDataCaption, container=layout, handler = saveDataHandler)
	layout[1,3] <- buttonSaveData
	
	buttonLoadModel <- gbutton(buttonLoadModelCaption, container=layout, handler = loadDataHandler)
	layout[2,1] <- buttonLoadModel
	checkBoxShowModel <- gcheckbox("Show model", checked=showModel, container=layout, handler = function(h,...) { drawPlot() } )
	layout[2,2] <- checkBoxShowModel
	buttonSaveModel <- gbutton(buttonSaveModelCaption, container=layout, handler = saveDataHandler)
	layout[2,3] <- buttonSaveModel
	

	tmp <- gframe("Column separator", container=layout)
	radioColumnDelimiter <- gradio(separatorNames, container=tmp)
	layout[3:5,1:2] <- tmp
	
	buttonPlotOptions <- gbutton("Plot options", container=layout, handler = plotOptionsHandler)
	layout[4,3] <- buttonPlotOptions
	buttonSavePlot <- gbutton("Save plot", container=layout, handler = savePlotHandler)
	layout[5,3] <- buttonSavePlot

	layout[6,1] <- ""
	buttonEquationLoad <- gbutton("Equation from file", container=layout, handler = loadEquationHandler)
	layout[7,1] <- buttonEquationLoad

	buttonPlotEquation <- gbutton("Update curve(s)", container=layout, handler = plotEquationHandler)
	layout[8,1] <- buttonPlotEquation
	checkBoxShowCurves <- gcheckbox("Show curve(s)", checked=showModel, container=layout, handler = function(h,...) { drawPlot() } )
	layout[8,2] <- checkBoxShowCurves

	layout[9,1] <- ""
	layout[10,1] <- ""

	layout[11,1] <- gbutton("Load workspace", container=layout, handler = loadWorkspaceHandler)
	layout[12,1] <- gbutton("Save workspace", container=layout, handler = saveWorkspaceHandler)
	layout[12,2] <- gbutton("Clear workspace", container=layout, handler = function(h,...) { clearWorkspace(TRUE); clearPlot() })
	layout[12,3] <- gbutton("Exit", container=layout, handler = exitHandler)

	visible(mainWin) <- TRUE
	
	clearWorkspace(TRUE)
	updateWorkspace()
}

vstarplot()

