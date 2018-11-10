# based on https://sourceforge.net/p/vstar/code/HEAD/tree/trunk/script/plot_model.R (dbenn@computer.org)
#
# GUI by mpyat2@gmail.com [Maksym Pyatnytskyy, PMAK (AAVSO]
#
vstarplot = function ()
{
	print("Starting VStar plotter")
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
################################################################################

	mainWin <- NULL
	plotOptionsWin <- NULL
	
	#data holders
	dataXY <- NULL		# one series
	modelXY <- NULL		# one series
	vstarModels <- NULL	# list of VStar equations

	separatorName <- ""; 
	vstarEquation <- "#copy an equation from VStar and paste here"; 
	drawErrorBars <- TRUE
	showModel <- TRUE
	showCurves <- TRUE
	
	plotHeader <- ""
	plotHeaderEdit <- NULL

	separatorNames <- c("Tab", "Comma", "Semicolon")
	
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
			plot(x=dataX, y=dataY, col="green", xlab="JD", ylab="Magnitude", ylim=plotYlim, main=plotHeader)
			points(dataX, dataY, col = "green", bg = "green", pch = 21)
			enabled(loadModelButton) <- TRUE
			enabled(plotEquationButton) <- TRUE
			enabled(buttonSavePlot) <- TRUE

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
			dataFileName <- gfile(filter = list("Text files" = list(patterns = c("*.txt", "*.csv")), "All files" = list(patterns = c("*"))) ) 
		}, error = function(ex) { })
		if (!is.null(dataFileName) && !is.na(dataFileName) && dataFileName != "")
		{
			## Read observation or model data.
			tryCatch({
				if(svalue(h$obj) == "Load data")
				{
				  clearWorkspace(FALSE) # do not reset options (checkboxes etc)
				  clearPlot()
					dataXY <<- read.table(dataFileName, sep=getSeparator())
				}
				else
				if(svalue(h$obj) == "Load model")
				{
					modelXY <<- NULL
					enabled(checkBoxShowModel) <- FALSE
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
	  enabled(loadModelButton) <- FALSE
	  enabled(plotEquationButton) <- FALSE
	  enabled(checkBoxShowModel) <- FALSE
	  enabled(checkBoxShowCurves) <- FALSE
	  enabled(buttonSavePlot) <- FALSE
	  if (!is.null(plotOptionsWin))
	  {
	    dispose(plotOptionsWin)
	    plotHeader <<- ""
	  }
	  if (resetOptios)
	  {
	    setDrawErrorBars(TRUE)
	    setShowModel(TRUE)
	    setShowCurves(TRUE)
	    setVstarEquation("")
	    setSeparatorName("")
	  }
	}
	
	# Save data, model, equations, and some options into an external file (R-format)
	saveWorkspace = function(wsFileName)
	{
	  getVstarEquation()
	  getSeparator()
	  #getDrawErrorBars()
	  save(file=wsFileName, list = c("dataXY", "modelXY", "vstarEquation", "separatorName", "plotHeader"))
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
    },finally = {remove("temp_env")})
	  if (is.null(vstarEquation)) vstarEquation <<- ""
	  if (is.null(separatorName)) separatorName <<- ""
	  if (is.null(plotHeader)) plotHeader <<- ""
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
	    wsFileName <- gfile(type = "save", filter = list("VStarPlot files" = list(patterns = c("*.vsplotter")), "All files" = list(patterns = c("*"))) )
	  }, error = function(ex) { })
	  if (!is.null(wsFileName) && !is.na(wsFileName) && wsFileName != "")
	  {
	    if (file_ext(wsFileName) == "") wsFileName = paste(wsFileName, ".vsplotter", sep="")
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
	    wsFileName <- gfile(filter = list("VStarPlot files" = list(patterns = c("*.vsplotter")), "All files" = list(patterns = c("*"))) )
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
	  if (!is.null(plotHeaderEdit))
	  {
	    plotHeader <<- svalue(plotHeaderEdit)
	    plotHeaderEdit <<- NULL
	  }
	}
	
	plotOptionsHandler = function(h,...)
	{
	  if (is.null(plotOptionsWin)) 
	  {
	    plotOptionsWin <<- gwindow("Plot options", parent = mainWin, handler = plotOptionsDispose, height=50, width=100, visible=FALSE)
	    layout <- glayout(container=plotOptionsWin)
	    layout[1,1] <- "Title"
	    plotHeaderEdit <<- gedit(plotHeader, container=layout)
	    layout[1,2] <- plotHeaderEdit
	    layout[2,2] <- gbutton("Update", container=layout, handler = function(h,...) { plotHeader <<-  svalue(plotHeaderEdit); drawPlot() })
      visible(plotOptionsWin) <<- TRUE
	  }
	}

	exitHandler = function(h,...) 
	{
	  graphics.off()
	  dispose(mainWin)
	  print("Type 'q()' to quit R.")	  	  
	}

################################################################################
# Begin of main program
################################################################################

  mainWin <- gwindow(paste("VStar plotter", vspVersion), visible=FALSE)

	gp <- ggroup(horizontal=TRUE, container=mainWin, expand=TRUE, fill=TRUE)
	
	tmp <- gframe("VStar equation(s)", container=gp, expand=TRUE, fill=TRUE)
	equationText <- gtext(NULL, text=vstarEquation, wrap=FALSE, width=400, height=200, container=tmp, expand=TRUE, fill=TRUE)
	
	tmp <- gframe("Data & Model", container=gp, expand=FALSE, fill=TRUE)
	layout <- glayout(container=tmp, spacing=0)
	layout[1,1] <- gbutton("Load data", container=layout, handler = loadDataHandler)
	checkBoxErrorBars <- gcheckbox("Show Uncertainties", checked=drawErrorBars, container=layout, handler = function(h,...) { drawPlot() } )
	layout[1,2] <- checkBoxErrorBars
	buttonSavePlot <- gbutton("Save plot", container=layout, handler = savePlotHandler)
	layout[1,3] <- buttonSavePlot

	loadModelButton <- gbutton("Load model", container=layout, handler = loadDataHandler)
	layout[2,1] <- loadModelButton
	checkBoxShowModel <- gcheckbox("Show model", checked=showModel, container=layout, handler = function(h,...) { drawPlot() } )
	layout[2,2] <- checkBoxShowModel



	tmp <- gframe("Column separator", container=layout)
	radioColumnDelimiter <- gradio(separatorNames, container=tmp)
	layout[3,1:2] <- tmp
	

  layout[4,1] <- ""
  buttonEquationLoad <- gbutton("Equation from file", container=layout, handler = loadEquationHandler)
	layout[5,1] <- buttonEquationLoad

	plotEquationButton <- gbutton("Update curve(s)", container=layout, handler = plotEquationHandler)
	layout[6,1] <- plotEquationButton
	checkBoxShowCurves <- gcheckbox("Show curve(s)", checked=showModel, container=layout, handler = function(h,...) { drawPlot() } )
	layout[6,2] <- checkBoxShowCurves

	layout[7,1] <- ""
	layout[8,1] <- gbutton("Plot options", container=layout, handler = plotOptionsHandler)
	layout[9,1] <- ""
	layout[10,1] <- gbutton("Load workspace", container=layout, handler = loadWorkspaceHandler)
	layout[12,1] <- gbutton("Save workspace", container=layout, handler = saveWorkspaceHandler)
	layout[12,2] <- gbutton("Clear workspace", container=layout, handler = function(h,...) { clearWorkspace(TRUE); clearPlot() })
	
	layout[12,3] <- gbutton("Exit", container=layout, handler = exitHandler)

	enabled(loadModelButton) <- FALSE
	enabled(plotEquationButton) <- FALSE
	enabled(checkBoxShowModel) <- FALSE
	enabled(checkBoxShowCurves) <- FALSE
	enabled(buttonSavePlot) <- FALSE

	visible(mainWin) <- TRUE
	
	clearWorkspace(TRUE)
	updateWorkspace()
}

vstarplot()

