
######################################################################
#### functions for package

readInputData<- function(file, sep){
	inputData<-  read.csv(file, header=TRUE, stringsAsFactors=FALSE, sep=sep ,check.names=FALSE)
	cat(paste("Have read table with",nrow(inputData),"lines and",ncol(inputData),"columns.\n"))
	return(as.data.frame(inputData))
}


addTimeScaleToData<- function(iT, fT, data){
	time<-  seq(iT, fT, length.out=nrow(data))
	data<-  cbind(time=time, data)
	cat(paste("Have added a time scale ",iT,"-",fT,"(",nrow(data),"steps )\n"))
	return(data)
}



demo <- function() {

######################################################################
#### user code

	#sorting out data

		filename<- "~/Desktop/Trinidad/LTT/fractionOfPopsize2.txt"
		separator<- "\t"
		itime<- 1920
		ftime<- 2008

		data<-  readInputData(file=filename, sep=separator)
		data<- addTimeScaleToData(iT= itime, fT= ftime, data=data)
		nDataCols<- ncol(data)-1 #how many data columns we have, excluding time

		# source("~/Desktop/Trinidad/LTT/plotStackedAreas.R")

	#examples of plotting

		#normal plot, little options
		plotStackedAreas(
			data,
			stacked100=FALSE,
			ylab="variable",
			xlab="time"
		)

		#stacked plot, little options
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time"
		)

		### Load the package or install if not present
# 		if (!require("RColorBrewer")) {
# 		  install.packages("RColorBrewer")
# 		  library(RColorBrewer)
# 		}

		#stacked plot, working with options
		#myColours= brewer.pal(8,"Set1")
    myColours= (rainbow(nDataCols, start=0.11))
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time",
			colours= myColours, #choose colours
			areaBorderCol= myColours #use area colours to hide borders
		)


		#stacked plot, working with options
		myColours= rainbow(nDataCols, start=0.11)
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time",
			colours= myColours, #choose colours
			areaBorderCol= myColours, #use area colours to hide borders
			addAxisSpace=TRUE, #add white area around?
			main="with axis margins"
		)

		#stacked plot, working with options
		myColours= rainbow(nDataCols, start=0.11)
		myOrder= sample(1:nDataCols, nDataCols) #random selection of data columns
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time",
			colours= myColours, #choose colours
			areaBorderCol= myColours, #use area colours to hide borders
			addAxisSpace=FALSE, #add white area around?
			order=myOrder, #choose order to plot data
			main="with data using random order"
		)

		#stacked plot, working with options
		myColours= rainbow(nDataCols, start=0.11)
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time",
			colours= myColours, #choose colours
			areaBorderCol= myColours, #use area colours to hide borders
			addAxisSpace=FALSE, #add white area around?
			main="playing with legends", #add title
			addLegend="topright" #add a legend to it by giving position to use
		)

		#stacked plot, working with options
		myColours= rainbow(nDataCols, start=0.11)
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time",
			colours= myColours, #choose colours
			areaBorderCol= myColours, #use area colours to hide borders
			addAxisSpace=FALSE, #add white area around?
			main="playing with legends", #add title
			addLegend="topright", #add a legend to it
			legend.cex=0.85 #edit legend: change font size
		)

		#stacked plot, working with options
		myColours= rainbow(nDataCols, start=0.11)
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time",
			colours= myColours, #choose colours
			areaBorderCol= myColours, #use area colours to hide borders
			addAxisSpace=FALSE, #add white area around?
			main="playing with legends", #add title
			boxlwd=2, #add a nice border to the entire plot
			addLegend="topright", #add a legend to it
			legend.cex=0.85, #edit legend: change font size
			legend.bg="white" #edit legend: bg colour
		)

		#stacked plot, working with options
		myColours= rainbow(nDataCols, start=0.11)
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time",
			colours= myColours, #choose colours
			areaBorderCol= myColours, #use area colours to hide borders
			addAxisSpace=FALSE, #add white area around?
			main="playing with legends", #add title
			boxlwd=2, #add a nice border to the entire plot
			addLegend="topright", #add a legend to it
			legend.cex=0.85, #edit legend: change font size
			legend.bg="white", #edit legend: bg colour
			file="myoutoutfilename", #choose to export this plot to a PDF file
			pdfW=5.5, #choose width of PDF
			pdfH=5	#choose height of PDF
		)

		#stacked plot, working with options
		myColours= rainbow(nDataCols, start=0.11)
		myColours[length(myColours)]= "white"
		plotStackedAreas(
			data,
			stacked100=TRUE,
			ylab="relative variable",
			xlab="time",
			colours= myColours, #choose colours
			areaBorderCol= myColours, #use area colours to hide borders
			addAxisSpace=FALSE, #add white area around?
			main="final example", #add title
			boxlwd=2, #add a nice border to the entire plot
			addLegend="topright", #add a legend to it
			legend.cex=0.85, #edit legend: change font size
			legend.bg="white", #edit legend: bg colour
			legend.pt.cex=1.5, #scalling of points in legend
			file="myoutoutfilename", #choose to export this plot to a PDF file
			pdfW=5.5, #choose width of PDF
			pdfH=5	#choose height of PDF
		)

}
