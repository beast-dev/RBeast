
.onLoad <- function(libname, pkgname) {
	rJava::.jpackage(pkgname, lib.loc = libname)
}


#' @title
#' runBeast
#'
#' @description
#' \code{runBeast} spins up BEAST
#'
#' @details
#' This function executes BEAST through rJava.
#'
#' @param commandLine Command-line string to pass to BEAST
#'
#' @examples
#' runBeast()
#'
#' @export
runBeast <- function(commandLine = "") {

	strings <- rJava::.jarray(commandLine)
	rJava::J("dr.app.beast.RBeastMain")$main(strings)
}

plotStackedAreas <- function(
	data,
	stacked100=TRUE,
	boxlwd=1.5,
	order = NA,
	ylab="", xlab="",
	areaBorderWidth= NA,
	areaBorderCol="black",
	colours= NA,
	addAxisSpace=FALSE,
	addLegend=NA,
	legend.cex=1,
	legend.bg=NA,
	main="",
	file=NA,
	pdfW=7,
	pdfH=5,
	legend.pt.cex=1,
	...
){

	#extract time scale
	X= data$time
	#normalize data to maximum? (stacked 100% plot)
	if(stacked100){
		sumDataCols<- function(X){
			return(sum(X[2:length(X)]))
		}
		dataSums= apply(data, MARGIN=1, FUN=sumDataCols)
		Y= data[,2:ncol(data)]/dataSums
	}else{
		Y= data[,2:ncol(data)]
	}
	ndatacols= ncol(Y)

	#make some checks before it breaks
	if(sum(Y<0)>=1) stop("Data columns can not have negative values.")
	if(sum(X<0)>=1) stop("Time scale can not have negative values.")
	if(nrow(Y)==0) stop("Data columns appear to be empty.")
	if(length(X)==0) stop("Time scale appears to be empty.")

	#work on default parameters
	if(is.na(areaBorderWidth)) areaBorderWidth<- 1
	if(length(colours)==1){
		if(is.na(colours)) colours<- rainbow(ndatacols)
	}
	areaBorderCol <- as.vector(matrix(areaBorderCol, nrow=ndatacols, ncol=1))
	colours <- as.vector(matrix(colours, nrow=ndatacols, ncol=1))
	areaBorderWidth <- as.vector(matrix(areaBorderWidth, nrow=ndatacols, ncol=1))

	if(length(order)>1) {
		if(length(order)!=ndatacols) stop("Order array must be the length of data columns.")
		Y <- Y[, order]
		colours <- colours[order]
		areaBorderWidth <- areaBorderWidth[order]
		areaBorderCol<- areaBorderCol[order]
	}

	upperPrevious <- X*0
	areas <- vector(mode="list", ndatacols)
	for(i in seq(areas)){
		upperThis <- upperPrevious + Y[,i]
		areas[[i]] <- list(x=c(X, rev(X)), y=c(upperPrevious, rev(upperThis)))
		upperPrevious <- upperThis
	}

	xaxs="r"
	yaxs="r"
	if(!addAxisSpace){
		xaxs="i"
		yaxs="i"
	}

	if(!is.na(file)){
		file=paste(file,".pdf",sep="")
		pdf(file, width = pdfW, height = pdfH, bg="white")
	}

	layout(matrix(1, ncol=1, byrow=TRUE))
	par(mar=c(4, 4, 2, 2), cex=0.9)

	ylim <- range(sapply(areas, function(x) range(x$y, na.rm=TRUE)), na.rm=TRUE)
	plot(X,Y[,1], ylab=ylab, xlab=xlab, ylim=ylim, t="n", xaxs=xaxs, yaxs=yaxs, main=main)
	for(i in seq(areas)){
		polygon(areas[[i]], border=areaBorderCol[i], col=colours[i], lwd=areaBorderWidth[i])
	}

	if(!is.na(addLegend)){
		names= colnames(Y)
		legend(addLegend, legend=names, pch=22, col="black", pt.bg=colours, pt.cex=legend.pt.cex, pt.lwd=1, lwd=0, lty=NA, box.lwd=NA, bg=legend.bg, cex=legend.cex)
	}

	box(lwd=boxlwd)

	if(!is.na(file)){
		a<- dev.off()
	}

}
