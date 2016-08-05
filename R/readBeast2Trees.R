#' Extract a list of phylogenies from a BEAST2 posterior file
#' @param file name of the BEAST2 posterior filename, usually ends with '.trees'
#' @param opt.burnin how many phylogenies to discard, a value of zero will keep all trees
#' @return a list of phylogenies of type 'phylo'
#' @examples
#'   trees_file <- system.file("extdata", "readBeast2TreesExample.trees", package = "RBeast")
#'   testit::assert(file.exists(trees_file))
#'   posterior <- readBeast2Trees(trees_file)
#'   testit::assert(length(posterior) == 10)
#'   testit::assert(class(posterior[[1]]) == "phylo")
#' @export
#' @author Oliver Ratmann
readBeast2Trees <- function(
  file,
  opt.burnin = 0
)
{
	tmp			<- readLines(file, n=2e3, warn = FALSE)
	tmp			<- which( grepl('#NEXUS', tmp) )
	if(length(tmp)>1)
	{
		cat(paste('\nFound #NEXUS headers, n=',length(tmp),'.\nDiscard all lines before last entry on line', tail(tmp,1)))
		cmd		<- paste('sed -i".bak" 1,',tail(tmp,1)-1,'d ', file, sep='')
		system(cmd)
		cmd		<- paste('sed -i".bak2" 1s/\\;// ', file, sep='')
		system(cmd)
		cmd		<- list.files(paste(rev(rev(strsplit(file, '/')[[1]])[-1]),collapse='/'), pattern='*bak*', full.names=TRUE)
		cat(paste('\nrm files\n', paste(cmd, collapse='\n')))
		file.remove(cmd)
	}
	mph			<- ape::read.nexus(file)
	#	remove burn in
	tmp			<- regexpr('[0-9]+',names(mph))
	if(any(tmp<0))	stop('unexpected nexus file without STATE iteration numbers')
	mph.it		<- as.numeric( regmatches( names(mph), tmp) )
	mph			<- lapply( which( mph.it>opt.burnin), function(j)	mph[[j]]	)
	mph.it		<- mph.it[ mph.it > opt.burnin ]
	names(mph)	<- paste('STATE_',mph.it,sep='')
	mph
}
