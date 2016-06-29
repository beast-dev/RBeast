parseHeader <- function(){
paste(
	"<?xml version=\"1.0\" standalone=\"yes\"?>\n<beast>\n"
	, sep = "")
}
####
TimeTag <- function(){
  paste("<!-- Created wtih RBeast at", Sys.time(), "-->\n")
}
####
parseTail <- function(){
  paste(
"<report>\n\t<property name=\"timer\">\n\t\t<mcmc idref=\"mcmc\"/>\n\t</property>\n</report>\n</beast>",
    sep = " "
  )
}
#####
parsePredictor <- function(Name, value){
  options(useFancyQuotes = "0X22")
  paste("\t<parameter id=",
        dQuote(paste(Name, collapse = " ")),
        " value=",
        dQuote(paste(as.vector(value), collapse = " ")),
        paste(" />\n"),
        sep = "")
}
#####
parseDistribution <- function(dist = c("normal", "poisson", "negativeBinomial")){
	Dist <- match.arg(dist)
	Link <- switch(Dist,
		normal = "identity",
		poisson = "log",
		negativeBinomial = "log"
	)
	subHeader <- paste("\t<glmModel id=\"GLM.glmModel\" checkFullRank=\"false\" family=\"",
										 dist,
										 "\" checkIdentifiability=\"false\">\n", sep = "")
	LinkString <- sprintf("\t<model linkFunction=\"%s\">\n", Link)
	parmBlock <- switch(Dist,
		normal = "\t<normalDistributionModel>\n\t\t<mean><parameter value=\"1.0\"/></mean>\n\t\t<scale><parameter id=\"LikVar\" value=\"1.0\"/></scale>\n\t</normalDistributionModel>\n" ,
		poisson = "\t<poissonDistributionModel>\n\t\t<mean><parameter value=\"1.0\"/></mean>\n\t</poissonDistributionModel>\n",
		negativeBinomial = "\t<negativeBinomialDistributionModel>\n\t\t<mean><parameter value=\"1.0\"/></mean>\n\t\t<alpha><parameter id=\"NB.alpha\" value=\"1.0\"/></alpha>\n\t</negativeBinomialDistributionModel>\n"
	)
	paste(subHeader, LinkString, parmBlock, "\t</model>\n", sep = "")
}
#####
parseGLMBlock <- function(dist, respVar, designMat){
	## 'dist' is a STRING[1] containing the sampling distribution (aka, The Likelihood)
	## 'respVar' is a STRING[1] that stores that name of the responsed (dependent) variable
	## 'designMat' is a (named) DATA.FRAME where each column is a taken to be a predictor
	P <- ncol(designMat) ## number of predictors
	depVar <- paste("\t<dependentVariables>\n\t<parameter idref=\"", respVar, "\"/>\n\t</dependentVariables>\n", sep = "")
	Preds <- lapply(1:P, function(i) parsePredictor(Name = names(designMat)[i], value = designMat[, i]) )
	PredBlock <- paste(
		"\t<independentVariables>\n",
		parsePredictor(Name = "GLM.glmCoefficients", value = rep(0.1, P)),
		paste("\t\t<indicator>\n", parsePredictor(Name = "GLM.coefIndicator" , value = rep(1, P)) , "\t\t</indicator>\n", sep = ""),
		"\t\t<designMatrix id=\"GLM.designMatrix\">\n",
	paste(Preds,  collapse = ""),
"\t\t</designMatrix>\n\t</independentVariables>\n", sep = "")
	paste(
		parseDistribution(dist = dist),
		depVar,
		PredBlock,
		"\t</glmModel>\n"
		,
		sep = ""
	)
}
#####
## Now a series of ugly-but-functional "parsers"...
#####
StatsBlock <- function(){
  paste(
    "\t\t<sumStatistic id=\"GLM.nonZeroIndicators\" name=\"nonZeroIndicatorCount\" elementwise=\"true\">\n
		\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t</sumStatistic>\n
    \t\t\t<parameter idref=\"GLM.coefIndicator\"/>\n\t\t</sumStatistic>\n
		\t\t<productStatistic id=\"GLM.coefficientsTimesIndicators\" elementwise=\"false\">\n
		\t\t\t<parameter idref=\"GLM.coefIndicator\"/>\n\t\t</productStatistic>\n", sep = ""
  )
}
#####
OperatorsBlock <- function(ssvs, dist = c("normal", "poisson", "negativeBinomial")){
	if(ssvs){
		OpsBlock <- paste(
			"\t<operators id=\"operators\" optimizationSchedule=\"log\">\n
\t\t<bitFlipOperator weight=\"10\" usesPriorOnSum=\"false\">\n
\t\t\t<parameter idref=\"GLM.coefIndicator\"/>\n\t\t</bitFlipOperator>\n
\t\t<bitMoveOperator weight=\"40\" numBitsToMove=\"1\" usesPriorOnSum=\"false\">\n
\t\t\t<bits>\n\t\t\t<parameter idref=\"GLM.coefIndicator\"/>\n\t\t\t</bits>\n
\t\t</bitMoveOperator>\n
\t\t<randomWalkOperator windowSize=\"0.5\" weight=\"25\">\n
\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t</randomWalkOperator>\n
\t\t<mvnOperator scaleFactor=\"1.0\" weight=\"25\" formXtXInverse=\"true\">\n
\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t\t\t<varMatrix>\n
\t\t\t\t<parameter idref=\"GLM.designMatrix\"/>\n\t\t\t\t</varMatrix>\n
\t\t</mvnOperator>\n
\t\t<scaleOperator scaleFactor=\"0.75\" weight=\"10\">\n\t\t\t\t<parameter idref=\"LikVar\"/>\n\t\t</scaleOperator>\n
\t</operators>\n"
			, collapse  = "")
	}else{
		OpsBlock <- paste(
			"\t<operators id=\"operators\" optimizationSchedule=\"log\">\n
\t\t<randomWalkOperator windowSize=\"0.5\" weight=\"50\">\n
\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n
\t\t</randomWalkOperator>\n\t\t<mvnOperator scaleFactor=\"1.0\" weight=\"50\" formXtXInverse=\"true\">\n
\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t\t\t<varMatrix>\n\t\t\t\t<parameter idref=\"GLM.designMatrix\"/>\n
\t\t\t\t</varMatrix>\n\t\t</mvnOperator>\n
\t\t<scaleOperator scaleFactor=\"0.75\" weight=\"10\">\n\t\t\t\t<parameter idref=\"LikVar\"/>\n\t\t</scaleOperator>\n
\t</operators>\n"
			, sep = "")
	}
	Dist <- match.arg(dist)
	gaussian.ScaleOPStr <- "\t\t<scaleOperator scaleFactor=\"0.75\" weight=\"10\">\n\t\t\t\t<parameter idref=\"LikVar\"/>\n\t\t</scaleOperator>\n"
	finalOpsBlock <- switch(Dist,
													normal = OpsBlock,
													poisson = gsub(gaussian.ScaleOPStr, "", OpsBlock),
													negativeBinomial = gsub("LikVar", "NB.alpha", OpsBlock)
	)
	paste(finalOpsBlock, sep = "")
}
#######
parseLoggers <- function(thn, fName, dist = c("normal", "poisson", "negativeBinomial")){
	NormalLogString <-
"\t<log id=\"screenLog\" logEvery=\"%i\">\n
\t\t<column label=\"Posterior\" dp=\"4\" width=\"12\">\n
\t\t\t<posterior idref=\"posterior\"/>\n
\t\t</column>\n
\t\t<column label=\"Prior\" dp=\"4\" width=\"12\">\n
\t\t\t<prior idref=\"prior\"/>\n
\t\t</column>\n
\t\t<column label=\"Likelihood\" dp=\"4\" width=\"12\">\n
\t\t\t<likelihood idref=\"likelihood\"/>\n
\t\t</column>\n
\t\t<column label=\"GLM.coefficients\" sf=\"6\" width=\"12\">\n
\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t</column>\n
\t\t<column label=\"GLM.nonZeroPredictors\" sf=\"6\" width=\"12\">\n
\t\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>\n\t\t</column>\n
\t</log>\n

\t<log id=\"fileLog\" logEvery=\"%i\" fileName=\"%s_GLM.log\" overwrite=\"false\">\n
\t\t<posterior idref=\"posterior\"/>\n
\t\t<prior idref=\"prior\"/>\n
\t\t<likelihood idref=\"likelihood\"/>\n
\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>\n
\t\t<parameter idref=\"GLM.coefIndicator\"/>
\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n
\t\t<parameter idref=\"LikVar\"/>\n
\t\t<productStatistic idref=\"GLM.coefficientsTimesIndicators\"/>\n
\t</log>\n

\t<log logEvery=\"%i\" fileName=\"%s_GLM_indicators.log\">\n
\t\t<parameter idref=\"GLM.coefIndicator\"/>\n
\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n
\t\t<productStatistic idref=\"GLM.coefficientsTimesIndicators\"/>\n
\t\t<glmModel idref=\"GLM.glmModel\"/>\n
\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>
\t</log>\n"

	PoissonLogString <-
"\t<log id=\"screenLog\" logEvery=\"%i\">\n
\t\t<column label=\"Posterior\" dp=\"4\" width=\"12\">\n
\t\t\t<posterior idref=\"posterior\"/>\n\t\t</column>\n
\t\t<column label=\"Prior\" dp=\"4\" width=\"12\">\n
\t\t\t<prior idref=\"prior\"/>\n
\t\t</column>\n
\t\t<column label=\"Likelihood\" dp=\"4\" width=\"12\">\n
\t\t\t<likelihood idref=\"likelihood\"/>\n
\t\t</column>\n
\t\t<column label=\"GLM.coefficients\" sf=\"6\" width=\"12\">\n
\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n
\t\t</column>\n
\t\t<column label=\"GLM.nonZeroPredictors\" sf=\"6\" width=\"12\">\n
\t\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>\n
\t\t</column>\n
\t</log>\n

\t<log id=\"fileLog\" logEvery=\"%i\" fileName=\"%s_GLM.log\" overwrite=\"false\">\n
\t\t<posterior idref=\"posterior\"/>\n
\t\t<prior idref=\"prior\"/>\n<likelihood idref=\"likelihood\"/>\n
\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>\n
\t\t<parameter idref=\"GLM.coefIndicator\"/>\n
\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n
\t\t<productStatistic idref=\"GLM.coefficientsTimesIndicators\"/>\n
\t</log>\n

\t<log logEvery=\"%i\" fileName=\"%s_GLM_indicators.log\">\n
\t\t<parameter idref=\"GLM.coefIndicator\"/>\n
\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n
\t\t<productStatistic idref=\"GLM.coefficientsTimesIndicators\"/>\n
\t\t<glmModel idref=\"GLM.glmModel\"/>\n
\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>
\t</log>\n"
	NegativeBinomialLogString <-
	  "\t<log id=\"screenLog\" logEvery=\"%i\">\n
	\t\t<column label=\"Posterior\" dp=\"4\" width=\"12\">\n
	\t\t\t<posterior idref=\"posterior\"/>\n
	\t\t</column>\n
	\t\t<column label=\"Prior\" dp=\"4\" width=\"12\">\n
	\t\t\t<prior idref=\"prior\"/>\n
	\t\t</column>\n
	\t\t<column label=\"Likelihood\" dp=\"4\" width=\"12\">\n
	\t\t\t<likelihood idref=\"likelihood\"/>\n
	\t\t</column>\n
	\t\t<column label=\"GLM.coefficients\" sf=\"6\" width=\"12\">\n
	\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t</column>\n
	\t\t<column label=\"GLM.nonZeroPredictors\" sf=\"6\" width=\"12\">\n
	\t\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>\n\t\t</column>\n
\t</log>\n

\t<log id=\"fileLog\" logEvery=\"%i\" fileName=\"%s_GLM.log\" overwrite=\"false\">\n
\t\t<posterior idref=\"posterior\"/>\n
\t\t<prior idref=\"prior\"/>\n
\t\t<likelihood idref=\"likelihood\"/>\n
\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>\n
\t\t<parameter idref=\"GLM.coefIndicator\"/>
\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n
\t\t<parameter idref=\"NB.alpha\"/>\n
\t\t<productStatistic idref=\"GLM.coefficientsTimesIndicators\"/>\n
\t</log>\n

\t<log logEvery=\"%i\" fileName=\"%s_GLM_indicators.log\">\n
\t\t<parameter idref=\"GLM.coefIndicator\"/>\n
\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n
\t\t<productStatistic idref=\"GLM.coefficientsTimesIndicators\"/>\n
\t\t<glmModel idref=\"GLM.glmModel\"/>\n
\t\t<sumStatistic idref=\"GLM.nonZeroIndicators\"/>
\t</log>\n"
	Dist <- match.arg(dist)
	LogString <- switch(Dist,
											normal = NormalLogString,
											poisson = PoissonLogString,
											negativeBinomial = NegativeBinomialLogString
	)
	paste(sprintf(
		LogString,
		thn, thn, fName, thn, fName))
}
#######
parsePriors <- function(dist = c("normal", "poisson", "negativeBinomial"), dim, PrZero){
	### 'dim' is the number of predictors;
	### 'PrZero' is the probability that there are zero included predictors
	NormalPriorString <-
"\t<prior id=\"prior\">\n
\t\t<normalPrior mean=\"0\" stdev=\"2\">\n\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t</normalPrior>\n
\t\t<gammaPrior shape=\"0.1\" scale=\"100.0\">\n\t\t\t<parameter idref=\"LikVar\" />\n\t\t</gammaPrior>\n
\t\t<binomialLikelihood>\n\t\t\t<proportion>\n\t\t\t\t<parameter value=\"%f\"/>\n\t\t\t</proportion>\n\t\t\t<trials>\n\t\t\t\t<parameter value=\"%s\"/>\n\t\t\t</trials>\n\t\t\t<counts>\n\t\t\t\t<parameter idref=\"GLM.coefIndicator\"/>\n\t\t\t</counts>\n\t\t</binomialLikelihood>\n
	\t</prior>\n"

	PoissonPriorString <-
		"\t<prior id=\"prior\">\n
\t\t<normalPrior mean=\"0\" stdev=\"2\">\n\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t</normalPrior>\n
\t\t<binomialLikelihood>\n\t\t\t<proportion>\n\t\t\t\t<parameter value=\"%f\"/>\n\t\t\t</proportion>\n\t\t\t<trials>\n\t\t\t\t<parameter value=\"%s\"/>\n\t\t\t</trials>\n\t\t\t<counts>\n\t\t\t\t<parameter idref=\"GLM.coefIndicator\"/>\n\t\t\t</counts>\n\t\t</binomialLikelihood>\n
	\t</prior>\n"
	
	NegativeBinomialPriorString <-
	  "\t<prior id=\"prior\">\n
\t\t<normalPrior mean=\"0\" stdev=\"2\">\n\t\t\t<parameter idref=\"GLM.glmCoefficients\"/>\n\t\t</normalPrior>\n
\t\t<gammaPrior shape=\"1.0\" scale=\"10.0\">\n\t\t\t<parameter idref=\"NB.alpha\" />\n\t\t</gammaPrior>\n
\t\t<binomialLikelihood>\n\t\t\t<proportion>\n\t\t\t\t<parameter value=\"%f\"/>\n\t\t\t</proportion>\n\t\t\t<trials>\n\t\t\t\t<parameter value=\"%s\"/>\n\t\t\t</trials>\n\t\t\t<counts>\n\t\t\t\t<parameter idref=\"GLM.coefIndicator\"/>\n\t\t\t</counts>\n\t\t</binomialLikelihood>\n
	\t</prior>\n"
	
	Dist <- match.arg(dist)
	PriorString <- switch(Dist,
												normal = NormalPriorString,
												poisson = PoissonPriorString,
												negativeBinomial = NegativeBinomialPriorString
	)
	indPars <- paste(rep(1, dim), collapse = " ")
	inc_prob <- 1-((PrZero)^(1/dim))
 sprintf(PriorString, inc_prob, indPars)
}
#######
parseMCMC <- function(Nit, thin, runName, dist, Dim, pr_zero){
	sprintf(
"<mcmc id=\"mcmc\" chainLength=\"%i\" autoOptimize=\"true\" operatorAnalysis=\"%s_GLM.ops\">\n
\t<posterior id=\"posterior\">\n%s\n
\t<likelihood id=\"likelihood\">\n
\t<glmModel idref=\"GLM.glmModel\"/>\n
\t</likelihood>\n
\t</posterior>\n
\t<operators idref=\"operators\"/>\n%s\n</mcmc>\n"
					, Nit, runName,
					parsePriors(dist = dist, dim = Dim, PrZero = pr_zero),
					parseLoggers(thn = thin, fName = runName, dist = dist)
	)
}
#######
glmXML <- function(response_variable, Distribution, Dt, DoBSSVS = TRUE,
                   Niter = 2E6, Nthin = 2E3, Pr_Zero = 1/2, file_name){
  GLM.xml.string <- paste(
    parseHeader(),
    TimeTag(),
    parsePredictor(Name = response_variable, value = Dt[, 1]), ## write dependent variable
    parseGLMBlock(dist = Distribution,
                  respVar = response_variable, designMat = Dt[, -1]),
    StatsBlock(),
    OperatorsBlock(ssvs = DoBSSVS, dist = Distribution),
    parseMCMC(Nit = Niter, thin = Nthin, runName = file_name, Dim = ncol(Dt)-1, dist = Distribution, pr_zero = Pr_Zero),
    parseTail(),
    sep = ""
  )
  return(gsub("\n\n", "\n", GLM.xml.string))
}