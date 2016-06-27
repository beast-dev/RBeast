## This script takes a .csv file and creates a  GLM using the data within
## the first column is assumed to be the response variable, whilst every other column is assumed to be a predictor
source("glm_parser_aux.R")
response_variable <- "Y"
Link_function <- "identity"
Distribution <- "poisson"
file_name <- "poisson_test"
Niter <- 5E6
Nthin <- Niter/1E3
DoBSSVS <- TRUE
Pr_Zero <- .5
###
### let's make some data
N <- 100
P <- 5
set.seed(666)
desMat <- BX <-  matrix(NA, ncol = P, nrow = N)
Betas <- rep(NA, P)
q <- .5
Inds <- sample(0:1, P, prob = c(1-q, q), replace = TRUE)
for(p in 1:P){
	Betas[p] <- runif(1, -1, 1) ## any beta between -2 and 2
	desMat[, p] <- rnorm(N, mean = 0, sd = runif(1, 1, 5)) ## any variance between 1 and 5
	BX[, p] <- Betas[p] * Inds[p]* desMat[, p]
}
names(desMat) <- paste("X", 1:P, sep = "")
Y <- rpois(N, lambda = exp(rowSums(BX)))
Dt <- data.frame(Y, desMat)
Betas*Inds
summary(glm(Y ~ -1 + ., data = Dt, family = "poisson"))
logLik(glm(Y ~ -1 + ., data = Dt, family = "poisson"))

###
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
write(gsub("\n\n", "\n", GLM.xml.string), file = paste("examples/", file_name, ".xml", sep = "") )