### let's fabricate some data
N <- 100
P <- 5
set.seed(666)
Sigma <- 1
desMat <- BX <-  matrix(NA, ncol = P, nrow = N)
Betas <- rep(NA, P)
q <- .99995
Inds <- sample(0:1, P, prob = c(1-q, q), replace = TRUE)
for(p in 1:P){
	Betas[p] <- runif(1, -2, 2) ## any beta between -2 and 2
	desMat[, p] <- rnorm(N, mean = 0, sd = runif(1, 1, 5)) ## any variance between 1 and 5
	BX[, p] <- Betas[p] * Inds[p]* desMat[, p]
}
names(desMat) <- paste("X", 1:P, sep = "")
normY <- rnorm(N, mean = rowSums(BX), sd = Sigma)
poisY <- rpois(N, lambda = exp(rowSums(BX)))

normalData <- data.frame(Y = normY, desMat)
poissonData <- data.frame(Y = poisY, desMat)

Betas*Inds

summary(glm(Y ~ -1 + ., data = normalData))
logLik(glm(Y ~ -1 + ., data = normalData))

summary(glm(Y ~ -1 + ., data = poissonData, family = "poisson"))
logLik(glm(Y ~ -1 + ., data = poissonData, family = "poisson"))

###
library(RBeast)
normalXML <- glmXML(response_variable = "Y", Distribution = "normal", file_name = "gaussian_test", Niter = 5E6,
                    Nthin = 5E6/1E3, DoBSSVS = TRUE, Pr_Zero = .5, Dt = normalData)
poissonXML <- glmXML(response_variable = "Y", Distribution = "poisson", file_name = "poisson_test", Niter = 5E6,
                     Nthin = 5E6/1E3, DoBSSVS = TRUE, Pr_Zero = .5, Dt = poissonData)

write(normalXML, file = "normal_glm_example.xml")
write(poissonXML, file = "poisson_glm_example.xml")