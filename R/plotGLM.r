## Auxiliary functions to plot GLMs estimated using BEAST
#####################################
splitLog <- function(dt, burninP = .2){ ## get indicators and coefficients while discarding burn-in
  ## 'Product' is whether coefficients should be delta*beta (default) or just beta
  res <- vector(2, mode = "list")
  names(res) <- c("Indicators", "Coefficients")
  init <- round(.2 * nrow(dt))
  dt.b <- dt[init:nrow(dt), ]
  res[[1]] <- dt.b[, grep("coefIndicator", names(dt.b))]
  res[[2]] <- dt.b[, grep("GLM.glmCoefficients", names(dt.b))]
  return(res)
}
getSummary <- function(x, alpha = .95){
  return(
    data.frame(lwr = as.numeric(quantile(x, probs = (1 - alpha)/2 )),
         mean = mean(x), upr = as.numeric(quantile(x, probs = (1 + alpha)/2)), row.names = "")
  )
}
#
list2df <- function(ll){ ## could be skipped with a little of extra work... TODO
  N <- length(ll)
  dt <- data.frame(matrix(NA, nrow = N, ncol = 4 ))
  names(dt) <- c("parameter", "lwr", "mean", "upr")
  dt$parameter <- names(ll)
  for(i in 1:N) dt[i, 2:4] <- ll[[i]]
  return(dt)
}
#
getSummary <- function(x, alpha = .95){
  return(data.frame(lwr = quantile(x, probs = (1 - alpha)/2) ,
                    mean = mean(x),
                    upr = quantile(x, probs = (1 + alpha)/2)
  ))
}
#
conditional_betas_BEAST <- function(betas, inds){
  if(ncol(betas) != ncol(inds)) stop("Coefficients and indicators are not the same dimension")
  K <- ncol(betas)
  result <- data.frame(matrix(NA, ncol = 3 , nrow = K))
  names(result) <- c("lwr", "mean", "upr")
  for(k in 1:K){
    result[k, ] <- getSummary(betas[, k][inds[, k] == 1])
  }
  return(result)
}
#
plotSimpleGLM <- function(Names, Log, probZero = .5, BF = 3, intercept = FALSE, Burnin = .2,
                          export = TRUE, fileName = "GLM_plot", title = ""){
  require(ggplot2)
  require(repr)
  require(scales)
  require(grid)
  ## 'Names' is a vector with the predictor names
  ## 'Log' is the .log file [already loaded as a data.frame] to be analysed
  ## 'probZero' is the probability that no predictors are included
  ## 'BF' is the Bayes factor threshold (default 3)
  ## 'intercept' is a boolean specifying whether an intercept was included in the model
  ## 'Burnin' is the percent of the chain to be discarded as burn-in
  ## 'betaind' is a boolean specifying whether to report delta*beta
  Pars <- splitLog(Log, burninP = Burnin)
  if(intercept){
    if(!ncol(Pars$Indicators)== (length(Names)+ 1)) stop("Model probably doesn't have intercept")
    Pars <- lapply(Pars, function(x) x[, -ncol(x)])
  } 
  Summaries <- lapply(Pars, function(d) apply(d, 2, getSummary))
  SumDf <- lapply(Summaries, list2df)
  npred <- length(Names)
  inclusion.probabilities <- data.frame(
    p.mean = SumDf$Indicators$mean,
    p.lwr =  SumDf$Indicators$lwr,
    p.upr =  SumDf$Indicators$upr,
    predictor = Names
  )
  #
  regression.coefficients <- data.frame(predictor = Names,
                                        b = conditional_betas_BEAST(betas = Pars$Coefficients,
                                                                    inds = Pars$Indicators))
  #
  q <- 1-((probZero)^(1/npred))
  bf <- BF
  cutoff <- (q*bf)/(q*(bf-1) + 1)
  #
  p0 <- ggplot(regression.coefficients, aes(x = predictor , y = b.mean))+
    geom_pointrange(aes(ymin = b.lwr, ymax = b.upr), position = position_dodge(0.5)) + 
    coord_flip() +
    scale_y_continuous("Coefficient", expand = c(0, 0)) +
    scale_x_discrete("Predictor") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) + 
    theme_bw()
  
  p0 <- p0 +  theme(legend.position = "none")
  p1 <- ggplot(inclusion.probabilities, aes(x = predictor, y = p.mean))+
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous("Inclusion probability", expand = c(0, 0)) +
    scale_x_discrete("Predictor") +
    geom_hline(yintercept = cutoff, linetype = "dashed", colour = "black", size = 0.7) +
    geom_hline(yintercept = q, linetype = "solid", colour = "green", size = 0.2) + 
    ggtitle(title) +
    theme_bw()
  p1 <- p1 + guides(fill = guide_legend(reverse = TRUE)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank()
    )
  if(export){
    pdf(paste(fileName, ".pdf", sep = ""))
  }
  options(repr.plot.width = 10, repr.plot.height = 5)
  grid.draw(cbind(ggplotGrob(p0), ggplotGrob(p1), size = "first"))
  if(export){
    dev.off()
  }
}