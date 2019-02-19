.conditional_betas_BEAST <- function(betas, inds) {
  if (ncol(betas) != ncol(inds))
    stop("Coefficients and indicators are not the same dimension")
  K <- ncol(betas)
  result <- data.frame(matrix(NA, ncol = 3, nrow = K))
  names(result) <- c("lwr", "mean", "upr")
  for (k in 1:K) {
    result[k, ] <- .get_summary(betas[, k][inds[, k] == 1])
  }
  return(result)
}
.split_log <- function(dt, burninP = 0.2) {
  ## get indicators and coefficients while discarding burn-in 'Product' is whether coefficients should be delta*beta
  ## (default) or just beta
  res <- vector(2, mode = "list")
  names(res) <- c("Indicators", "Coefficients")
  init <- round(burninP * nrow(dt))
  dt.b <- dt[init:nrow(dt), ]
  res[[1]] <- dt.b[, grep("coefIndicator", names(dt.b))]
  res[[2]] <- dt.b[, grep("glmCoefficients", names(dt.b))]
  return(res)
}
# could be skipped with a little of extra work... TODO
.list2df <- function(ll) {
  N <- length(ll)
  dt <- data.frame(matrix(NA, nrow = N, ncol = 4))
  names(dt) <- c("parameter", "lwr", "mean", "upr")
  dt$parameter <- names(ll)
  for (i in 1:N) dt[i, 2:4] <- ll[[i]]
  return(dt)
}
#
.get_summary <- function(x, alpha = 0.95) {
  return(data.frame(lwr = quantile(x, probs = (1 - alpha)/2),
                    mean = mean(x),
                    upr = quantile(x, probs = (1 + alpha)/2)))
}
#
.get_parameter_estimates <- function(Names, Log, Burnin, alpha, conditional, intercept) {
  Pars <- .split_log(Log, burninP = Burnin)
  if (intercept) {
    if (!ncol(Pars$Indicators) == (length(Names) + 1))
      stop("Model probably doesn't have intercept")
    Pars <- lapply(Pars, function(x) x[, -ncol(x)])
  }
  Summaries <- lapply(Pars, function(d) apply(d, 2, .get_summary, alpha = alpha))
  SumDf <- lapply(Summaries, .list2df)
  if (conditional) {
    SumDf$Coefficients <- data.frame(parameter = SumDf$Coefficients$parameter, .conditional_betas_BEAST(Pars$Coefficients,
                                                                                                        Pars$Indicators))
  }
  return(SumDf)
}
.make_dt_forplot <- function(summaryObj, p = 0.5) {
  ans <- data.frame(inclusionProbs = summaryObj$Indicators$mean, cutoffs = (summaryObj$Indicators$mean)/(1 - summaryObj$Indicators$mean)/(p)/(1 -
                                                                                                                                                p), meanCeffects = summaryObj$Coefficients$mean, lowerQuantile = summaryObj$Coefficients$lwr, upperQuantile = summaryObj$Coefficients$upr)
  return(ans)
}
#'Plots the results of a GLM analysis from BEAST
#' @param Names a character vector with the predictor names (labels)
#' @param Log a dataframe with traces without burnin
#' @param prob_zero a scalar specifying the probability that no predictors are included (default: 0.5)
#' @param cutoffs a numeric vector with the Bayes factor cutoffs to be used (dotted lines in the plot, default: 3 and 50)
#' @param intercept a boolean specifying whether the model has an intercept (default: FALSE)
#' @param alpha a scalar with the credibility level to be used for credibility intervals (default:0.95)
#' @param conditional a boolean specifying whether the coeffcients should be plotted conditionally
#' @param title a character with the title in the plot (default: none)
#' @param burnin a scalar specifying the *proportion* of the samples to discard as burn-in/warm-up
#' @param export a boolean (default: TRUE)
#' @param file_name a character specifying the name of the output PDF file. Ignored if export = FALSE.
#' @return  a (ggplot) plot object showing inclusion probabilities, Bayes factors and coefficient estimates
#' @example see vignette
#' @export
#' @author Luiz Max Carvalho/Philippe Lemey/Nuno Faria
plot_simple_glm <- function(Names, Log, prob_zero = 0.5, cutoffs = c(3, 50),
                            intercept = FALSE, alpha = 0.95,  conditional = TRUE,
                            title = "", burnin = 0.2, export = TRUE, file_name = "GLM_plot"){
  require(ggplot2)
  require(repr)
  require(scales)
  require(grid)
  summary_df <- .get_parameter_estimates(Names = Names, Log = Log,
                                         Burnin = burnin, alpha = alpha,
                                         conditional = conditional,
                                         intercept = intercept)

  inclusion.probabilities <- data.frame(p.mean = summary_df$Indicators$mean, p.lwr = summary_df$Indicators$lwr, p.upr = summary_df$Indicators$upr,
                                        predictor = Names)
  #
  regression.coefficients <- data.frame(predictor = Names, b = summary_df$Coefficients)
  #
  npred <- length(Names)
  q <- 1 - ((prob_zero)^(1/npred))
  cutoffs <- (q * cutoffs)/(q * (cutoffs - 1) + 1)
  #
  p0 <- ggplot(regression.coefficients, aes(x = predictor, y = b.mean)) +
    geom_pointrange(aes(ymin = b.lwr, ymax = b.upr), position = position_dodge(0.5)) +
    coord_flip() +
    scale_y_continuous("Coefficient", expand = c(0, 0)) +
    scale_x_discrete("Predictor") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    theme_bw()
  p0 <- p0 + theme(legend.position = "none")

  p1 <- ggplot(inclusion.probabilities, aes(x = predictor, y = p.mean)) +
    geom_bar(stat = "identity") + coord_flip() +
    scale_y_continuous("Inclusion probability", expand = c(0, 0)) +
    scale_x_discrete("Predictor") +
    geom_hline(yintercept = cutoffs, linetype = "dashed", colour = "black", size = 0.7) +
    geom_hline(yintercept = q, linetype = "solid", colour = "green", size = 0.2) +
    ggtitle(title) +
    theme_bw()
  p1 <- p1 +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                                                                 axis.title.y = element_blank())
  if (export) {
    pdf(paste(file_name, ".pdf", sep = ""))
    options(repr.plot.width = 10, repr.plot.height = 5)
    grid.draw(cbind(ggplotGrob(p0), ggplotGrob(p1), size = "first"))
    dev.off()
  }else{
    return ( grid.draw(cbind(ggplotGrob(p0), ggplotGrob(p1), size = "first")) )
  }
}
#'Plots the results of a GLM analysis from BEAST
#' @param Names a character vector with the predictor names (labels)
#' @param Log a dataframe with traces without burnin
#' @param prob_zero a scalar specifying the probability that no predictors are included (default: 0.5)
#' @param cutoffs a numeric vector with the Bayes factor cutoffs to be used (dotted lines in the plot, default: 3 and 50)
#' @param intercept a boolean specifying whether the model has an intercept (default: FALSE)
#' @param alpha a scalar with the credibility level to be used for credibility intervals (default:0.95)
#' @param conditional a boolean specifying whether the coeffcients should be plotted conditionally
#' @param title a character with the title in the plot (default: none)
#' @param burnin a scalar specifying the *proportion* of the samples to discard as burn-in/warm-up
#' @param export a boolean (default: TRUE)
#' @param file_name a character specifying the name of the output PDF file. Ignored if export = FALSE.
#' @return  a (ggplot) plot object showing inclusion probabilities, Bayes factors and coefficient estimates
#' @example see vignette
#' @export
#' @author Luiz Max Carvalho/Philippe Lemey/Nuno Faria
plot_glm <- function(Names, Log, prob_zero = 0.5, cutoffs = c(3, 50),
                     intercept = FALSE, alpha = 0.95, conditional = TRUE,
                     burnin = 0.2, title = "", export = TRUE, file_name = "GLM_plot"){
  require(ggplot2)
  require(repr)
  require(scales)
  require(grid)
  require(gridExtra)
  # auxiliary functions
  .layOut <- function(...) {
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    pushViewport(viewport(layout = grid.layout(n, p)))
    for (i in seq_len(length(x))) {
      print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]], layout.pos.col = x[[i]][[3]]))
    }
  }
  #
  .add_annotations <- function(data, predictors, labels) {
    data$rowname = labels
    data$x = rep(0, predictors)
    data$y = c(1:predictors)
    data$start = seq(0.5, (predictors - 0.5), by = 1)
    data$end = seq(1.5, (predictors + 0.5), by = 1)
    #
    cols = rep(c("lavender", "white"), predictors/2)
    odd <- predictors%%2
    if (odd != 0)
      cols <- c(cols, "lavender")
    data$color <- cols
    #
    return(data)
  }
  #
  .make_labels <- function() {
    p <- ggplot()
    p <- p + geom_rect(aes(NULL, NULL, xmin = -0.1, xmax = 0.65, ymin = start, ymax = end, fill = color), alpha = 0.4,
                       data = data1)
    p <- p + scale_fill_manual(values = gs.pal(2))
    p <- p + geom_text(aes(x = x, y = y, label = rowname),
                       hjust = 0, size = 3.5, data = data1)
    theme <- theme_update(axis.text.y = element_blank(), axis.line.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.x = element_text(colour = "white"),
                          axis.line.x = element_blank(),
                          axis.ticks.x = element_line(colour = "white"),
                          panel.border = element_rect(fill = NA, colour = "white"),
                          panel.background = element_rect(size = 1, fill = "white", colour = NA),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), legend.position = "none")
    p <- p + theme_set(theme)
    p <- p + scale_y_reverse()
    p <- p + xlab("")
    p <- p + ylab(NULL)
    return(p)
  }
  #
  .make_barplot <- function(data, postOdds1, postOdds2) {
    p <- ggplot()
    p <- p + geom_rect(aes(NULL, NULL, ymin = 0, ymax = 1, xmin = start, xmax = end, fill = color), alpha = 0.4,
                       data = data)
    p <- p + scale_fill_manual(values = gs.pal(2))
    p <- p + geom_hline(aes(yintercept = postOdds1), color = "black", size = 0.5)
    p <- p + geom_hline(aes(yintercept = postOdds2), color = "black", size = 0.7)
    p <- p + geom_bar(aes(y = inclusionProbs, x = y), stat = "identity", color = "black", width = 0.3, fill = "navyblue",
                      data = data)
    theme <- theme_update(axis.text.y = element_blank(), axis.line.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.x = element_text(colour = "black"),
                          axis.line.x = element_blank(),
                          axis.ticks.x = element_line(colour = "black"),
                          panel.border = element_rect(fill = NA, colour = "black"),
                          panel.background = element_rect(size = 1, fill = "white", colour = NA),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), legend.position = "none")
    p <- p + theme_set(theme)
    p <- p + xlab(NULL)
    p <- p + ylab("Inclusion probability")
    p <- p + ylab(bquote(paste("Inclusion probability ", "(E[", delta, "])", sep = " ")))
    p <- p + coord_flip()
    p <- p + scale_x_reverse()
    return(p)
  }
  #
  .make_linerange <- function(data) {
    lims <- seq(-5, 5, by = 0.5)
    Min = min(data$lowerQuantile)
    Max = max(data$upperQuantile)
    ylim1 = lims[max(which(lims < Min))]
    ylim2 = lims[min(which(lims > Max))]
    limits1 = seq(from = 0, to = ylim1, length.out = 5)
    limits2 = seq(from = 0, to = ylim2, length.out = 5)
    limits = sort(unique(c(limits1, limits2)))
    breaks1 = limits[seq(from = 1, to = length(limits), by = 2)]
    assign("ylim1", ylim1, envir = .GlobalEnv)
    assign("ylim2", ylim2, envir = .GlobalEnv)
    assign("limits1", limits1, envir = .GlobalEnv)
    assign("limits2", limits2, envir = .GlobalEnv)
    assign("limits", limits, envir = .GlobalEnv)
    assign("breaks1", breaks1, envir = .GlobalEnv)
    p <- ggplot()
    p <- p + geom_rect(aes(NULL, NULL, ymin = ylim1, ymax = ylim2, xmin = start, xmax = end, fill = color), alpha = 0.4,
                       data = data)
    p <- p + scale_fill_manual(values = gs.pal(2))
    p <- p + geom_hline(aes(yintercept = 0), color = "black", size = 0.7)
    p <- p + geom_hline(aes(yintercept = limits), color = "black", size = 0.5, linetype = "dotted")
    p <- p + geom_linerange(aes(ymin = lowerQuantile, ymax = upperQuantile, x = y),
                            size = 0.5, na.rm = TRUE, data = data)
    p <- p + geom_point(aes(x = y, y = meanCeffects), color = "black", fill = "skyblue", size = 3.2, pch = 21,
                        na.rm = TRUE, data = data)
    theme <- theme_update(axis.text.y = element_blank(), axis.line.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.x = element_text(colour = "black"),
                          axis.line.x = element_blank(),
                          axis.ticks.x = element_line(colour = "black"),
                          panel.border = element_rect(fill = NA, colour = "black"),
                          panel.background = element_rect(size = 1, fill = "white",colour = NA),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
    p <- p + theme_set(theme)
    p <- p + xlab(NULL)
    p <- p + ylab(bquote(paste("In coefficient ", "(", beta, "I", delta, "=1)", sep = " ")))

    p <- p + coord_flip()
    p <- p + scale_x_reverse()

    p <- p + scale_y_continuous(breaks = breaks1, limits = c(ylim1, ylim2))
    rm(ylim1, ylim2, limits1, limits2, limits, breaks1)
    return(p)
  }
  #
  BF1 <- cutoffs[1]
  BF2 <- cutoffs[2]
  npred <- length(Names)
  prior_inclusion_prob <- 1 - prob_zero^(1/npred)

  summary_df <- .get_parameter_estimates(Names = Names, Log = Log, Burnin = burnin, alpha = alpha, conditional = conditional,
                                         intercept = intercept)

  prior_odss <- prior_inclusion_prob/(1 - prior_inclusion_prob)
  post_odds1 <- BF1 * prior_odss/(1 + (BF1 * prior_odss))
  post_odds2 <- BF2 * prior_odss/(1 + (BF2 * prior_odss))

  predata <- .make_dt_forplot(summaryObj = summary_df, p = prior_inclusion_prob)
  data1 <- .add_annotations(data = predata, predictors = npred, labels = Names)

  gs.pal <- colorRampPalette(c("lavender", "white"))

  suppressWarnings(p1 <- .make_labels())
  suppressWarnings(p2 <- .make_barplot(data1, post_odds1, post_odds2))
  suppressWarnings(p3 <- .make_linerange(data1))
  if (export) {
    pdf(paste(file_name, ".pdf", sep = ""))
    suppressWarnings(.layOut(list(p1, 1, 1), list(p2, 1, 2), list(p3, 1, 3)))
    dev.off()
  }else{
    return ( .layOut(list(p1, 1, 1), list(p2, 1, 2), list(p3, 1, 3)) )
  }
}
