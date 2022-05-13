## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, error = FALSE, warning = FALSE,
  comment = NA
)

## ----load-package, echo = FALSE-----------------------------------------------
library(moveHMM)
library(ggplot2)
set.seed(342)
theme_set(theme_minimal())

## ----show-data----------------------------------------------------------------
head(haggis_data)

## ----prep-data----------------------------------------------------------------
data <- prepData(haggis_data, type = "UTM")

head(data)

## ----init-par, fig.width = 6, fig.height = 4, out.width="49%", fig.align = "center", fig.show="hold"----
hist(data$step, xlab = "step length")
hist(data$angle, breaks = seq(-pi, pi, length = 15), xlab = "turning angle")

stepPar0 <- c(1, 5, 1, 5)
anglePar0 <- c(pi, 0, 0.3, 5)

## ----fit-mod1-----------------------------------------------------------------
mod1 <- fitHMM(data = data, nbStates = 2, 
               stepPar0 = stepPar0, anglePar0 = anglePar0)

mod1

## ----plot-mod1, fig.width = 5, fig.height = 4, out.width="49%", fig.align = "center", fig.show="hold"----
plot(mod1, ask = FALSE, animals = 1)

## ----viterbi, fig.width = 6, fig.height = 4, out.width="49%", fig.align = "center", fig.show="hold"----
# Add most likely state sequence to data
data$state <- factor(viterbi(mod1))

# Plot tracks coloured by state
ggplot(data, aes(x, y, col = state, group = ID)) + 
    geom_path() +
    coord_equal()
# Plot step lengths coloured by state
ggplot(data, aes(x = 1:nrow(data), y = step, col = state, group = ID)) + 
    geom_point(size = 0.3)

## ----state-probs, fig.width = 6, fig.height = 4, out.width="60%", fig.align = "center"----
sp <- stateProbs(m = mod1)
head(sp)

# Add prob of state 1 to data set
data$sp1 <- sp[,1]
ggplot(data, aes(x, y, col = sp1, group = ID)) +
    geom_path() +
    coord_equal() +
    labs(col = "Pr(S = 1)")

## ----fit-mod2-----------------------------------------------------------------
mod2 <- fitHMM(data = data, nbStates = 2, 
               stepPar0 = stepPar0, anglePar0 = anglePar0, 
               formula = ~ temp + slope + I(slope^2))

## ----plot-mod2, fig.width = 6, fig.height = 4, out.width="49%", fig.align = "center", fig.show="hold"----
plot(mod2, ask = FALSE, plotTracks = FALSE, plotCI = TRUE)

## ----plot-stat, fig.width = 6, fig.height = 4, out.width="49%", fig.align = "center", fig.show="hold"----
plotStationary(mod2, plotCI = TRUE)

## ----aic----------------------------------------------------------------------
AIC(mod1, mod2)

## ----plot-pr, fig.width = 5, fig.height = 5, out.width="60%", fig.align = "center", fig.show="hold"----
plotPR(mod2)

## ----plot-tpm, fig.width = 6, fig.height = 4, out.width="60%", fig.align = "center"----
# Plot of transition probs as function of slope
plotData1 <- getPlotData(m = mod2, type = "tpm", format = "long")
ggplot(plotData1$slope, aes(slope, mle)) + 
    facet_wrap("prob") +
    geom_line() +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.3) +
    labs(y = "transition probability")

## ----plot-stat-2, fig.width = 6, fig.height = 4, out.width="60%", fig.align = "center"----
# Plot of stationary state probs as function of slope
plotData2 <- getPlotData(m = mod2, type = "stat", format = "long")
ggplot(plotData2$slope, aes(slope,  mle, col = factor(state))) + 
    geom_line() +
    geom_ribbon(aes(ymin = lci, ymax = uci, col = NULL,
                    fill = factor(state)), alpha = 0.3) +
    labs(fill = "state", col = "state", y = "stationary state probabilities")

## ----pred-tpm-----------------------------------------------------------------
new_data <- data.frame(temp = 0, slope = 17)
new_data

tpm <- predictTPM(m = mod2, newData = new_data, returnCI = TRUE)
tpm

