## ----init, message=FALSE-------------------------------------------------
set.seed(1)
library(moveHMM)

# Make up a fake covariate, to consider interactions later on
elk_data$temp <- rnorm(nrow(elk_data), 15, 8)

# Prepare data
data <- prepData(elk_data, type="UTM", coordNames = c("Easting", "Northing"))

# Number of states
nstate <- 2

# Fit model with covariates
m <- fitHMM(data, nbStates = nstate, stepPar0 = c(200, 1000, 200, 1000, 0.01, 0.01),
            anglePar0 = c(pi, 0, 1, 1), formula = ~ dist_water * temp)

## ----extract-pars--------------------------------------------------------
# Estimated step length parameters
stepMean <- m$mle$stepPar["mean",]
stepSD <- m$mle$stepPar["sd",]

# Estimated turning angle parameters
angleMean <- m$mle$anglePar["mean",]
angleCon <- m$mle$anglePar["concentration",]

## ----rate-shape----------------------------------------------------------
stepShape <- stepMean^2/stepSD^2
stepRate <- stepMean/stepSD^2

## ----states--------------------------------------------------------------
# Most likely state sequence
states <- viterbi(m)

## ----step-hists, out.width='.49\\linewidth', fig.width=6, fig.height=6, fig.show='hold'----
# Colours for states
mycols <- c("royalblue", "firebrick2")

# Grid of step length values, to plot densities
stepgrid <- seq(min(data$step, na.rm = TRUE),
                max(data$step, na.rm = TRUE),
                length = 1000)

# Loop over states
for(s in 1:nstate) {
    # Indices of observations in state s (excluding steps of length 0)
    ind <- which(states == s & data$step != 0)

    # Histogram of step lengths in state s
    hist(data$step[ind], col = "grey", border = 0, main = paste("State", s),
         xlab = "Step length (m)", probability = TRUE,
         xlim = range(data$step, na.rm = TRUE),
         breaks = seq(0, max(data$step, na.rm = TRUE), length = 20))

    # Estimated gamma density for state s
    points(stepgrid,
           dgamma(stepgrid, shape = stepShape[s], rate = stepRate[s]),
           col = mycols[s], type = "l")
}

## ----angle-hists, out.width='.49\\linewidth', fig.width=6, fig.height=6, fig.show='hold'----
# Grid of turning angle values
anglegrid <- seq(-pi, pi, length = 1000)

# Loop over states
for(s in 1:nstate) {
    # Indices of observations in state s
    ind <- which(states == s)

    # Histogram of turning angles in state s
    hist(data$angle[ind], col = "grey", border = 0, main = paste("State", s),
         xlab = "Turning angle (radians)", probability = TRUE,
         xlim = c(-pi, pi), breaks = seq(-pi, pi, length = 20))

    # Estimated von Mises density for state s
    points(anglegrid,
           dvm(anglegrid, mu = angleMean[s], kappa = angleCon[s]),
           col = mycols[s], type = "l")
}

## ----extract-beta--------------------------------------------------------
beta <- m$mle$beta

## ----design-mat----------------------------------------------------------
# Distance values at which the transition probabilities should be plotted
dist_water_grid <- seq(0, max(data$dist_water), length = 1000)

# Fixed values for other covariates (temperature)
temp_fixed <- c(10, 20)

# Design matrix for temp = 10
newcovs1 <- cbind("intercept" = 1,
                  "dist_water" = dist_water_grid,
                  "temp" = temp_fixed[1],
                  "dist_water:temp" = dist_water_grid * temp_fixed[1])

# Design matrix for temp = 20
newcovs2 <- cbind("intercept" = 1,
                  "dist_water" = dist_water_grid,
                  "temp" = temp_fixed[2],
                  "dist_water:temp" = dist_water_grid * temp_fixed[2])

## ----trMatrix------------------------------------------------------------
# Transition probability matrices for temp = 10
tpm1 <- moveHMM:::trMatrix_rcpp(nbStates = nstate,
                                beta = beta, covs = newcovs1)

# Transition probability matrices for temp = 20
tpm2 <- moveHMM:::trMatrix_rcpp(nbStates = nstate,
                                beta = beta, covs = newcovs2)

## ----trMatrix2-----------------------------------------------------------
tpm1[,,1:3]

## ----tpm, out.width='.49\\linewidth', fig.width=6, fig.height=6, fig.show='hold'----
# Plot transition probability from state 1 to state 2 (for temp = 10)
plot(dist_water_grid, tpm1[1,2,], type = "l", ylim = c(0, 1),
     xlab = "Distance to water (m)", ylab = "Transition probability",
     main = "Encamped to exploratory")
# Plot transition probability for temp = 20
points(dist_water_grid, tpm2[1,2,], type = "l", lty = 2)
# Add legend
legend("topleft", legend = paste("temp =", temp_fixed), lty = 1:2, bty = "n")

# Plot transition probability from state 2 to state 1 (for temp = 10)
plot(dist_water_grid, tpm1[2,1,], type = "l", ylim = c(0, 1),
     xlab = "Distance to water (m)", ylab = "Transition probability",
     main = "Exploratory to encamped")
# Plot transition probability for temp = 20
points(dist_water_grid, tpm2[2,1,], type = "l", lty = 2)
# Add legend
legend("topleft", legend = paste("temp =", temp_fixed), lty = 1:2, bty = "n")

## ----stat, out.width='.6\\linewidth', fig.width=6, fig.height=6, fig.align='center'----
# Compute stationary distribution for each row of 'newcovs'
stat <- stationary(m = m, covs = newcovs1)

# Visualise stationary distributions
head(stat)

# Plot stationary probability of state 1 as function of distance to water
plot(dist_water_grid, stat[,1], type="l", col = mycols[1], ylim = c(0, 1),
     xlab = "Distance to water (m)", ylab = "State probability")
# Stationary probability of state 2
points(dist_water_grid, stat[,2], type="l", col = mycols[2])

# Add legend
legend("topleft", legend = paste("State", 1:2),
       col = mycols, lty = 1, bty = "n")

