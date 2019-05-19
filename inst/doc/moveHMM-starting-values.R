## ----local-max, echo = FALSE, out.width='.49\\linewidth', fig.width=5, fig.height=5, fig.show='hold', fig.cap = 'Example of convergence issue. The optimisation was started from two different points, and the red line shows the steps of the optimiser in both cases. On the left, the starting value was too far from the maximum likelihood estimate, and the optimiser got stuck in a local maximum of the function.'----
# Likelihood function
f <- function(x) {
    res <- dnorm(x, 0, 1) + dnorm(x, 5, 0.5)
    # cat(x, ", ", res, ",\n", sep = "") # uncomment to get optimiser steps
    return(res)
}

# Negative likelihood function (for numerical minimisation)
g <- function(x) {
    return(- f(x))
}

# First scenario: starting value = - 3
x0 <- -3
m1 <- nlm(f = g, p = x0)

# Second scenario: starting value = 7
x0 <- 7
m2 <- nlm(f = g, p = x0)

# Optimiser steps in first scenario
steps1 <- matrix(c(-3, 0.004431848,
                   -3, 0.004431848,
                   -2.999997, 0.004431888,
                   -2.986704, 0.004611786,
                   -2.986701, 0.004611827,
                   -2.97293, 0.004805012,
                   -2.972927, 0.004805054,
                   -2.958645, 0.005012956,
                   -2.958642, 0.005013,
                   -2.943814, 0.005237254,
                   -2.943811, 0.005237299,
                   -2.928396, 0.005479779,
                   -2.928393, 0.005479826,
                   -2.912349, 0.005742692,
                   -2.912346, 0.005742741,
                   -2.895624, 0.006028491,
                   -2.895621, 0.006028541,
                   -2.878168, 0.006340079,
                   -2.878165, 0.006340132,
                   -2.85992, 0.00668085,
                   -2.859917, 0.006680905,
                   -2.840813, 0.007054788,
                   -2.840811, 0.007054845,
                   -2.820772, 0.007466599,
                   -2.820769, 0.007466658,
                   -2.79971, 0.007921875,
                   -2.799707, 0.007921937,
                   -2.777531, 0.008427303,
                   -2.777528, 0.008427368,
                   -2.754124, 0.008990936,
                   -2.754121, 0.008991005,
                   -2.729362, 0.009622544,
                   -2.729359, 0.009622616,
                   -2.703098, 0.01033407,
                   -2.703096, 0.01033415,
                   -2.675164, 0.01114025,
                   -2.675162, 0.01114033,
                   -2.645362, 0.01205942,
                   -2.64536, 0.01205951,
                   -2.613461, 0.01311463,
                   -2.613458, 0.01311472,
                   -2.579186, 0.01433518,
                   -2.579183, 0.01433528,
                   -2.542213, 0.01575873,
                   -2.54221, 0.01575883,
                   -2.502151, 0.01743428,
                   -2.502148, 0.01743439,
                   -2.458527, 0.01942651,
                   -2.458525, 0.01942663,
                   -2.410766, 0.02182201,
                   -2.410764, 0.02182214,
                   -2.358159, 0.0247385,
                   -2.358156, 0.02473864,
                   -2.299821, 0.02833869,
                   -2.299819, 0.02833884,
                   -2.234647, 0.03285136,
                   -2.234645, 0.03285152,
                   -2.161236, 0.03860365,
                   -2.161234, 0.03860383,
                   -2.077804, 0.04607092,
                   -2.077802, 0.04607112,
                   -1.982077, 0.05595239,
                   -1.982075, 0.05595261,
                   -1.871175, 0.06928082,
                   -1.871173, 0.06928107,
                   -1.741539, 0.08756124,
                   -1.741537, 0.0875615,
                   -1.589047, 0.112875,
                   -1.589046, 0.1128753,
                   -1.409683, 0.1477044,
                   -1.409682, 0.1477047,
                   -1.201467, 0.1938444,
                   -1.201466, 0.1938446,
                   -0.9685691, 0.2495736,
                   -0.9685681, 0.2495738,
                   -0.7268399, 0.3063317,
                   -0.7268389, 0.306332,
                   2.094733, 0.04447224,
                   0.2689029, 0.3847764,
                   0.2689039, 0.3847763,
                   -0.04701344, 0.3985016,
                   -0.04701244, 0.3985017,
                   0.001419394, 0.3989419,
                   0.001420394, 0.3989419,
                   -2.02258e-06, 0.3989423,
                   -1.02258e-06, 0.3989423),
                 ncol = 2, byrow = TRUE)

# Optimiser steps for second scenario
steps2 <- matrix(c(7, 0.0002676605,
                   7, 0.0002676605,
                   7.000007, 0.0002676455,
                   6.997859, 0.0002722824,
                   6.997866, 0.0002722672,
                   6.995683, 0.0002770557,
                   6.99569, 0.0002770403,
                   6.993471, 0.0002819877,
                   6.993478, 0.000281972,
                   6.991223, 0.0002870862,
                   6.99123, 0.0002870702,
                   6.988936, 0.0002923595,
                   6.988943, 0.0002923433,
                   6.98661, 0.0002978165,
                   6.986617, 0.0002978,
                   6.984244, 0.0003034667,
                   6.984251, 0.0003034498,
                   6.981835, 0.0003093201,
                   6.981842, 0.000309303,
                   6.979383, 0.0003153877,
                   6.97939, 0.0003153703,
                   6.976886, 0.0003216811,
                   6.976893, 0.0003216633,
                   6.974343, 0.0003282126,
                   6.97435, 0.0003281945,
                   6.971751, 0.0003349957,
                   6.971758, 0.0003349773,
                   6.969109, 0.0003420447,
                   6.969116, 0.000342026,
                   6.966415, 0.0003493752,
                   6.966422, 0.000349356,
                   6.963667, 0.0003570037,
                   6.963674, 0.0003569841,
                   6.960863, 0.0003649482,
                   6.96087, 0.0003649283,
                   6.958, 0.0003732282,
                   6.958007, 0.0003732078,
                   6.955077, 0.0003818646,
                   6.955084, 0.0003818439,
                   6.952091, 0.0003908804,
                   6.952098, 0.0003908591,
                   6.949039, 0.0004003001,
                   6.949046, 0.0004002784,
                   6.945918, 0.0004101507,
                   6.945925, 0.0004101285,
                   6.942726, 0.0004204615,
                   6.942733, 0.0004204388,
                   6.939458, 0.0004312643,
                   6.939465, 0.0004312411,
                   6.936113, 0.0004425942,
                   6.93612, 0.0004425704,
                   6.932685, 0.0004544893,
                   6.932692, 0.000454465,
                   6.929172, 0.0004669916,
                   6.929179, 0.0004669667,
                   6.925568, 0.0004801472,
                   6.925575, 0.0004801216,
                   6.92187, 0.0004940069,
                   6.921877, 0.0004939806,
                   6.918073, 0.0005086267,
                   6.918079, 0.0005085997,
                   6.91417, 0.0005240688,
                   6.914177, 0.0005240411,
                   6.910158, 0.0005404021,
                   6.910165, 0.0005403736,
                   6.906029, 0.0005577032,
                   6.906036, 0.0005576738,
                   6.901777, 0.0005760576,
                   6.901784, 0.0005760273,
                   6.897395, 0.000595561,
                   6.897402, 0.0005955298,
                   6.892875, 0.0006163207,
                   6.892882, 0.0006162885,
                   6.888209, 0.0006384575,
                   6.888216, 0.0006384243,
                   6.883387, 0.0006621081,
                   6.883394, 0.0006620737,
                   6.878399, 0.000687427,
                   6.878406, 0.0006873915,
                   6.873234, 0.0007145903,
                   6.873241, 0.0007145535,
                   6.86788, 0.0007437992,
                   6.867886, 0.0007437611,
                   6.862322, 0.0007752844,
                   6.862329, 0.0007752448,
                   6.856547, 0.0008093118,
                   6.856554, 0.0008092706,
                   6.850537, 0.0008461893,
                   6.850544, 0.0008461464,
                   6.844274, 0.0008862754,
                   6.844281, 0.0008862307,
                   6.837736, 0.0009299898,
                   6.837743, 0.000929943,
                   6.8309, 0.0009778265,
                   6.830906, 0.0009777776,
                   6.823739, 0.001030371,
                   6.823745, 0.00103032,
                   6.816222, 0.001088322,
                   6.816229, 0.001088268,
                   6.808316, 0.00115252,
                   6.808323, 0.001152464,
                   6.79998, 0.001223984,
                   6.799986, 0.001223924,
                   6.791167, 0.001303958,
                   6.791174, 0.001303894,
                   6.781825, 0.00139398,
                   6.781832, 0.001393913,
                   6.77189, 0.001495972,
                   6.771897, 0.0014959,
                   6.761287, 0.001612358,
                   6.761294, 0.001612281,
                   6.749928, 0.001746242,
                   6.749935, 0.001746159,
                   6.737705, 0.001901654,
                   6.737712, 0.001901565,
                   6.724488, 0.002083916,
                   6.724494, 0.00208382,
                   6.710113, 0.002300186,
                   6.71012, 0.00230008,
                   6.694379, 0.002560295,
                   6.694386, 0.002560178,
                   6.677027, 0.002878082,
                   6.677034, 0.002877953,
                   6.657721, 0.003273586,
                   6.657728, 0.003273442,
                   6.636015, 0.003776796,
                   6.636021, 0.003776632,
                   6.6113, 0.004434395,
                   6.611306, 0.004434206,
                   6.58272, 0.005322598,
                   6.582726, 0.005322376,
                   6.549023, 0.006573303,
                   6.54903, 0.006573036,
                   6.508295, 0.008432167,
                   6.508302, 0.008431835,
                   6.457424, 0.0114021,
                   6.45743, 0.01140167,
                   6.390954, 0.0166508,
                   6.39096, 0.01665021,
                   6.298313, 0.0274051,
                   6.29832, 0.0274042,
                   6.155994, 0.05510944,
                   6.156, 0.05510788,
                   5.901172, 0.1572351,
                   5.901178, 0.1572318,
                   5.334393, 0.6379931,
                   5.334398, 0.6379885,
                   4.481027, 0.465604,
                   4.989382, 0.7977062,
                   4.989387, 0.7977064,
                   5.002551, 0.7978756,
                   5.002556, 0.7978756,
                   4.999995, 0.797886,
                   5, 0.797886,
                   4.999995, 0.797886,
                   5, 0.797886),
                 ncol = 2, byrow = TRUE)

# Grid of parameter values, for plot
grid <- seq(-5, 10, length = 1e3)

# Plot optimiser steps for scenario 1
plot(grid, f(grid), type = "l", xlab = "parameter", ylab = "likelihood",
     main = "Local maximum")
points(steps1[seq(1, nrow(steps1), by = 1), ], type = "o",
       col = "firebrick2", pch = 20, cex = 0.7)
points(steps1[1,1], steps1[1,2], pch = "+",
       col = "seagreen", cex = 2.5)
points(steps1[nrow(steps1),1], steps1[nrow(steps1),2],
       pch = "+", col = "royalblue", cex = 2.5)
legend("topleft", legend = c("Starting value", "Estimate"),
       col = c("seagreen", "royalblue"), pch = "+", bty = "n", pt.cex = 2)

# Plot optimiser steps for scenario 2
plot(grid, f(grid), type="l", xlab = "parameter", ylab = "likelihood",
     main = "Global maximum")
points(steps2[seq(1, nrow(steps2), by = 1), ], type = "o",
       col = "firebrick2", pch = 20, cex = 0.7)
points(steps2[1,1], steps2[1,2], pch = "+",
       col = "seagreen", cex = 2.5)
points(steps2[nrow(steps2),1], steps2[nrow(steps2),2], pch = "+",
       col = "royalblue", cex = 2.5)
legend("topleft", legend = c("Starting value", "Estimate"),
       col = c("seagreen", "royalblue"), pch = "+", bty = "n", pt.cex = 2)

## ----readdat, cache = TRUE, results = 'hide', message = FALSE------------
# Load package
library(moveHMM)

# Read data from URL
URL <- paste0("https://www.datarepository.movebank.org/bitstream/handle/",
              "10255/move.373/Elliptical%20Time-Density%20Model%20%28Wall%",
              "20et%20al.%202014%29%20African%20Elephant%20Dataset%20%",
              "28Source-Save%20the%20Elephants%29.csv")
rawdata <- read.csv(url(URL))

# Select and rename relevant columns (only keep 1000 rows for speed)
eleph_data <- rawdata[1:1000, c(11, 4, 5)]
colnames(eleph_data) <- c("ID", "x", "y")

# Derive step lengths and turning angles
hmmdata <- prepData(eleph_data)

# Display first rows of data set
head(hmmdata)

## ----stephist2, out.width='.6\\linewidth', fig.width=6, fig.height=5, fig.align="center"----
# Plot histogram of step lengths
hist(hmmdata$step)

## ----steppar-------------------------------------------------------------
# Starting values for the step length parameters
stepMean0 <- c(0.1, 0.7) # initial means (one for each state)
stepSD0 <- c(0.1, 0.7) # initial standard deviations (one for each state)
stepPar0 <- c(stepMean0, stepSD0)

## ----zeromass------------------------------------------------------------
# Indices of steps of length zero
whichzero <- which(hmmdata$step == 0)

# Proportion of steps of length zero in the data set
length(whichzero)/nrow(hmmdata)

## ----anglehist2, out.width='.6\\linewidth', fig.width=6, fig.height=5, fig.align="center"----
# Plot histogram of turning angles
hist(hmmdata$angle, breaks = seq(-pi, pi, length = 15))

## ----vonmises, echo = FALSE, message = FALSE, out.width='.6\\linewidth', fig.width=6, fig.height=5, fig.align="center"----
# Load package for von Mises distribution
library(CircStats)

# Colours for line plots below
cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

# Grid of turning angle values
anglegrid <- seq(-pi, pi, length = 1000)

# Concentration parameters
kappas <- c(0, 1, 2, 5, 10, 20)

# Plot the six density functions
plot(anglegrid, dvm(anglegrid, mu = 0, kappa = kappas[1]), type = "l",
     col = cols[1], xlab = "Turning angle (radians)", ylab = "Density",
     ylim = c(0, 2), xaxt = "n")
for(i in 2:length(kappas)) {
    kappa <- kappas[i]
    points(anglegrid, dvm(anglegrid, mu = 0, kappa = kappa), type = "l", col = cols[i])
}
axis(side = 1, at = c(-pi, -pi/2, 0, pi/2, pi),
     labels = expression(-pi, -pi/2, 0, pi/2, pi))
legend("topleft", legend = kappas, col = cols, lty = 1,
       title = expression(kappa), bty = "n")

## ----anglepar------------------------------------------------------------
# Starting values for the turning angle parameters
angleMean0 <- c(0, 0) # initial means (one for each state)
angleCon0 <- c(0.5, 3) # initial concentrations (one for each state)
anglePar0 <- c(angleMean0, angleCon0)

## ----fithmm--------------------------------------------------------------
m <- fitHMM(data = hmmdata, nbStates = 2, stepPar0 = stepPar0, anglePar0 = anglePar0)

## ----random--------------------------------------------------------------
# For reproducibility
set.seed(12345)

# Number of tries with different starting values
niter <- 25

# Save list of fitted models
allm <- list()

for(i in 1:niter) {
    # Step length mean
    stepMean0 <- runif(2,
                       min = c(0.05, 0.3),
                       max = c(0.3, 1))
    
    # Step length standard deviation
    stepSD0 <- runif(2,
                     min = c(0.05, 0.3),
                     max = c(0.3, 1))
    
    # Turning angle mean
    angleMean0 <- c(0, 0)
    
    # Turning angle concentration
    angleCon0 <- runif(2,
                       min = c(0.1, 1),
                       max = c(1, 5))
    
    # Fit model
    stepPar0 <- c(stepMean0, stepSD0)
    anglePar0 <- c(angleMean0, angleCon0)
    allm[[i]] <- fitHMM(data = hmmdata, nbStates = 2, stepPar0 = stepPar0,
                        anglePar0 = anglePar0)
}

## ----allnllk-------------------------------------------------------------
# Extract likelihoods of fitted models
allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))
allnllk

## ----mbest---------------------------------------------------------------
# Index of best fitting model (smallest negative log-likelihood)
whichbest <- which.min(allnllk)

# Best fitting model
mbest <- allm[[whichbest]]
mbest

## ----parallel, eval = FALSE----------------------------------------------
#  # Package for parallel computations
#  library(parallel)
#  
#  # Create cluster of size ncores
#  ncores <- detectCores() - 1
#  cl <- makeCluster(getOption("cl.cores", ncores))
#  # Export objects needed in parallelised function to cluster
#  clusterExport(cl, list("hmmdata", "fitHMM"))
#  
#  # Number of tries with different starting values
#  niter <- 25
#  
#  # Create list of starting values
#  allPar0 <- lapply(as.list(1:niter), function(x) {
#      # Step length mean
#      stepMean0 <- runif(2,
#                         min = c(0.05, 0.3),
#                         max = c(0.3, 1))
#  
#      # Step length standard deviation
#      stepSD0 <- runif(2,
#                       min = c(0.05, 0.3),
#                       max = c(0.3, 1))
#  
#      # Turning angle mean
#      angleMean0 <- c(0, 0)
#  
#      # Turning angle concentration
#      angleCon0 <- runif(2,
#                         min = c(0.1, 1),
#                         max = c(1, 5))
#  
#      # Return vectors of starting values
#      stepPar0 <- c(stepMean0, stepSD0)
#      anglePar0 <- c(angleMean0, angleCon0)
#      return(list(step = stepPar0, angle = anglePar0))
#  })
#  
#  # Fit the niter models in parallel
#  allm_parallel <- parLapply(cl = cl, X = allPar0, fun = function(par0) {
#      m <- fitHMM(data = hmmdata, nbStates = 2, stepPar0 = par0$step,
#                  anglePar0 = par0$angle)
#      return(m)
#  })
#  
#  # Then, we can extract the best-fitting model from allm_parallel
#  # as before

