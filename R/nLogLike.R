
#' Negative log-likelihood function
#'
#' @param wpar Vector of working parameters.
#' @param nbStates Number of states of the HMM.
#' @param bounds Matrix with 2 columns and as many rows as there are elements in \code{wpar}. Each row
#' contains the lower and upper bound for the correponding parameter.
#' @param parSize Vector of two values: number of parameters of the step length distribution,
#' number of parameters of the turning angle distribution.
#' @param data An object \code{moveData}.
#' @param covs Model matrix for transition probabilities
#' @param stepDist Name of the distribution of the step lengths (as a character string).
#' Supported distributions are: gamma, weibull, lnorm, exp. Default: gamma.
#' @param angleDist Name of the distribution of the turning angles (as a character string).
#' Supported distributions are: vm, wrpcauchy. Set to \code{"none"} if the angle distribution should
#' not be estimated. Default: vm.
#' @param angleMean Vector of means of turning angles if not estimated (one for each state).
#' Default: \code{NULL} (the angle mean is estimated).
#' @param zeroInflation \code{TRUE} if the step length distribution is inflated in zero.
#' Default: \code{FALSE}. If \code{TRUE}, initial values for the zero-mass parameters should be
#' included in \code{stepPar0}.
#' @param stationary \code{FALSE} if there are covariates. If \code{TRUE}, the initial distribution is considered
#' equal to the stationary distribution. Default: \code{FALSE}.
#' @param knownStates Vector of values of the state process which are known prior to fitting the
#' model (if any). Default: NULL (states are not known). This should be a vector with length the number
#' of rows of 'data'; each element should either be an integer (the value of the known states) or NA if
#' the state is not known.
#'
#' @return The negative log-likelihood of the parameters given the data.
#'
#' @examples
#' \dontrun{
#' # data is a moveData object (as returned by prepData), automatically loaded with the package
#' simPar <- example$simPar
#' par0 <- example$par0
#'
#' estAngleMean <- is.null(simPar$angleMean)
#' bounds <- moveHMM:::parDef(simPar$stepDist,simPar$angleDist,simPar$nbStates,
#'                            estAngleMean,simPar$zeroInflation)$bounds
#' parSize <- moveHMM:::parDef(simPar$stepDist,simPar$angleDist,simPar$nbStates,
#'                             estAngleMean,simPar$zeroInflation)$parSize
#'
#' par <- c(par0$stepPar0,par0$anglePar0)
#' wpar <- moveHMM:::n2w(par,bounds,par0$beta0,par0$delta0,simPar$nbStates,FALSE)
#'
#' covs <- model.matrix(example$m$conditions$formula, example$data)
#'
#' l <- nLogLike(wpar=wpar,nbStates=simPar$nbStates,bounds=bounds,parSize=parSize,
#'               data=data,covs=covs, stepDist=simPar$stepDist,
#'               angleDist=simPar$angleDist,
#'               zeroInflation=simPar$zeroInflation)
#' }
#'
#' @export

nLogLike <- function(wpar,nbStates,bounds,parSize,data,covs,stepDist=c("gamma","weibull","lnorm","exp"),
                     angleDist=c("vm","wrpcauchy","none"),angleMean=NULL,zeroInflation=FALSE,
                     stationary=FALSE,knownStates=NULL)
{
    # check arguments
    stepDist <- match.arg(stepDist)
    angleDist <- match.arg(angleDist)
    if(nbStates<1)
        stop("nbStates must be at least 1.")

    nbCovs <- ncol(covs) - 1 # substract intercept column

    if(!stationary & (length(wpar)!=sum(parSize)*nbStates+nbStates*(nbStates-1)*(nbCovs+1)+nbStates-1))
        stop("Wrong number of parameters in wpar.")
    if(stationary & (length(wpar)!=sum(parSize)*nbStates+nbStates*(nbStates-1)*(nbCovs+1)))
        stop("Wrong number of parameters in wpar.")
    if(length(data)<1)
        stop("The data input is empty.")

    if(is.null(data$step))
        stop("Missing field(s) in data.")

    estAngleMean <- (is.null(angleMean) & angleDist!="none")

    # convert the parameters back to their natural scale
    par <- w2n(wpar,bounds,parSize,nbStates,nbCovs,estAngleMean,stationary)

    if(!is.null(angleMean) & angleDist!="none") # if the turning angles' mean is not estimated
        par$anglePar <- rbind(angleMean,par$anglePar)

    nbObs <- length(data$step)

    nbAnimals <- length(unique(data$ID))

    # aInd = list of indices of first observation for each animal
    aInd <- c(1, which(data$ID[-1] != data$ID[-nbObs]) + 1)

    # easier to deal with in C++ function
    if(is.null(knownStates))
        knownStates <- -1
    else
        knownStates[which(is.na(knownStates))] <- 0

    # NULL arguments don't suit C++
    if(angleDist=="none")
        par$anglePar <- matrix(NA)
    if(stationary)
        par$delta <- c(NA)
    if(nbStates==1) {
        par$beta <- matrix(NA)
        par$delta <- c(NA)
        par$stepPar <- as.matrix(par$stepPar)
        par$anglePar <- as.matrix(par$anglePar)
    }

    nllk <- nLogLike_rcpp(nbStates,par$beta,as.matrix(covs),data,stepDist,angleDist,par$stepPar,
                          par$anglePar,par$delta,aInd,zeroInflation,stationary,knownStates)

    return(nllk)
}
