% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rwrpcauchy}
\alias{rwrpcauchy}
\title{Sample from wrapped Cauchy distribution}
\usage{
rwrpcauchy(n, mu, rho)
}
\arguments{
\item{n}{Number of samples}

\item{mu}{Mean parameter}

\item{rho}{Concentration parameter}
}
\value{
Vector of n samples from wrpcauchy(mu, rho)
}
\description{
Sample from wrapped Cauchy distribution
}
\details{
Uses basic rejection sampling, based on dwrpcauchy(), which might
be inefficient for large rho.
}
