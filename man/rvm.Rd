% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rvm}
\alias{rvm}
\title{Sample from von Mises distribution}
\usage{
rvm(n, mu, kappa)
}
\arguments{
\item{n}{Number of samples}

\item{mu}{Mean parameter}

\item{kappa}{Concentration parameter}
}
\value{
Vector of n samples from vm(mu, kappa)
}
\description{
Sample from von Mises distribution
}
\details{
Uses basic rejection sampling, based on dvm(), which might
be inefficient for large kappa. Could be improved following Best & Fisher
(1979), Efficient simulation of the von Mises distribution, JRSSC, 28(2),
152-157.
}
