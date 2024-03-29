
context("densities")

test_that("C++ and R density functions are identical", {
    x <- seq(0, 100, length = 1000)
    # dgamma (note the conversion between mean/sd and shape/rate)
    expect_equal(dgamma(x, 0.05^2/0.01^2, 0.05/0.01^2), c(dgamma_rcpp(x, 0.05, 0.01)), tolerance = 1e-10)
    expect_equal(dgamma(x, 1000^2/1000^2, 1000/1000^2), c(dgamma_rcpp(x, 1000, 1000)), tolerance = 1e-10)
    expect_equal(dgamma(x, 50^2/10^2, 50/10^2), c(dgamma_rcpp(x, 50, 10)), tolerance = 1e-10)

    # dweibull
    expect_equal(dweibull(x, 0.05, 0.05), c(dweibull_rcpp(x, 0.05, 0.05)), tolerance = 1e-10)
    expect_equal(dweibull(x, 10, 10), c(dweibull_rcpp(x, 10, 10)), tolerance = 1e-10)
    expect_equal(dweibull(x, 1, 1.5), c(dweibull_rcpp(x, 1, 1.5)), tolerance = 1e-10)

    # dlnorm
    expect_equal(dlnorm(x, -1000, 0.05), c(dlnorm_rcpp(x, -1000, 0.05)), tolerance = 1e-10)
    expect_equal(dlnorm(x, 100, 100), c(dlnorm_rcpp(x, 100, 100)), tolerance = 1e-10)
    expect_equal(dlnorm(x, 0, 2), c(dlnorm_rcpp(x, 0, 2)), tolerance = 1e-10)

    # dexp
    expect_equal(dexp(x, 0.05), c(dexp_rcpp(x, 0.05)), tolerance = 1e-10)
    expect_equal(dexp(x, 100), c(dexp_rcpp(x, 100)), tolerance = 1e-10)
    expect_equal(dexp(x, 1), c(dexp_rcpp(x, 1)), tolerance = 1e-10)

    x <- seq(-pi, pi, length = 1000)
    # dvm
    expect_equal(dvm(x, 0, 0.05), c(dvm_rcpp(x, 0, 0.05)), tolerance = 1e-10)
    expect_equal(dvm(x, pi, 1000), c(dvm_rcpp(x, pi, 1000)), tolerance = 1e-10)
    expect_equal(dvm(x, pi/2, 1), c(dvm_rcpp(x, pi/2, 1)), tolerance = 1e-10)

    # dwrpcauchy
    expect_equal(dwrpcauchy(x, 0, 0.01), c(dwrpcauchy_rcpp(x, 0, 0.01)), tolerance = 1e-10)
    expect_equal(dwrpcauchy(x, pi, 0.99), c(dwrpcauchy_rcpp(x, pi, 0.99)), tolerance = 1e-10)
    expect_equal(dwrpcauchy(x, pi/2, 0.5), c(dwrpcauchy_rcpp(x, pi/2, 0.5)), tolerance = 1e-10)
})
