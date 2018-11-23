## Write a function that calculates cumulative distribution function of a binomial random 
## variable. Compare results from your function with pbinom() function.

cdf_binom = function(k, n, p) {
  a = 0
  sum = choose(n, 0) * (1 - p)^n
  for (i in 1:k) {
    a = choose(n, i) * p^i * (1 - p)^(n - i)
    sum = sum + a
  }
  return(sum)
}

cdf_binom(3, 10, 0.5)
pbinom(3, 10, 0.5)

cdf_binom(3, 20, 0.7)
pbinom(3, 20, 0.7)

## Write a function that runs simulations to obtain power from a one-sample t-test. Run your 
## function (with number of simulations = 10,000 ) with n = 30, delta = 0.5, sd = 1 and 
## sig.level = 0.05. Compare your results with power.t.test(n = 30, delta = 0.5, sd = 1, 
## sig.level = 0.05, type = 'one.sample').

power_t = function(n, delta, sd, sig.level) {
  a = mean(replicate(10000, t.test(x = rnorm(n, delta, sd))$p.value) < sig.level)
  return(a)
}

power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05, type = "one.sample")

power_t(n = 30, delta = 0.5, sd = 1, sig.level = 0.05)

