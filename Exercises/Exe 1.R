set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))
out = NULL
for (i in 1:1000) {
  sample = sample(x, replace = TRUE)
  out[i] = mean(sample)
}
cv = sd(sample)/mean(sample)
quantile(cv, c(0.025, 0.975))
se_cv = cv/sqrt(2*length(sample))
cv + (1.96 * se_cv)
cv - (1.96 * se_cv)
