set.seed(8)
n <- 100
p <- 0.5
x <- rbinom(n,1,p)
x <- factor(x, levels = c(0,1), labels = c('no', 'yes'))
x
(t <- table(x))
prop.table(t)
(res <- binom.test(x = length(x[x=='yes']), n=length(x), p=0.5, alternative = 'two.sided'))
# Exact binomial test
# 
# data:  length(x[x == "yes"]) and length(x)
# number of successes = 48, number of trials = 100, p-value = 0.7644
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.3790055 0.5822102
# sample estimates:
#   probability of success 
# 0.48 

(res$conf.int[2] - res$conf.int[1])/2
# 0.1016024 오차한계

