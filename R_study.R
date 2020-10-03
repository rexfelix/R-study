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

# 1. 수치~수치

mpg %>% ggplot(aes(cty, hwy)) + geom_jitter()+geom_smooth(method = 'lm')
mpg %>% ggplot(aes(cty,hwy))+geom_boxplot()

# 상관계수
mpg %>% summarise(cor_pearson=cor(cty,hwy),cor_spearman=cor(cty,hwy, method = 'spearman'), cor_kendall=cor(cty,hwy, method = 'kendall'))

# linear regression
(hwy_lm <- lm(hwy~cty, data = mpg))
summary(hwy_lm)
# Call:
#   lm(formula = hwy ~ cty, data = mpg)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.3408 -1.2790  0.0214  1.0338  4.0461 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.89204    0.46895   1.902   0.0584 .  
# cty          1.33746    0.02697  49.585   <2e-16 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.752 on 232 degrees of freedom
# Multiple R-squared:  0.9138,	Adjusted R-squared:  0.9134 
# F-statistic:  2459 on 1 and 232 DF,  p-value: < 2.2e-16

predict(hwy_lm)
resid(hwy_lm)
oprs <- par(mfrow=c(2,2))
plot(hwy_lm)
par(oprs)

MASS::lqs(stack.loss~.,data=stackloss)
