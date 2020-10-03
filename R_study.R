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

hwy_loess <- loess(hwy~displ, data = mpg)
summary(hwy_loess)
# Call:
#   loess(formula = hwy ~ displ, data = mpg)
# 
# Number of Observations: 234 
# Equivalent Number of Parameters: 4.57 
# Residual Standard Error: 3.372 
# Trace of smoother matrix: 4.98  (exact)
# 
# Control settings:
#   span     :  0.75 
# degree   :  2 
# family   :  gaussian
# surface  :  interpolate	  cell = 0.2
# normalize:  TRUE
# parametric:  FALSE
# drop.square:  FALSE 

mpg %>% ggplot(aes(hwy, displ))+geom_point()+geom_smooth()

# categorical x, numerical y

mpg %>% ggplot(aes(class, cty))+geom_boxplot()

# 1.ANOVA

class_hwy <- lm(hwy~class, data = mpg)
summary(class_hwy)
# Call:
#   lm(formula = hwy ~ class, data = mpg)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.1429 -1.8788 -0.2927  1.1803 15.8571 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       24.800      1.507  16.454  < 2e-16 ***
#   classcompact       3.498      1.585   2.206   0.0284 *  
#   classmidsize       2.493      1.596   1.561   0.1198    
# classminivan      -2.436      1.818  -1.340   0.1815    
# classpickup       -7.921      1.617  -4.898 1.84e-06 ***
#   classsubcompact    3.343      1.611   2.075   0.0391 *  
#   classsuv          -6.671      1.567  -4.258 3.03e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.37 on 227 degrees of freedom
# Multiple R-squared:  0.6879,	Adjusted R-squared:  0.6797 
# F-statistic: 83.39 on 6 and 227 DF,  p-value: < 2.2e-16

pars <- par(mfrow=c(2,2))
plot(class_cty)
par(pars)

# numerical x, categorical y

chall <- read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/challenger.csv',header = T, sep = ',')
chall <- tbl_df(chall)
glimpse(chall)
chall %>% ggplot(aes(temperature, distress_ct))+geom_point()
chall %>% ggplot(aes(factor(distress_ct), temperature))+geom_boxplot()
chall_glm <- glm(cbind(distress_ct,o_ring_ct-distress_ct)~temperature,data = chall,family = 'binomial')
summary(chall_glm)
# Call:
#   glm(formula = cbind(distress_ct, o_ring_ct - distress_ct) ~ temperature, 
#       family = "binomial", data = chall)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.7526  -0.5533  -0.3388  -0.1901   1.5388  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)  8.81692    3.60697   2.444  0.01451 * 
#   temperature -0.17949    0.05822  -3.083  0.00205 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 20.706  on 22  degrees of freedom
# Residual deviance:  9.527  on 21  degrees of freedom
# AIC: 24.865
# 
# Number of Fisher Scoring iterations: 6

predict(chall_glm, data.frame(temperature=30),type='response')
# 1 
# 0.9686946   => 0.97의 확률로 1,폭발


