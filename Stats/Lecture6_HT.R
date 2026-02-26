x= rnorm(7,-2,1) #obs sample
##INPUTS 
#x= sample
# mu= mu value under Ho (mu0)
#alternative = H1: less (h1: mu<mu0), 
                  # greater (h1: mu> mu0)
                  # two.site (h1: mu=!mu0)
# conf.level = 1- significance level
t.test(x, mu=-2, alternative = "greater", conf.level=0.95)
# t = -0.60341, this is T obs
# df = 6, 
# p-value = 0.7158
#alternative hypothesis: true mean is greater than -2
#95 percent confidence interval:
 #-2.533175       Inf
#sample estimates:

# TO REPRODUCE THE SOME VALUES
t_obs= (mean(x)-(-2))/(sd(x)/length(x)) #t obs
df= length(x)-1 #degrees of freedom
pval= 1 -pt(t_obs, df)


# we can decide if reject or not:
#1) comparing p_val with alpha
#2) compare t_alpha with t
t_alpha= qt(1-0.05,7-1)
# 3) look if CI includes or not mu0
# the CI includes mu0 (-2) so we do not reject 

# gaussina pop, unknown variuance, alpha=0.05
t.test(x, mu= 2, alternative= "two.sided", conf.level=0.95)
t_alpha= qt(1-0.05, 7-1)
t_alpha
t= (mean(x)-2)/(sd(x)/(length(x)))
t
