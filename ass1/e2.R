file_location = "C:/Users/markd/Code/EDDA2023/ass1/datasets/cholesterol.txt"

#~~~~A~~~~#
data <- read.csv(file_location, header = TRUE, sep = " ")
data
before = data$Before
after = data$After8weeks
head(data)

# Exploration
cor(data)
pairs(data)

# Boxplot
boxplot(data)

# Shapiro-Wilk
shapiro.test(before)
shapiro.test(after)

# Histogram
hist(before)
hist(after)

# QQ-plots:
qqnorm(before)
qqnorm(after)

mean(before)
mean(after)

# From these plots, we could say that the plots are pretty normal, no abnormalities.
# From the Boxplot, we can see that the before is slightly higher than the after.
# The means show that the mean is 0.7 higher in the before than in the after.

#~~~~B~~~~#
# Seems to be from of normal distribution
hist(before - after)
plot(before,after)

# Assuming the normality from A, we can use a Paired T-Test:
t.test(before, after, paired = TRUE)
#t = 14.946, df = 17, p-value = 3.279e-11
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  0.5401131 0.7176646
#sample estimates:
#  mean difference 
#0.6288889 

wilcox.test(before, after)
# Wilcoxon rank sum test with continuity correction
# 
# data:  before and after
# W = 210.5, p-value = 0.1288
# alternative hypothesis: true location shift is not equal to 0

hist(before)
hist(after)

ks.test(before, after, alternative = "less") # We expect that after8weeks is bigger than before.
ks.test(before, after, alternative = "greater") # We expect that before is bigger than after8weeks.

plot(before~after) # This Pearson's/Spearman's Rank correlation test shows that there might be a correlation?

#~~~~C~~~~#
# Apply the Cental Limit Theory (CLT).
#CLT is STD / sqrt(n)
theta_right = mean(after) + (mean(after) - 3) # Theta Hat
#CLT = SIGMA/SQRT(n)
clt = sd(after) / sqrt(length(after))
CI_left = theta_right - 2*clt
CI_right = theta_right + 2*clt

CI_left
CI_right

#~~~~D~~~~#
t_statistic = max(after)
t_statistic

runif(n = 18, min = 3, max = 8)

for (i in 3:12)
{
  runif(n = 18, min = 3, max = i)
}

# 
# #ks.test(before, after)
# B = 1000
# tstar = numeric(B)
# n=length(after)
# for (i in 1:B)
# {
#   #xstar = rexp(n, 1)
#   # Van de 1000 samples zijn er 40% die niet onder de max zitten?
#   xstar = punif(tstar, min=3, max = 12)
#   hist(xstar)
#   tstar[i] = max(xstar)
# }
# hist(tstar)
# hist(xstar)
# 
# 
# pl=sum(tstar<t_statistic)/B
# pr=sum(tstar>t_statistic)/B
# p=2*min(pl,pr)
# pl
# pr
# p

#~~~~E~~~~#
# For this question, we should look at the median of two groups.
median(after) < 6
binom.test(median(after) < 6, length(after))

# Sign test?
binom.test(sum(after < 4.5), length(after), p=0.25)
