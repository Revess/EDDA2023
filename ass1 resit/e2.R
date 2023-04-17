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

# Test the Pearson's correlation test
cor.test(before, after, method = 'pearson')

# Show the line through the Pearson correlation plot.
line <- function(before, after){
  abline(lm(after~before))
  points(before,after)}
pairs(data, panel=line)

# Boxplot
boxplot(data)

# Shapiro-Wilk
shapiro.test(before)
shapiro.test(after)

# Histogram
hist(before)
hist(after)

# QQ-plots:
qqnorm(before, main='QQ-plot before')
qqnorm(after, main='QQ-plot after')

mean(before)
mean(after)

# From these plots, we could say that the plots are pretty normal, no abnormalities.
# From the Boxplot, we can see that the before is slightly higher than the after.
# The means show that the mean is 0.7 higher in the before than in the after.

#~~~~B~~~~#
# Seems to be from of normal distribution
hist(before - after)
#plot(before, after, main = 'Correlation')
boxplot(before - after, main = 'Before - After')

# Assuming the normality from A, we can use a Paired T-Test:
t.test(before, after, paired = TRUE)
#t = 14.946, df = 17, p-value = 3.279e-11
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  0.5401131 0.7176646
#sample estimates:
#  mean difference 
#0.6288889 

wilcox.test(before, after, paired=TRUE, alternative = "two.sided")
#wilcox.test(before, after, paired=TRUE, alternative = "greater")
#wilcox.test(before, after, paired=TRUE, alternative = "l")
# Wilcoxon rank sum test with continuity correction
# 
# data:  before and after
# W = 210.5, p-value = 0.1288
# alternative hypothesis: true location shift is not equal to 0
cor(before, after)

hist(before)
hist(after)

ks.test(before, after, alternative = "less") # We expect that after8weeks is bigger than before.
ks.test(before, after, alternative = "greater") # We expect that before is bigger than after8weeks.
ks.test(before, after)

cor.test(before, after, method = 'pearson')

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

B = 1000
pvals = c()
for (i in seq(3,12, by=0.001)){
  maxes = c()
  for (j in 1:B)
  {
    # Collect maxes
    maxes[[length(maxes)+1]] = max(runif(18,3,i))
  }
  pl = sum(maxes < t_statistic) / B
  pr = sum(maxes > t_statistic) / B
  pvals[[length(pvals)+1]] = min(pl,pr)
}

plot(seq(3, 12, by=0.001), pvals, type='o', main='p-values of each possible theta')

left = min(seq(3, 12, by=0.001)[pvals>0.05])
right = max(seq(3, 12, by=0.001)[pvals>0.05])
left
right    

#lines(seq(3, 12, by=0.1), p_value_t > 0.05, col='blue', type='l')


#plot(seq(3,12, by=0.1), pvals, main='p-values of each possible theta')
pvals


# 
# t_statistic = max(after)
# t_statistic
# 
# pvals = numeric(9)
# for (i in 3:12)
# {
#   B = 1000
#   totals = numeric(B)
#   for (j in 1:B)
#   {
#     totals[j] = t.test(after, runif(n = 18, min = 3, max = i))$p.value
#   }
#   
#   pvals[i] = sum(totals) / B
#   print(pvals[i])
# }
# plot(pvals, main='p-values of each possible theta')
# pvals


#~~~~E~~~~#
# For this question, we should look at the median of two groups.
median(after)
median(after) < 6
binom.test(sum(after < 6), length(after), alternative = 'l')

# Sign test?
sum(after < 4.5)
sum(after < 4.3)
sum(after < 4.1)
binom.test(sum(after < 4.5), length(after), p=0.25, alternative = 'l')
binom.test(sum(after < 4.5), length(after), p=0.25, alternative = 'g')
