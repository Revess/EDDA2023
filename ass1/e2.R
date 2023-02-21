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

# An attempt at doing the permutation test, but the resulting P ends up at 0, which is because B = 1000.
# Or something, I don't actually know. Might have to be checked.
# As for the question, it is probably not applicable as the values are no independent (but instead correlate as they are the same person.)

# mystat=function(x,y) {mean(x-y)}
# B=1000
# tstar=numeric(B)
# for (i in 1:B) {
#   ashinastar = t(apply(cbind(before,after),1,sample))
#   val = mystat(ashinastar[,1],ashinastar[,2])
#   val
#   tstar[i] = val
# }
# tstar
# 
# myt=mystat(before, after)
# myt
# hist(tstar)
# lines(rep(myt,2),c(0,20),col="red",lwd=2)
# sum(tstar)
# plt = sum(tstar<myt)
# plt
# pl=plt /B
# pr=sum(tstar>myt)/B
# p=2*min(pl,pr)
# p
# sum(tstar<myt)



#~~~~C~~~~#
# Apply the Cental Limit Theory.
# We take samples of size = 4 with B = 1000.
A = 1000
A = 100 # Single trial
means = numeric(A)
for (a in 1:A)
{
  B = 1000
  sample_size = 3
  
  values = numeric(B)
  for (i in 1:B)
  {
    values[i] = mean(sample(after, sample_size))
  }
  #hist(values)
  
  sample_mean = mean(values)
  means[a] = sample_mean
}

hist(means)
mean(means)
mean(after)

head(after)
min(after)
max(after)


#standard_devation = sd(values)
#left_bound = sample_mean - 2*standard_devation
#right_bound = sample_mean + 2*standard_devation
#sample_mean
#standard_devation
#left_bound
#right_bound





B = 1000
sample_size = 3

rb = numeric(B)
means = numeric(B)
for (i in 1:B)
{
  mn = mean(sample(after, sample_size))
  rb[i] = mn + (mn - 3)
  means[i] = mn
}

hist(rb)
hist(means)

mean(means)
mean(rb)

mean(means) + (mean(means) - 3)
