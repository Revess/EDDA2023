#~~~~A~~~~#
birthweights = read.csv("C:/Users/markd/Code/EDDA2023/ass1/datasets/birthweight.txt")
#The First sentence
qqnorm(birthweights$birthweight)

#The CI
t.test(birthweights$birthweight,conf.level=0.96)

#Bootstrapping
B=206
CI=0.96
a=1-CI
Tstar=numeric(B)
c1 = birthweights$birthweight
for(i in 1:B) {
    Xstar=sample(c1,replace=TRUE)
    Tstar[i]=mean(Xstar) 
}

L=quantile(Tstar,a)
U=quantile(Tstar,1-(a))
sum(Tstar<L)
c(2*mean(c1)-U,2*mean(c1)-L)


#~~~~B~~~~#
t.test(birthweights$birthweight, mu=2800, alternative = "greater")
#We reject the null hypothesis, 0.01357<0.05
binom.test(sum(birthweights$birthweight>2800), n=nrow(birthweights), alternative = "greater")
#We reject the null hypothesis, 0.03399<0.05

#~~~~C~~~~#
binom.test(sum(birthweights$birthweight<2800), n=nrow(birthweights), alternative = "greater")
1-binom.test(sum(birthweights$birthweight<2800), n=nrow(birthweights), alternative = "greater")$p.value

power.t.test(n = length(birthweights$birthweight), delta = (2800 - mean(birthweights$birthweight)) / sd(birthweights$birthweight), sd = sd(birthweights$birthweight), sig.level = 0.05, type = "one.sample", alternative = "greater")

B=1000; n=length(birthweights$birthweight)
psign=numeric(B) ## will contain p-values of sign test
pttest=numeric(B) ## will contain p-values of t-test
for(i in 1:B) {
x=rnorm(n,mean=0.5,sd=1) ## generate data under H1 with mu=0.5
pttest[i]=t.test(x)[[3]] ## extract p-value
psign[i]=binom.test(sum(x>0),n,p=0.5)[[3]] } ## extract p-value
sum(psign<0.05)/B # fraction of rejecting H0, the power of the sign test
sum(pttest<0.05)/B # fraction of rejecting H0, the power of the t-test

############################
# This was some testing, ignore:
#binom.test(sum(birthweights$birthweight<2600), n=nrow(birthweights), p=0.025)
#sum(birthweights$birthweight<2600)
#r = 0.25 + (1.96 * sqrt((0.25*0.75)/sum(birthweights$birthweight<2600)))
#l = 0.25 - (1.96 * sqrt((0.25*0.75)/sum(birthweights$birthweight<2600)))
#sqrt((r-l)/2)
#(r-l)/2/2


#lower = subset(birthweights, birthweight < 2600)
#prob = length(lower$birthweight) / length(birthweights$birthweight)
#prob
#mean(lower$birthweight)


#new_mean = mean(birthweights$birthweight) * (2*prob)
#new_mean
#binom.test(lower, n=nrow(lower$birthweight), p=0.25)


#binom.test(ll, lb, p=0.5)
#binom.test(length(subset(birthweights, birthweight < 2600)$birth), length(birthweights$birthweight), alternative="less", conf.level=0.25)

############################
#~~~~D~~~~#
# Calculate the PR based on PR
sample_mean_amount = mean(subset(birthweights, birthweight < 2600)$birthweight)
sample_mean_amount
sample_mean_prob = length(lower$birthweight) / length(birthweights$birthweight)
sample_mean_prob
pl = 0.25
pr = sample_mean_prob + (sample_mean_prob - pl)
pl
pr


mean_all = mean(birthweights$birthweight)
mean_all
cl_left = mean_all * (2 * pl)
cl_right = mean_all * (2 * pr)
cl_left
cl_right

result = (cl_left + cl_right) / 2
result

###############################
