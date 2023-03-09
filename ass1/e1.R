#~~~~A~~~~#
df = read.csv("~/projects/EDDA2023/ass1/datasets/birthweight.txt")
df = read.csv("C:/Users/markd/Code/EDDA2023/ass1/datasets/birthweight.txt")

#The First sentence
qqnorm(df$birthweight)


# Load the birthweight data
sample_mean <- mean(df$birthweight)
sample_sd <- sd(df$birthweight)
n=length(df$birthweight)

# Calculate the margin of error for a 96% confidence interval with a maximum length of 100
n <- (qnorm(0.98)^2*sample_sd^2)/(100^2)
n





#The CI
restarts = 1000
pvals = c()
for(i in 2:500) {
    averaging = c()
    for(j in 1:restarts) {
        values = t.test(sample(df$birthweight,i),conf.level=0.96)$conf.int
        averaging = c(averaging, abs(values[2]-values[1]))
    }
    pvals = c(pvals, mean(averaging))
}
plot(pvals)
pvals

#Bootstrapping
B=100
CI=0.96
a=1-CI
Tstar=numeric(B)
c1 = df$birthweight
for(i in 1:B) {
    Xstar=sample(c1,replace=TRUE)
    Tstar[i]=mean(Xstar) 
}

L=quantile(Tstar,a)
U=quantile(Tstar,1-(a))
sum(Tstar<L)
c(2*mean(c1)-U,2*mean(c1)-L)


#~~~~B~~~~#
t.test(df$birthweight, mu=2800, alternative = "greater")
#We reject the null hypothesis, 0.01357<0.05
binom.test(sum(df$birthweight>2800), n=nrow(df), alternative = "greater")
#We reject the null hypothesis, 0.03399<0.05

#~~~~C~~~~#
binom.test(sum(df$birthweight<2800), n=nrow(df), alternative = "greater", power=0.8)
1-binom.test(sum(df$birthweight<2800), n=nrow(df), alternative = "greater")$p.value

power.t.test(n = length(df$birthweight), delta = mean(df$birthweight)-2800, sd = sd(df$birthweight), alternative="one.sided")

binom.test(sum(df$birthweight>2800), n=nrow(df), p=length(df[df$birthweight > 2800,])/length(df$birthweight), alternative = "greater")



power.signtest()
B=1000; n=length(df$birthweight)
psign=numeric(B) ## will contain p-values of sign test
pttest=numeric(B) ## will contain p-values of t-test
for(i in 1:B) {
x=rnorm(n,mean=2800,sd=sd(df$birthweight)) ## generate data under H1 with mu=2800
pttest[i]=t.test(x)[[3]] ## extract p-value
psign[i]=binom.test(sum(x>0),n,p=2800)[[3]] } ## extract p-value
sum(psign<0.05)/B # fraction of rejecting H0, the power of the sign test
sum(pttest<0.05)/B # fraction of rejecting H0, the power of the t-test

############################
# This was some testing, ignore:
#binom.test(sum(df$birthweight<2600), n=nrow(df), p=0.025)
#sum(df$birthweight<2600)
#r = 0.25 + (1.96 * sqrt((0.25*0.75)/sum(df$birthweight<2600)))
#l = 0.25 - (1.96 * sqrt((0.25*0.75)/sum(df$birthweight<2600)))
#sqrt((r-l)/2)
#(r-l)/2/2


#lower = subset(df, birthweight < 2600)
#prob = length(lower$birthweight) / length(df$birthweight)
#prob
#mean(lower$birthweight)


#new_mean = mean(df$birthweight) * (2*prob)
#new_mean
#binom.test(lower, n=nrow(lower$birthweight), p=0.25)


#binom.test(ll, lb, p=0.5)
#binom.test(length(subset(df, birthweight < 2600)$birth), length(df$birthweight), alternative="less", conf.level=0.25)

############################
#~~~~D~~~~#
# Calculate the PR based on PR
lower = subset(df, birthweight < 2600)$birthweight
sample_mean_amount = mean(lower)
sample_mean_amount
sample_mean_prob = length(lower) / length(df$birthweight)
sample_mean_prob
pl = 0.25
pr = sample_mean_prob + (sample_mean_prob - pl)
pl
pr

mean_all = mean(df$birthweight)
mean_all
cl_left = mean_all * (2 * pl)
cl_right = mean_all * (2 * pr)
cl_left
cl_right

result = (cl_left + cl_right) / 2
result
###############################

#~~~~E~~~~#
# sd(df$birthweight)
# mean(df$birthweight)

# pvals = c()

# for(i in 1:5000){
#     m2600 = sample(df[df < 2600], 34)
#     f2600 = sample(df[df < 2600 & df != m2600], 28)

#     males = sample(df[df >= 2600], 61)
#     females = sample(df[df >= 2600 & df != males], 65)

#     males = c(m2600,males)
#     females = c(f2600,females)
#     pvals = c(pvals, t.test(males,females)$p.value)
# }

# mean(pvals)
# hist(pvals)

malemeans = c()
femalemeans = c()
for(i in 1:5000){
    m2600 = sample(df[df < 2600], 34)
    f2600 = sample(df[df < 2600 & df != m2600], 28)

    males = sample(df[df >= 2600], 61)
    females = sample(df[df >= 2600 & df != males], 65)

    males = c(m2600,males)
    females = c(f2600,females)
    malemeans= c(malemeans,mean(males))
    femalemeans= c(femalemeans,mean(females))
}

t.test(malemeans,femalemeans)

hist(malemeans)
hist(femalemeans)

mean(malemeans)
mean(femalemeans)

