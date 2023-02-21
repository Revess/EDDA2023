#~~~~A~~~~#
df = read.csv("~/projects/EDDA2023/ass1/datasets/birthweight.txt")
#The First sentence
qqnorm(df$birthweight)

#The CI
t.test(df$birthweight,conf.level=0.96)

#Bootstrapping
B=206
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
binom.test(sum(df$birthweight<2800), n=nrow(df), alternative = "greater")
1-binom.test(sum(df$birthweight<2800), n=nrow(df), alternative = "greater")$p.value

power.t.test(n = length(df$birthweight), delta = (2800 - mean(df$birthweight)) / sd(df$birthweight), sd = sd(df$birthweight), sig.level = 0.05, type = "one.sample", alternative = "greater")

B=1000; n=length(df$birthweight)
psign=numeric(B) ## will contain p-values of sign test
pttest=numeric(B) ## will contain p-values of t-test
for(i in 1:B) {
x=rnorm(n,mean=0.5,sd=1) ## generate data under H1 with mu=0.5
pttest[i]=t.test(x)[[3]] ## extract p-value
psign[i]=binom.test(sum(x>0),n,p=0.5)[[3]] } ## extract p-value
sum(psign<0.05)/B # fraction of rejecting H0, the power of the sign test
sum(pttest<0.05)/B # fraction of rejecting H0, the power of the t-test

#~~~~D~~~~#
# t.test(df$birthweight, mu=2600, alternative = "less")
# binom.test(sum(df$birthweight<2600), n=nrow(df), alternative = "less")
zl = qnorm(0.25)
zl

binom.test(sum(df$birthweight<2600), n=nrow(df), alternative = "less")
binom.test(sum(df$birthweight<2600), n=nrow(df))

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