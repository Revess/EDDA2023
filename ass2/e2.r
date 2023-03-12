########## START WITH RUNNING THE FOLLOWING COMMAND: ##########
install.packages('car')


#df = read.csv("~/projects/EDDA2023/ass1/datasets/birthweight.txt", sep=" ")
expensescrime = read.csv("C:/Users/markd/Code/EDDA2023/ass2/datasets/expensescrime.txt",  sep=" ")
#expensescrime_rounded = round()
cleaned = expensescrime[,-1]

# COLUMNS:
#$state
#$expend
#$bad
#$crime
#$lawyers
#$employ
#$pop

#~~~~A~~~~#
# a)  Make some graphical summaries of the data. Investigate the problem of influence points, and the problem of collinearity.

# Pairplots & boxplots will be used:
pairs(expensescrime[,-1]) # Make sure that your plot-size on screen is big enough
pairs(expensescrime[,c(2,4,6,7)]) # Subset

boxplot(expensescrime[,-1]) # This plots all besides the first column, as that is text

qqnorm(expensescrime$crime, main="Crime") # This seems to be the only normal one                                   (;))
qqline(expensescrime$crime, main="Crime") # This seems to be the only normal one                                   (;))
hist(expensescrime$crime)

# Extra plots that won't be used: (One of each can be used to showcase)
# qqnorm(expensescrime$expend)
# qqline(expensescrime$expend)
# qqnorm(expensescrime$bad)
# qqline(expensescrime$bad)
# qqnorm(expensescrime$lawyers)
# qqline(expensescrime$lawyers)
# qqnorm(expensescrime$employ)
# qqline(expensescrime$employ)
# qqnorm(expensescrime$pop)
# qqline(expensescrime$pop)
# 
# hist(expensescrime$expend)
# hist(expensescrime$expend)
# hist(expensescrime$bad)
# hist(expensescrime$lawyers)
# hist(expensescrime$employ)
# hist(expensescrime$pop)


# Influence points
lm_ex = lm(cleaned$expend ~ cleaned$crime)
lm_ba = lm(cleaned$bad ~ cleaned$crime)
lm_la = lm(cleaned$lawyers ~ cleaned$crime)
lm_em = lm(cleaned$employ ~ cleaned$crime)
lm_po = lm(cleaned$pop ~ cleaned$crime)
qqnorm(residuals(lm_ex))
qqnorm(residuals(lm_ba))
qqnorm(residuals(lm_la))
qqnorm(residuals(lm_em))
qqnorm(residuals(lm_po))

# Displays the outlier - Always results in 5 being the worst (expect for lm_ba)
order(abs(residuals(lm_ex)))
order(abs(residuals(lm_ba)))
order(abs(residuals(lm_la)))
order(abs(residuals(lm_em)))
order(abs(residuals(lm_po)))

summary(lm_ex)
summary(lm_ba)
summary(lm_la)
summary(lm_em)
summary(lm_po)

plot(1:length(cleaned$crime), cooks.distance(lm_ex), type="b", main="Cook's distance of expend ~ crime")

# Post-processing of information
five_is_bad = rep(0, length(cleaned$crime))
five_is_bad[5] = 1
five_is_bad

test = lm(cleaned$expend ~ cleaned$crime + five_is_bad)
summary(test) # Resulting P-value of cleaned$crime = 0.0362, thus the coefficient of the explanatory variable is significantly different from 0, the outlier thus is significant


# Tests: - Won't be used.
# head(cleaned)
# model = lm(cleaned$lawyers~cleaned$crime)
# bm = order(abs(residuals(model)))
# bad = rep(0, length(cleaned$crime))
# bad[bm[length(bm)]] = 1
# added = lm(cleaned$lawyers~cleaned$crime+bad)
# added
# summary(added)
# 
# model = lm(cleaned$lawyers ~ cleaned$crime)
# qqnorm(residuals(model))


# This was a test to see what would happend when all other variables would be used.
#lmodel = lm(cleaned$crime ~ cleaned$expend + cleaned$bad + cleaned$lawyers + cleaned$employ + cleaned$pop)
#qqnorm(residuals(lmodel))

# Collinearity
round(cor(cleaned), 2)
pairs(cleaned)
# When looking at the numerical differences, we can see that: 
# -expend + lawyers (0.97)
# -expend + employ (0.98)
# -expend + pop (0.95)
# -bad + pop (0.92)
# -employ + lawyers (0.97)
# -pop + employ (0.97)
# are all high valued collinears


head(cleaned)
car::vif(lm(crime ~ expend+bad+lawyers+employ+pop, data = cleaned)) # Using all, big values.
car::vif(lm(crime ~ expend+bad+lawyers+pop, data = cleaned)) # Removed employ, lower values but still large
car::vif(lm(crime ~ bad+lawyers+pop, data = cleaned)) # Removed bad, lower values but pop is still 16
car::vif(lm(crime ~ bad+lawyers, data = cleaned)) # Removed pop, all values are < 5!

# This would then suggest that crime based on bad + lawyers would be a good setup.

#~~~~B~~~~#
#b)  Fit a linear regression model to the data. Use the step-up method to find the best model. Comment.


# Step-up is by starting with the base model and then slowly adding more variables
summary(lm(crime ~ expend, data = cleaned)) # 0.1119
summary(lm(crime ~ bad, data = cleaned)) # 0.01391
summary(lm(crime ~ lawyers, data = cleaned)) # 0.1408
summary(lm(crime ~ employ, data = cleaned)) # 0.09641
summary(lm(crime ~ pop, data = cleaned)) # 0.0759
# Thus, we add lawyers

summary(lm(crime ~lawyers + expend, data = cleaned)) # 0.154
summary(lm(crime ~lawyers + bad, data = cleaned)) # 0.1528
summary(lm(crime ~lawyers + employ, data = cleaned)) # 0.1807
summary(lm(crime ~lawyers + pop, data = cleaned)) # 0.1848 # NOTE: p=0.1139, thus we stop.
# Resulting model = crime ~ lawyers + pop, but because the p-values is small, the resulting formula is:
# 4.392e+03 + 3.177e-02 * lawyers + error, with R2 = 0.1408

#~~~~C~~~~# (WORK IN PROGRESS!)
#c)  Determine a 95% prediction interval for the expend using the model you preferred in b) for a (hypothetical) state with bad=50, crime=5000, lawyers=5000, employ=5000 and pop=5000. Can you improve this interval?
fitted(lm(crime ~ lawyers, data=cleaned))

