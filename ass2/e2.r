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

qqnorm(expensescrime$expend, main="Expend") # This seems to be the only normal one                                   (;))
qqline(expensescrime$expend, main="Expend") # This seems to be the only normal one                                   (;))
hist(expensescrime$expend)

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

###################################################### Good till here.
lm_total = lm(expend ~ bad+crime+lawyers+employ+pop, data=cleaned)
qqnorm(residuals(lm_total))


# Influence points
lm_ba = lm(cleaned$expend ~ cleaned$bad)
lm_cr = lm(cleaned$expend ~ cleaned$crime)
lm_la = lm(cleaned$expend ~ cleaned$lawyers)
lm_em = lm(cleaned$expend ~ cleaned$employ)
lm_po = lm(cleaned$expend ~ cleaned$pop)
## Could have the norms of each variable added.

# Displays the outlier - Always results in 5 being the worst (expect for lm_ba)
qqplot(1:length(cleaned$expend), residuals(lm_total), main ="Residuals on outliers")
order(abs(residuals((lm_total))))
# This shows that there are 2 main outliers, the ones at -600 and the +600, which can also be shown by the following hist:
hist(residuals(lm_total))

# Removing these yields:
remove_bad_ones = rep(0, length(cleaned$expend))
absolute = order(abs(residuals(lm_total)))
remove_bad_ones[absolute[length(absolute)]] = 1
remove_bad_ones

fixed = lm(expend ~ bad+crime+lawyers+employ+pop + remove_bad_ones, data=cleaned)
summary(fixed) # remove_bad_ones has a p-value of 3.63e-10, meaning that 5 is a significant outlier!


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
car::vif(lm(expend ~ crime+bad+lawyers+employ+pop, data = cleaned)) # Using all, big values.
# Results in:
#crime       bad   lawyers    employ       pop 
#1.487978  8.364321 16.967470 33.591361 32.937517 

car::vif(lm(expend ~ crime+bad+lawyers+pop, data = cleaned)) # Removed employ
# Results in:
#crime       bad   lawyers       pop 
#1.487593  8.243504 10.084215 20.680738 

car::vif(lm(expend ~ crime+bad+lawyers, data = cleaned)) # Removed pop
# Results in:
#   crime      bad  lawyers 
#1.180356 3.293266 3.299667

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
#c)  Determine a 95% prediction interval for the expend using the model you preferred in b) for a (hypothetical) state with
# bad=50, crime=5000, lawyers=5000, employ=5000 and pop=5000. Can you improve this interval?
model = lm(crime ~ lawyers, data=cleaned) 
fitted(model)
newxdata = data.frame(bad=50,crime=5000, lawyers=5000, employ=5000, pop=5000)
#predict(model, data.frame(lawyers=1))
predict(model, newxdata, interval = 'prediction', level = 0.95)



