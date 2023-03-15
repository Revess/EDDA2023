########## START WITH RUNNING THE FOLLOWING COMMAND: ##########
#install.packages('car')
#install.packages('glmnet')
library('car')
#library('glmnet')


#df = read.csv("~/projects/EDDA2023/ass1/datasets/birthweight.txt", sep=" ")
expensescrime = read.csv("C:/Users/markd/Code/EDDA2023/ass2/datasets/expensescrime.txt",  sep=" ")
#expensescrime_rounded = round()
cleaned = expensescrime[,-1]
hist(cleaned$expend)

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
#pairs(expensescrime[,c(2,4,6,7)]) # Subset

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
qqline(residuals(lm_total))


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
head(expensescrime)

# Collinearity
round(cor(cleaned), 2)
# pairs(cleaned) # This one is also in one of the first lines.
# When looking at the numerical differences, we can see that: 
# -expend + lawyers (0.97)
# -expend + employ (0.98)
# -expend + pop (0.95)
# -bad + pop (0.92)
# -employ + lawyers (0.97)
# -pop + employ (0.97)
# are all high valued collinears


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
                                        # R - R^2
summary(lm(expend ~ bad, data=cleaned)) # 0.6964 - 0.6902
summary(lm(expend ~ crime, data=cleaned)) # 0.1119 - 0.09373
summary(lm(expend ~ lawyers, data = cleaned)) # 0.9373 - 0.936 
summary(lm(expend ~ employ, data = cleaned)) # 0.954 - 0.953 
summary(lm(expend ~ pop, data = cleaned)) #  0.9073 - 0.9054 
# Thus, we add employ

summary(lm(expend ~ employ + bad, data=cleaned)) #Multiple R-squared:  0.9551,	Adjusted R-squared:  0.9532 
summary(lm(expend ~ employ + crime, data=cleaned)) # Multiple R-squared:  0.9551,	Adjusted R-squared:  0.9532 
summary(lm(expend ~ employ + lawyers, data=cleaned)) # Multiple R-squared:  0.9632,	Adjusted R-squared:  0.9616 
summary(lm(expend ~ employ + pop, data=cleaned)) # Multiple R-squared:  0.9543,	Adjusted R-squared:  0.9524 
# Thus we add lawyers, since it's R-value is low and the P-value is 0.00113!

summary(lm(expend ~ employ + lawyers + bad, data=cleaned)) # Multiple R-squared:  0.9639,	Adjusted R-squared:  0.9616 
summary(lm(expend ~ employ + lawyers + crime, data=cleaned))# Multiple R-squared:  0.9632,	Adjusted R-squared:  0.9608 
summary(lm(expend ~ employ + lawyers + pop, data=cleaned)) # Multiple R-squared:  0.9637,	Adjusted R-squared:  0.9614 
# At this point, any of the added variables will be insignificant, as the addition of these will make their p-values > 0.05.
# This means that the final model is (expend ~ employ + lawyers)

#~~~~C~~~~#
#c)  Determine a 95% prediction interval for the expend using the model you preferred in b) for a (hypothetical) state with
# bad=50, crime=5000, lawyers=5000, employ=5000 and pop=5000. Can you improve this interval?
model = lm(expend ~ employ + lawyers, data=cleaned) 
fitted(model)

newxdata = data.frame(bad=50,crime=5000, lawyers=5000, employ=5000, pop=5000)
predict(model, newxdata, interval = 'prediction', level = 0.95)
#      fit       lwr      upr
# 172.2098 -302.9307 647.3504

# Can you improve this interval? - We can improve this interval by using the 'confidence' interval.
predict(model, newxdata, interval = 'confidence', level = 0.95)

#~~~~D~~~~#
# Apply the LASSO method to choose the relevant variables (with default parameters as in the lecture and lambda=lambda.1se).
# (You will need to install the R-package glmnet, which is not included in the standard distribution of R.)
# Compare the resulting model with the model obtained in b).
# (Beware that in general a new run delivers a new model because of a new train set.)
library(glmnet) 

# Share X and Y:
x=as.matrix(cleaned[,-1])
y=cleaned[,1]

# Train and test set + predict
train=sample(1:nrow(x),0.67*nrow(x)) 
x.train=x[train,]; y.train=y[train]  
x.test=x[-train,]; y.test = y[-train]
lm.model=lm(expend ~ employ + lawyers,data=cleaned,subset=train)
y.predict.lm=predict(lm.model,newdata=cleaned[-train,])

# Mean-squared error
mse.lm=mean((y.test-y.predict.lm)^2); mse.lm

# Lasso!
lasso.model=glmnet(x.train,y.train,alpha=1)
lasso.cv=cv.glmnet(x.train,y.train,alpha=1,type.measure="mse",nfolds=5)

# Plots
plot(lasso.model,label=T,xvar="lambda")
plot(lasso.cv) 

# Find out which variables are relevant:
lambda.min=lasso.cv$lambda.min; lambda.1se=lasso.cv$lambda.1se; 
lambda.min; lambda.1se # best lambda by cross validation
coef(lasso.model,s=lasso.cv$lambda.min) # cyl,hp,wt,am and carb are relevant
coef(lasso.model,s=lasso.cv$lambda.1se) # only cyl,hp and wt are releveant


# The model found was:
#> coef(lasso.model,s=lasso.cv$lambda.min) # cyl,hp,wt,am and carb are relevant
#6 x 1 sparse Matrix of class "dgCMatrix"
#s1
#(Intercept) -52.66989609
#bad           .         
#crime         .         
#lawyers       0.06307596
#employ        0.00233929
#pop           0.01403667

#> coef(lasso.model,s=lasso.cv$lambda.1se) # only cyl,hp and wt are releveant
#6 x 1 sparse Matrix of class "dgCMatrix"
#s1
#(Intercept) 1.227297e+02
#bad         .           
#crime       .           
#lawyers     5.897375e-02
#employ      3.396696e-05
#pop         .           

# When comparing that to the variables found in B), 
# mse: lawyers + employ + pop
# 1se: we can see that employ + lawyers are the same variables found
