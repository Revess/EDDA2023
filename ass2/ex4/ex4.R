setwd("~/projects/EDDA2023/ass2/ex4")
df = read.csv("../data/coups.txt",sep=" ")

# A
dfglm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim, family=poisson, data=df)
summary(dfglm)
'''
Call:
glm(formula = miltcoup ~ oligarchy + pollib + parties + pctvote + 
    popn + size + numelec + numregim, family = poisson, data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.3443  -0.9542  -0.2587   0.3905   1.6953  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)   
(Intercept) -0.5102693  0.9053301  -0.564  0.57301   
oligarchy    0.0730814  0.0345958   2.112  0.03465 * 
pollib      -0.7129779  0.2725635  -2.616  0.00890 **
parties      0.0307739  0.0111873   2.751  0.00595 **
pctvote      0.0138722  0.0097526   1.422  0.15491   
popn         0.0093429  0.0065950   1.417  0.15658   
size        -0.0001900  0.0002485  -0.765  0.44447   
numelec     -0.0160783  0.0654842  -0.246  0.80605   
numregim     0.1917349  0.2292890   0.836  0.40303   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 65.945  on 35  degrees of freedom
Residual deviance: 28.668  on 27  degrees of freedom
AIC: 111.48

Number of Fisher Scoring iterations: 6

The oligarchy, pollib, and parties are statistically significant at the 0.05 level in predicting the count of military coups.
Specifically, a one-unit increase in oligarchy is associated with a 7.3% increase in the count of military coups, while a 
one-unit increase in pollib is associated with a 71.3% decrease in the count of military coups, and a one-unit increase in 
parties is associated with a 3.1% increase in the count of military coups.
On the other hand, the variables pctvote, popn, size, numelec, and numregim are not found to be statistically significant 
predictors of miltcoup. The overall model shows a good fit with a residual deviance of 28.668 on 27 degrees of freedom, 
which indicates that the model explains a significant portion of the variance in the data. However, the intercept term is 
not statistically significant, which suggests that there may be other factors that could influence the count of military 
coups that are not captured by the model.
'''

# B
dfglm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim, family=poisson, data=df)
summary(dfglm)
# Remove numelec since it is the highest p-value
dfglm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numregim, family=poisson, data=df)
summary(dfglm)
# Remove numregim since it is the highest p-value
dfglm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size, family=poisson, data=df)
summary(dfglm)
# Remove size since it is the highest p-value
dfglm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn, family=poisson, data=df)
summary(dfglm)
# Remove popn since it is the highest p-value
dfglm=glm(miltcoup~oligarchy+pollib+parties+pctvote, family=poisson, data=df)
summary(dfglm)
# Remove pctvote since it is the highest p-value
dfglm=glm(miltcoup~oligarchy+pollib+parties, family=poisson, data=df)
summary(dfglm)
# Remove size since it is the highest p-value

'''
Call:
glm(formula = miltcoup ~ oligarchy + pollib + parties, family = poisson, 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.3583  -1.0424  -0.2863   0.6278   1.7517  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.251377   0.372689   0.674  0.50000    
oligarchy    0.092622   0.021779   4.253 2.11e-05 ***
pollib      -0.574103   0.204383  -2.809  0.00497 ** 
parties      0.022059   0.008955   2.463  0.01377 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 65.945  on 35  degrees of freedom
Residual deviance: 32.856  on 32  degrees of freedom
AIC: 105.66

Number of Fisher Scoring iterations: 5


The Poisson regression model has been used to predict the miltcoup variable using the oligarchy, pollib, and parties predictor variables. 
The model has a residual deviance of 32.856 on 32 degrees of freedom, indicating that the model fits the data well. 
The AIC (Akaike Information Criterion) value is 105.66, which is a measure of the quality of the model, with lower values indicating better fit.
The coefficients in the model indicate that oligarchy and parties have a positive effect on miltcoup, while pollib has a negative effect.
The p-values associated with these coefficients indicate that oligarchy and pollib are statistically significant predictors of miltcoup at the 0.01 level, 
while parties is significant at the 0.05 level.
Overall, these results suggest that countries with higher levels of oligarchy and political parties are more likely to experience military coups, 
while those with higher levels of political liberalism are less likely to experience coups. 
However, it\'s important to note that correlation does not imply causation, and further research is needed to establish causal relationships between these variables.
'''

# C
avg_data <- data.frame(
  oligarchy = mean(df$oligarchy),
  pollib = mean(df$pollib), 
  parties = mean(df$parties)
)

predict(dfglm,newdata=avg_data)
'''
Given the average of the previous countries any the chance of a given country having a coup is 0.1710151
'''