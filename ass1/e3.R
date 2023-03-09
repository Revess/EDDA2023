'''
To investigate the effect of 3 types of diet, 78 persons were divided randomly in 3 groups, 
the first group following diet 1, second group diet 2 and the third group diet 3. 
Next to some other characteristics, the weight was measured before diet and after 6 weeks 
of diet for each person in the study. The collected data is summarized in the 
data frame diet.txt Download diet.txtwith the following columns: 
person - participant number, 
gender - gender (1 = male, 0 = female), 
age - age (years), 
height - height (cm), 
preweight - weight before the diet (kg), 
diet - the type of diet followed, 
weight6weeks - weight after 6 weeks of diet (kg). 
Compute and add to the data frame the variable weight.lost expressing the lost weight, 
to be used as response variable.

a)  Make an informative graphical summary of the data relevant for study of the effect of diet 
on the wight loss. By using only the columns preweight and weight6weeks, test the claim that the 
diet affects the weight loss. Check the assumptions of the test applied.

b)  Apply one-way ANOVA to test whether type of diet has an effect on the lost weight. Do all three types 
diets lead to weight loss? Which diet was the best for losing weight? Can the Kruskal-Wallis test be applied 
for this situation?

c)  Use two-way ANOVA to investigate effect of the diet and gender (and possible interaction) on the lost weight.

d)  Apply an appropriate model to investigate effects of diet and height (and possibly their interaction) on the 
lost weight. Is the effect of height the same for all 3 types of diet?

e)  Which of the two approaches, the one from b) or the one from d), do you prefer? Why? For the preferred model, 
predict the lost weight for all three types of diet for an average person.
'''

df = read.csv("~/projects/EDDA2023/ass1/datasets/diet.txt", sep="\t")
df$weightdiff = c(df["preweight"] - df["weight6weeks"])$preweight
#~~~A~~~#
# Then somehow combine these into a histogram
t.test(df[df$diet == 1,]$preweight, df[df$diet == 1,]$weight6weeks)
'''data:  df[df$diet == 1, ]$preweight and df[df$diet == 1, ]$weight6weeks
t = 1.3623, df = 46, p-value = 0.1797
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -1.575829  8.175829
sample estimates:
mean of x mean of y 
   72.875    69.575'''

t.test(df[df$diet == 2,]$preweight, df[df$diet == 2,]$weight6weeks)
'''data:  df[df$diet == 2, ]$preweight and df[df$diet == 2, ]$weight6weeks
t = 1.0948, df = 51.992, p-value = 0.2787
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -2.520349  8.572201
sample estimates:
mean of x mean of y 
 71.11111  68.08519'''

t.test(df[df$diet == 3,]$preweight, df[df$diet == 3,]$weight6weeks)
'''data:  df[df$diet == 3, ]$preweight and df[df$diet == 3, ]$weight6weeks
t = 2.385, df = 51.668, p-value = 0.02078
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.816050 9.480246
sample estimates:
mean of x mean of y 
 73.62963  68.48148'''

# OR IT IS JUST ONE TEST????
t.test(df$preweight, df$weight6weeks)  ##And is the alt less than? , alt="less"
'''data:  df$preweight and df$weight6weeks
t = 2.721, df = 153.92, p-value = 0.007259
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 1.053396 6.636348
sample estimates:
mean of x mean of y 
 72.52564  68.68077'''

#~~~B~~~#
library(dplyr)
aov(weightdiff ~ diet, data=df)
anova(lm(weightdiff ~ diet, data=df))

summary(lm(weightdiff ~ diet, data=df))

kruskal.test(weightdiff ~ diet, data=df)


res = lm( weightdiff ~ diet , data=df )
anova ( res )
summary( res )

varietdf = data.frame(yield=as.vector(as.matrix(df$weightdiff)), variety=factor(df$diet))
res = lm(yield ~ variety, data=varietdf)
anova(res)
summary(res)

#~~~C~~~#
aov(weightdiff ~ gender + age, data=df)

#~~~D~~~#
kruskal.test(df[df$diet == 1,]$weightdiff ~ df[df$diet==1,]$height)
kruskal.test(df[df$diet == 2,]$weightdiff ~ df[df$diet==2,]$height)
kruskal.test(df[df$diet == 3,]$weightdiff ~ df[df$diet==3,]$height)
# BC it gives a p-value

#~~~E~~~#
# BC it gives a p-value
kruskal.test(df[df$diet == 1,]$preweight ~ df[df$diet==1,]$weight6weeks)
kruskal.test(df[df$diet == 2,]$preweight ~ df[df$diet==2,]$weight6weeks)
kruskal.test(df[df$diet == 3,]$preweight ~ df[df$diet==3,]$weight6weeks)

diet1 = df[df$diet == 1,]
diet2 = df[df$diet == 2,]
diet3 = df[df$diet == 3,]
mean(diet1$weightdiff, na.rm=TRUE)
mean(diet2$weightdiff, na.rm=TRUE)
mean(diet3$weightdiff, na.rm=TRUE)

summary(lm(weightdiff ~ diet, data=diet1))
summary(lm(weightdiff ~ diet, data=diet2))
summary(lm(weightdiff ~ diet, data=diet3))



summary(mean_sales = mean(df$weightdiff, na.rm=TRUE))
