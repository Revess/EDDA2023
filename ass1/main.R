# Exercise 4
# a
library(MASS)
library(dplyr)
dat <- npk
dat$plot1 <- sample(seq(1,24),24)
dat$plot2 <- sample(seq(1,24),24)
for(i in 1:nrow(dat)){
  if(dat$plot1[i] == dat$plot2[i]){
    print('reshuffling')
    a <- seq(1,24)
    sam <- sample(a[a != i],1)
    new <- dat$plot2[sam]
    dat$plot2[sam] <- dat$plot2[i]
    print(new)
    print(dat$plot2[i])
    dat$plot2[i] <- new
  }
}

# b
grouped <- dat %>% 
  group_by(block, N) %>%
  summarise(mean = mean(yield))
boxplot(mean~N, data = grouped, main = 'Mean of yields per block with nitrogen and without nitrogen treatment', xlab = 'Nitrogen treatment', ylab = 'Yield in pounds')

# The factor of block ensures that all treatments get averaged and that they are not thrown on one pile during calculation

# c
dataov <- lm(yield ~ block*N, data = dat) 
anova(dataov)
# The p-value for the interaction is 0.47. We cannot reject the statement that there is no interaction between the factors. Therefore it does not make sense in include the block factor
