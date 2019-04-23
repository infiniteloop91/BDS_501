library(ggplot2)
library(plm)
library(reshape)
library(stargazer)
library(ggplot2)
library(lmtest)
library(tseries)

##### POWER CALC #####
##library(pwr)
###pooled_std <- sqrt((0.343925127^2 + 0.281999662^2)/2) 

##pwr.t.test(d=(4.869047619-4.642857143 )/pooled_std,power=.8,sig.level=.05,
##           type="two.sample",alternative="two.sided")

#/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_ANCHOR_7.csv 
#/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_ANCHOR_14.csv
#/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_CONTROL_7.csv
#/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_CONTROL_14.csv
#/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_DEFfAULT_7.csv
#/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_DEFAULT_14.csv


#load the data 
DEFAULT_7 <- read.csv("/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_DEFFAULT_7.csv")
DEFAULT_14 <- read.csv("/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_DEFAULT_14.csv")
CONTROL_7 <- read.csv("/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_CONTROL_7.csv")
CONTROL_14 <- read.csv("/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_CONTROL_14.csv")
ANCHOR_7 <- read.csv("/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_ANCHOR_7.csv")
ANCHOR_14 <- read.csv("/Users/ericlyons/Desktop/SLIPPERY_SLOPE/PANEL_ANCHOR_14.csv")


#build some models 

##### DEFAULT 7 ROUNDS #####
with_model_DEFAULT_7 <- plm(Roll~Age+Gender+Round, data = DEFAULT_7, model = "within", index = "ResponseId")
rand_model_DEFAULT_7 <- plm(Roll~Age+Gender+Round, data = DEFAULT_7, model = "random", index = "ResponseId")

### SHOW DEFAULT 7 ROUNDS MODELS ####
summary(with_model_DEFAULT_7)
summary(rand_model_DEFAULT_7)

#### DEFAULT 14 ROUNDS ######
with_model_DEFAULT_14 <- plm(Roll~Age+Gender+Round, data = DEFAULT_14, model = "within", index = "ResponseId")
rand_model_DEFAULT_14 <- plm(Roll~Age+Gender+Round, data = DEFAULT_14, model = "random", index = "ResponseId")


### SHOW DEFAULT 14 ROUNDS MODELS ####
summary(with_model_DEFAULT_14)
summary(rand_model_DEFAULT_14)

#### CONTROL 7 ROUNDS #####
with_model_CONTROL_7 <- plm(Roll~Age+Gender+Round, data = CONTROL_7, model = "within", index = "Subject")
rand_model_CONTROL_7 <- plm(Roll~Age+Gender+Round, data = CONTROL_7, model = "random", index = "Subject")

### SHOW CONTROL 7 ROUNDS MODELS ####
summary(with_model_CONTROL_7)
summary(rand_model_CONTROL_7)

##### CONTROL 14 ROUNDS ####
with_model_CONTROL_14 <- plm(Roll~Age+Gender+Round, data = CONTROL_14, model = "within", index = "ResponseId")
rand_model_CONTROL_14 <- plm(Roll~Age+Gender+Round, data = CONTROL_14, model = "random", index = "ResponseId")

### SHOW CONTROL 14 ROUNDS MODELS ####
summary(with_model_CONTROL_14)
summary(rand_model_CONTROL_14)


#### ANCHOR 7 ROUNDS #####
with_model_ANCHOR_7 <- plm(Roll~Age+Gender+Round, data = ANCHOR_7, model = "within", index = "Subject")
rand_model_ANCHOR_7 <- plm(Roll~Age+Gender+Round, data = ANCHOR_7, model = "random", index = "Subject")

### SHOW ANCHOR 7 ROUNDS MODELS ####
summary(with_model_ANCHOR_7)
summary(rand_model_ANCHOR_7)

##### ANCHOR 14 ROUNDS ####
with_model_ANCHOR_14 <- plm(Roll~Age+Gender+Round, data = ANCHOR_14, model = "within", index = "ResponseId")
rand_model_ANCHOR_14 <- plm(Roll~Age+Gender+Round, data = ANCHOR_14, model = "random", index = "ResponseId")

### SHOW ANCHOR 14  ROUNDS MODELS ####
summary(with_model_ANCHOR_14)
summary(rand_model_ANCHOR_14)

##### ANOVA of means #####
CONTROL_7_LIST <- c(4.388888889,	4.611111111,	4.5,	4.5,	3.833333333,	4.277777778,	4.444444444)							
CONTROL_14_LIST <-	c(4.3125,	5,	4.5,	4.375,	4.5625,	4.75,	4.125,	4.0625,	4.625,	4.125,	4.3125,	3.8125,	4.6875,	4.75)
ANCHOR_7_LIST <-	c(5.111111111,	5.055555556,	4.888888889,	5.222222222,	4.888888889,	4.555555556,	4.833333333)							
ANCHOR_14_LIST <-	c(5.15,	5,	4.75,	5.3,	5,	5.05,	5.1,	4.95,	4.95,	4.65,	5,	4.65,	4.7,	5.1)
DEFAULT_7_LIST <-	c(4.875,	5.25,	5.1875,	5.125,	5.375,	4.8125,	5.4375)							
DEFAULT_14_LIST <-	c(4.736842105,	5.052631579,	4.947368421,	5.157894737,	5.157894737,	5.052631579,	4.421052632,	4.578947368,	4.578947368,	4.263157895,	4.789473684,	4.789473684,	4.947368421,	4.684210526)

combined <- data.frame(cbind(CONTROL_7_LIST,CONTROL_14_LIST,ANCHOR_7_LIST,ANCHOR_14_LIST,DEFAULT_7_LIST,DEFAULT_14_LIST))
stacked <- stack(combined)
anova_output <- aov(values ~ ind, data = stacked)
summary(anova_output)
#F(5,78) = 21.76 p<.05

### GRANGER ####
index_7 <- c(1,2,3,4,5,6,7)
combined_7 <- data.frame(cbind(index_7,CONTROL_7_LIST,ANCHOR_7_LIST,DEFAULT_7_LIST))
index_14 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
combined_14 <- data.frame(cbind(index_14,CONTROL_14_LIST,ANCHOR_14_LIST,DEFAULT_14_LIST))

#### CONTROL LIST ####
grangertest(combined_7$index_7 ~ combined_7$CONTROL_7_LIST, order = 1, data=combined_7)
grangertest(combined_14$index_14 ~ combined_14$CONTROL_14_LIST, order = 1, data=combined_14)

#### Try some basic regressions #####
control7lm <- lm(CONTROL_7_LIST~index_7, data = combined_7)
control14lm <- lm(CONTROL_14_LIST~index_14, data = combined_14)
summary(control7lm)
summary(control14lm)
#### ANCHOR LIST #####
grangertest(combined_7$index_7 ~ combined_7$ANCHOR_7_LIST, order = 1, data=combined_7)
grangertest(combined_14$index_14 ~ combined_14$ANCHOR_14_LIST, order = 1, data=combined_14)

#### Try some basic regressions #####
anchor7lm <- lm(ANCHOR_7_LIST~index_7, data = combined_7)
anchor14lm <- lm(ANCHOR_14_LIST~index_14, data = combined_14)
summary(anchor7lm)
summary(anchor14lm)
### DEFAULT LIST ####
grangertest(combined_7$index_7 ~ combined_7$DEFAULT_7_LIST, order = 1, data=combined_7)
grangertest(combined_14$index_14 ~ combined_14$DEFAULT_14_LIST, order = 1, data=combined_14)

#### Try some basic regressions #####
default7lm <- lm(DEFAULT_7_LIST~index_7, data = combined_7)
default14lm <- lm(DEFAULT_14_LIST~index_14, data = combined_14)
summary(default7lm)
summary(default14lm)

