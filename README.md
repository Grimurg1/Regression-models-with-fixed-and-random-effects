# Regression-models-with-fixed-and-random-effects
PSYP13 (HT2017) Home assignment 

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")
describe(data_sample_1)

#The gender data has value of 3, delete.
newdata1 <- data_sample_1[-15,]
summary(newdata1)
describe(newdata1)

#The describe data shows that mindfulness variable has three variables under 1...24, 25 and 66. 
#Those have to be excluded.
newdata1<-newdata1[!newdata1$mindfulness<=1,] 

#Total of excluded participants until now are 4. 

#Hérna verð ég að snúa Mindfulness

#Plots

plot(age ~ pain, data = newdata1)
plot(sex ~ pain, data = newdata1)
plot(STAI_trait ~ pain, data = newdata1)
plot(pain_cat ~ pain, data = newdata1)
plot(cortisol_serum ~ pain, data = newdata1)
plot(cortisol_saliva ~ pain, data = newdata1)
plot(mindfulness ~ pain, data = newdata1)


#No obvious outliers

#Regression model 1

mod_pain1 = lm(pain ~ age + sex, data = newdata1)

plot(pain ~ age, data = newdata1)
abline(lm(pain ~ age, data = newdata1))
plot(pain ~ sex, data = newdata1)
abline(lm(pain ~ sex, data = newdata1))

#Looks like there are outliers in the sex variable. Use cook-distance. 
#The criteria is that if there are values greater than 1, it is an outlier, there was no value greater than 1.
cooks.distance(mod_pain1)
plot(mod_pain1, which = 4)

#run the regression model
mod_pain1
predict(mod_pain1)

#results and wheigts
summary(mod_pain1)
AIC(mod_pain1)
confint(mod_pain1)
lm.beta(mod_pain1)

#Model 1 =
#Residual standard error: 1.292 on 153 degrees of freedom
#Multiple R-squared:  0.1131,	Adjusted R-squared:  0.1015 
#F-statistic: 9.753 on 2 and 153 DF,  p-value: 0.0001031

#Regression model 2
mod_pain2<- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = newdata1)

#results and weights 2
summary(mod_pain2)
AIC(mod_pain2)
confint(mod_pain2)
lm.beta(mod_pain2)

#Residual standard error: 0.9929 on 148 degrees of freedom
#Multiple R-squared:  0.4932,	Adjusted R-squared:  0.4693 
#F-statistic: 20.58 on 7 and 148 DF,  p-value: < 2.2e-16


#Assumption checking (already checked for outliers)


#Normality of residuals 
hist( x = residuals( mod_pain2 ), xlab = "Value of residual", breaks = 20)
plot(mod_pain2, which = 2)
describe(residuals(mod_pain2))
#The residuals are normally distributed


#Linearity
#Pred vs Actual
pred <- predict( object = mod_pain2 )
plot( x = pred, y = newdata1$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
#Pred vs Residuals
plot(mod_pain2, which = 1)
residualPlots(mod_pain2)

#INTERPRET??


#Homogeneity of variance
plot(mod_pain2, which = 3)
ncvTest(mod_pain2)
#The variance of the residuals are constant


#Multicollinearity
vif(mod_pain2)
pairs.panels(newdata1[,c("pain", "age", "sex")], col = "red", lm = T)
pairs.panels(newdata1[,c("pain", "age", "sex", "cortisol_serum", "cortisol_saliva", "mindfulness", "STAI_trait", "pain_cat"),], col = "red", lm = T)

#Nothing wrong in model 1
#Residuals are independent of each other
#No "bad" outliers
#High correlation between cortisol variables. Take one out. Same same.

mod_pain3_without_cortisolserum <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = newdata1)

predict(mod_pain3_without_cortisolserum)

summary(mod_pain3_without_cortisolserum)
AIC(mod_pain3_without_cortisolserum)
confint(mod_pain3_without_cortisolserum)
lm.beta(mod_pain3_without_cortisolserum)

mod_pain4_without_cortisolsaliva <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = newdata1)

predict(mod_pain4_without_cortisolsaliva)

summary(mod_pain4_without_cortisolsaliva)
AIC(mod_pain4_without_cortisolsaliva)
confint(mod_pain4_without_cortisolsaliva)
lm.beta(mod_pain4_without_cortisolsaliva)

vif(mod_pain3_without_cortisolserum)
vif(mod_pain4_without_cortisolsaliva)
#Take out serum and keep saliva in > Saliva explains more of the variance and according to the article it is better than serum.

#assumption checking again, cortisol serum excluded

#Normality

plot(mod_pain3_without_cortisolserum, which = 2)
describe(residuals(mod_pain3_without_cortisolserum))
hist(residuals(mod_pain3_without_cortisolserum), breaks = 20)

#Linearity
#Pred vs Actual
pred <- predict( object = mod_pain3_without_cortisolserum )
plot( x = pred, y = newdata1$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
#Pred vs Residuals
plot(mod_pain3_without_cortisolserum, which = 1)
residualPlots(mod_pain2)

#ENN OG AFTUR, HVERNIG Á ÉG AÐ TÚLKA

#Homoscedasticty assumption (homogeneity of variance)
plot(mod_pain3_without_cortisolserum, which = 3)
ncvTest(mod_pain3_without_cortisolserum)

vif(mod_pain3_without_cortisolserum)
pairs.panels(newdata1[,c("pain", "age", "sex")], col = "red", lm = T)

#Compare with ANOVA and AIC
anova(mod_pain1, mod_pain3_without_cortisolserum)

AIC(mod_pain1)
AIC(mod_pain3_without_cortisolserum)
help(AIC)

################################ASSIGNMENT2#######################################

back_model <- lm(pain ~ age+ sex+ STAI_trait + pain_cat + cortisol_serum + mindfulness + weight, data = newdata1)
step(back_model)
summary(back_model)
describe(back_model)
describe(newdata1)
summary(newdata1)
#No problem with the new variable weight

back_model1 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva + mindfulness + weight, data = newdata1)
step(back_model1)
#Starting the backward regression

back_model1 <- lm(pain ~ age+ sex+ STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = newdata1)

step(back_model1)

back_streets_back <- lm(pain ~ age + pain_cat + cortisol_saliva + mindfulness, data = newdata1)

step(back_streets_back)

summary(back_streets_back)

#Compare back_model1 and back_streets_back)
AIC(back_model1,back_streets_back)
anova(back_model1,back_streets_back)

#Back_streets_back is better
confint(back_streets_back) 
lm.beta(back_streets_back)

#Compare back_streets_back with mod_pain3_without_cortisolserum

AIC(mod_pain3_without_cortisolserum, back_streets_back)

Anova(mod_pain3_without_cortisolserum,back_streets_back)

anova(mod_pain3_without_cortisolserum,back_streets_back)

#apply to new data

home_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")
#Check if everything is alright
home_sample1<-home_sample_2[!home_sample_2$mindfulness<=1,] 

pred_test <- predict(mod_pain3_without_cortisolserum, home_sample1 )
pred_test_back <- predict(back_streets_back, home_sample1 )

RSS_test = sum((home_sample1["pain"] - pred_test)^2)
RSS_test_back = sum((home_sample1["pain"] - pred_test_back)^2)
RSS_test
RSS_test_back

AIC(mod_pain3_without_cortisolserum)
AIC(back_streets_back)

back_streets_back <- lm(pain ~ age + pain_cat + cortisol_saliva + mindfulness, data = home_sample1)
back_model <- lm(pain ~ age+ sex+ STAI_trait + pain_cat + cortisol_saliva + mindfulness + weight, data = home_sample1)

summary(back_streets_back)
summary(back_model1)

##########################ASSIGNMENT3######################################

home_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")

# the repeated varibales in the dataset
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")

#descriptives
describe(home_sample_3)
table(home_sample_3)

# histograms #Lítið hægt að gera við þessu, allt skewed. Þarf ekki að vera normal. Við erum í fínum málum ef errorarnir eru normally distributaðir.
hist(home_sample_3$pain_cat, breaks = 15)
hist(home_sample_3$cortisol_serum, breaks = 20) 
hist(home_sample_3$STAI_trait, breaks = 20) 
hist(home_sample_3$cortisol_saliva, breaks = 20) 
hist(home_sample_3$mindfulness, breaks = 20) 
hist(home_sample_3$weight, breaks = 20) 
hist(home_sample_3$pain1, breaks = 20) 
hist(home_sample_3$pain2, breaks = 20) 
hist(home_sample_3$pain3, breaks = 20) 
hist(home_sample_3$pain4, breaks = 20) 


###########################################################
#           Transform wide to long format                 #
###########################################################

# id.vars should be all non-repeated variables
data_pain_long = melt(home_sample_3, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating")

# order data frame by participant ID(not necessary, just makes the dataframe look more intuitive)
data_pain_long = data_pain_long[order(data_pain_long[,"ID"]),]

# change the time variable to a numerical vector
data_pain_long$time = as.numeric(data_pain_long$time)

data_pain_long


###########################################################
#                        Analysis                         #
###########################################################

mod_rep_int = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + (1|ID), data = data_pain_long)
mod_rep_slope = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + (time|ID), data = data_pain_long)
summary(mod_rep_int)
summary(mod_rep_slope) 

#On to r2
r2beta(mod_rep_int, method="nsj")
r2beta(mod_rep_slope, method="nsj")

### model comparison to see whether to use random slope or random intercept models
## plot the regression line (prediction)
# save the predictions of bot models to variables
data_pain_long$pred_int = predict(mod_rep_int)
data_pain_long$pred_slope = predict(mod_rep_slope)

# random intercept model
  ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
    geom_point(size = 3)+
    geom_line(color='red', aes(y=pred_int, x=time))+
    facet_wrap( ~ ID, ncol = 5)
# random slope and intercept model
  ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
    geom_point(size = 3)+
    geom_line(color='red', aes(y=pred_slope, x=time))+
    facet_wrap( ~ ID, ncol = 5)

# compare models with cAIC
# not too different
cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic

# Random slope and intercept is better (mod_rep_slope = 175.8321)

#Rule of thumb, if the difference doesnt reach 2, the prdictive ability is the same. if then, use the random intercept.
#The difference is more than 2, therefore we go with random slope and intercept

# compare models with anova
anova(mod_rep_int, mod_rep_slope)

# Chisq(2/13) = 20.732, p = 3.149e-05 *** Signif. codes:  0 ‘***’ #BREEEEEYYYTA
# cAIC values differ significantly from one another

# if there is not too much benefit for the random slope model,
# use the one with only random intercept in it

#There is a benefit from the slope we do further analysis with this model 
### adding a quadratic term of time to the slope model
# to account for curved relationship between time and wound rating
mod_rep_slope_quad = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + I(time^2)+ (time|ID), data = data_pain_long)

#summary
summary(mod_rep_slope_quad)

#adjusted r- squared 

r2beta(mod_rep_slope_quad, method="nsj")


## plot the results
# save prediction of the model to new variable
data_pain_long$pred_slope_quad = predict(mod_rep_slope_quad)

# random slope model
  ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
    geom_point(size = 3)+
    geom_line(color='red', aes(y=pred_slope_quad, x=time))+
    facet_wrap( ~ ID, ncol = 5)

# compare models with cAIC
cAIC(mod_rep_slope)$caic #175.8321
cAIC(mod_rep_slope_quad)$caic #121.9584

# compare models with anova
anova(mod_rep_slope, mod_rep_slope_quad)
# Chisq(1/14) = 29.885 , p = 4.584e-08 *** Signif. codes:  0 ‘***’ 


#Assumptions 
  
#outliers
influence(mod_rep_slope_quad, group = "ID")$alt.fixed 
influence(mod_rep_slope_quad, obs = T)$alt.fixed 
  
###if there would be a very influential case, you could spot it by having a look at the data (I(time^2))
##varibility of the cases are really small --> so no influential outlier

# normality assumption
# QQ plot
qqmath(mod_rep_slope_quad, id=0.05) 
# this might require the lattice package, but you can get the same graph wit the qqnorm() function

# linearity assumption
# linearity of prediction and standardized residuals
plot(mod_rep_slope_quad)
#linearity of each predictor and the standardized residual
# visualize the linearity of connection of each predictor
predictors=c("sex","age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")

for(i in 1:length(predictors)){
      predictor_to_test = data_pain_long[,predictors[i]]
      print(ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_rep_slope_quad,type="pearson")),
                             aes(x=x,y=pearson)) +
              geom_point() +
              geom_smooth(method = 'loess') +
              theme_bw()
        )
    }

# homoscedasticty assumption (homogeneity of variance)
# look for funnel shape on this graph
plot(mod_rep_slope_quad)
# Levens model for testing the for heteroscedasticity, from here: http://ademos.people.uic.edu/Chapter18.html
# look at the overall model F and p, if it is significant, there may be heteroscedasticity
summary(lm(residuals(mod_rep_slope_quad)^2 ~ data_pain_long[,"ID"]))
# no significant value, homoscedasticty not violated 
# multicollinearity
# there are some functions out there, but for now just look at the correlation matrix
# some example of a function designed to extract vif from lmer: https://raw.githubusercontent.com/aufrank/R-hacks/master/mer-utils.R
pairs.panels(data_pain_long, col = "red", lm = T)

pairs.panels(data_pain_long[,c("sex","age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")], col = "red", lm = T)
# no high correlations between the variables, no need to exclude any

