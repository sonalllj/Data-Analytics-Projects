#Load the data
load('nyse.RData')

#First five rows
head(nyse)

#Create a subset of the data frame that only contains stock values
nyse.sub <- nyse[,-c(1:4)]
nyse.sub

#Create a vector of the correlations between the stock values.
nyse.cors <- cor(nyse.sub)[,1]
#Check that the function worked by comparing it with a single exmaple
#correlation
cor(nyse$GOOGL, nyse$AAPL)
#Remove Google's correlation of 1 with itself
nyse.cors <- nyse.cors[-1]

#Order the correlations in descending order of strength
nyse.cors.order <- nyse.cors[order(nyse.cors,decreasing=TRUE)]
#Convert to absolute values of correlations and also order these
nyse.cors.abs.order <- abs(nyse.cors)[order(abs(nyse.cors),decreasing=TRUE)]
nyse.cors.abs.order
#Even though the question asks for the top 5 correlated companies with Google,
#the top 7 correlations range from 0.64 to 0.90, then the next five highest
#range between 0.57 to 0.60. This means that including any of these next five
#independent variables wouldn't add significantly to the variation that the
#model can explain already with the top 7 (CDE, AMZN, SAM, GIL, MSFT, M, DPZ).




#From the second barplot we can see the 5 companies whose stock prices
#have the strongest correlation with Google are Couer Mining, Amazon,
#Boston Beer Company, Gidlan Activewear and Microsoft.
mod.stocks <- lm(GOOGL~CDE+AMZN+SAM+GIL+MSFT,data=nyse)
summary(mod.stocks) #adj r2 is 0.8417
#plot(nyse.sub$GOOGL)
#abline(mod.stocks)

#create model with intercept only to have some comparison
mod.base <- lm(GOOGL~CDE, data=nyse)
summary(mod.base)
#R^2=0.8039, Adj R^2=0.8037
#not that much worse, maybe we have redundant variables


#Try the leaps thing
#Get a subset with those variables
nyse5 = data.frame(nyse)
#keep only DV and top 5 IVs
install.packages("dplyr")
library(dplyr)
nyse5= nyse5 %>% select(GOOGL, CDE, AMZN, SAM, GIL, MSFT)
attributes(nyse5)

#run the leaps
install.packages("leaps")
library(leaps)
best_subset <- leaps(x = nyse5[, 2:6], y = nyse5[, 1],
                     nbest = 5, method = "adjr2",
                     names = colnames(nyse5[,-1]))


#check results
data.frame(Size = best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
           best_subset$which, row.names = NULL)

#plot results
plot(best_subset$size, best_subset$adjr2,
     ylab = "Adjusted R-squared",
     xlab = "Number of variables (including intercept)")

#Do the same but with Mallor's Cp
best_subset2 <- leaps(x = nyse5[, 2:6], y = nyse5[, 1],
                      nbest = 5, method = "Cp",
                      names = colnames(nyse5[,-1]))
#print results
data.frame(Size = best_subset2$size, Cp = round(best_subset2$Cp, 3),
           best_subset$which, row.names = NULL)

#We want to run leaps for the 10 most correlated variables
#to see if any of them would make a better model

#subset 7 most correlated
nyse.cors.abs.order
#from printing this we can see top 7 correlated are
#CDE, AMZN, SAM, GIL, MSFT, M, DPZM BP, RBS, WHR
#subset
nyse7 = data.frame(nyse.sub)
nyse7 = nyse7 %>% select(GOOGL, CDE, AMZN, SAM, GIL, MSFT, M, DPZ)
attributes(nyse7)

#run the leaps again for R^2
best_subset7_r <- leaps(x = nyse7[, 2:8], y = nyse7[, 1],
                     nbest = 5, method = "adjr2",
                     names = colnames(nyse7[,-1]))
df7 <- data.frame(Size = best_subset7_r$size, AdjR2 = round(best_subset7_r$adjr2, 3),
           best_subset7_r$which, row.names = NULL)

#plot the above
plot(best_subset7_r$size, best_subset7_r$adjr2,
     ylab = "Adjusted R-squared",
     xlab = "Number of variables (including intercept)",
     ylim = c(0,1))


df7$AdjR2
sort(df7$AdjR2)
df7.sort <- df7[order(df7$AdjR2,decreasing=TRUE),]
#base model is adj r^2 0.8037

mod.better <- lm(GOOGL~CDE+SAM,data=nyse)
summary(mod.better)
#save residuals and standardise them
res <- residuals(mod.better)
st.res <- (res-mean(res))/sd(res)

#Both independent variables have negative observations, so this affects the
#transformations that can be used
mod.betterlog <- lm(log(GOOGL)~(CDE)^2+(SAM)^2,data=nyse)
plot(mod.betterlog)
#first we tried transforming CDE and SAM but there were still heavy tails
#we then transformed Y as well and that improved things
try <- lm(GOOGL^(1/3)~(CDE)^2+(SAM)^2,data=nyse)
#At the moment, cube root transformation of DV and log transformations of IVs
#appear to be best.
#amz cde c 
par(mfrow=c(1,2))
qqnorm(mod.betterlog$residuals)
abline(a=0,b=1,lty=2,col="red")
qqnorm(try2$residuals)
abline(a=0,b=1,lty=2,col="red")
?par
dev.off()


#save fitted values
fit <- mod.better$fitted.values
#plot standardised residuals against fitted values
plot(fit, rstudent(mod.better))
#we can see a bit of fanning out and also a curve
#plot studentised residuals against cde
plot(nyse$CDE, rstudent(mod.better))
#there is some curviture here as well
plot(nyse$SAM, rstudent(mod.better))

#transform DV using boxcox
library(MASS)
boxcox(GOOGL~CDE+SAM, data=nyse)
par(mfrow=c(2,2))
plot(nyse$GOOGL)
#looking at tukey ladder, we would use log(Y)
plot(log((nyse$GOOGL)))
plot(sqrt((nyse$GOOGL)))
plot((nyse$GOOGL)^(1/3))
#after plotting we can see using log is closest to straight line

par(mfrow=c(1,2))
plot(nyse$CDE, rstudent(mod.better))
plot(log(nyse$CDE), rstudent(mod.better))
#qqplot
qqnorm(st.res)
abline(a=0,b=1,lty=2,col="red")

#histogram
hist(st.res)

#density
plot(density(st.res))

plot(st.res)

#Look for transformations that reduce fat tails??
#plot Y variable against each X
plot(nyse$CDE)
plot(nyse$SAM, nyse$GOOGL)
?plot

#####################
#step wise selection#
#####################

mod_step <- lm(GOOGL~1, data = nyse)
step(mod_step, scope = ~ CDE+ AMZN + SAM + GIL + 
       MSFT + M + DPZ, direction = "both")

#Check independence of variables
colnames(nyse.sub)
nyse.best <- nyse.sub[,c(7,3,20,11,16,15,9)]
cor(nyse.best)

#Remove Amazon because it is highly correlated with the other variables.

mod_step2 <- lm(GOOGL~1, data = nyse)
step(mod_step2, scope = ~ CDE + SAM + GIL + MSFT + M + DPZ, direction = "both")

#Barplot of actual correlations
barplot(nyse.cors.order, main = "Correlations between Google and other companies' stock prices", xlab="Company", ylab="Pearson's r", ylim=c(-1, 1))

#Barplot of absolute correlations
barplot(nyse.cors.abs.order, main = "Absolute value correlations between Google and other companies' stock prices", xlab="Company", ylab="Pearson's r", ylim=c(0, 1))

mod.noamzn <- lm(GOOGL~CDE + GIL + SAM + M + DPZ, data=nyse)
plot(mod.noamzn)

boxcox(GOOGL~CDE + GIL + SAM + M + DPZ, data=nyse)

mod.noamzn.trans <- lm(log(GOOGL) ~ CDE^2 + GIL^2 + SAM^2 + M^2 + DPZ^2, data=nyse)
plot(mod.noamzn.trans)

res2 <- residuals(mod.noamzn.trans)
st.res2 <- (res2-mean(res2))/sd(res2)

qqnorm(st.res2)
plot(density(st.res2))

################
##Dimitur edit##
################

#try forward selection with 7 variables

mod.fwd <- lm(GOOGL ~ 1, data = nyse7)
add1(mod.fwd, test = "F",
     scope = ~ CDE + GIL + AMZN + M + DPZ + SAM + MSFT)

#add CDE
mod.fwd2 <- lm(GOOGL ~ CDE, data = nyse7)
add1(mod.fwd2, test = "F",
     scope = ~ CDE + GIL + AMZN + M + DPZ + SAM + MSFT)

#add GIL
#check adj r2 
summary(lm(GOOGL ~ CDE+GIL, data=nyse7)) #0.8228
mod.fwd3 <- lm(GOOGL ~ CDE+GIL, data = nyse7)
add1(mod.fwd3, test = "F",
     scope = ~ CDE + GIL + AMZN + M + DPZ + SAM + MSFT)

#add SAM
#check adj r2 
summary(lm(GOOGL ~ CDE+GIL+SAM, data=nyse7)) #0.8344
mod.fwd4 <- lm(GOOGL ~ CDE+GIL+SAM, data = nyse7)
add1(mod.fwd4, test = "F",
     scope = ~ CDE + GIL + AMZN + M + DPZ + SAM + MSFT)

#add MSFT
#check adj r2 
summary(lm(GOOGL ~ CDE+GIL+SAM+MSFT, data=nyse7)) #0.8364
mod.fwd5 <- lm(GOOGL ~ CDE+GIL+SAM+MSFT, data = nyse7)
add1(mod.fwd5, test = "F",
     scope = ~ CDE + GIL + AMZN + M + DPZ + SAM + MSFT)

#add AMZN
#check adj r2 
summary(lm(GOOGL ~ CDE+GIL+SAM+MSFT+AMZN, data=nyse7)) #0.8417
#decide to stop here as adj r2 is not getting much better

##try mod2a##########
#check pairs
attributes(nyse)
mod2a.data = nyse[, c(5, 11, 15)]
pairs(mod2a.data)
#there is curve in GOOGL v CDE, and slight curve in GOOGLE v GIL

#fit model
mod2a <- lm(GOOGL^(1/3)~CDE+GIL, data=nyse7)
#store fitted values
fit <- mod2a$fitted.values
# Plot studentised residuals against fitted values.
plot(fit, rstudent(mod2a))
#clearly there is fanning out

# Plot studentised residuals against CDE.
plot(nyse7$CDE, rstudent(mod2a))
#pattern seems a little bit non-random

# Plot studentised residuals against GIL.
plot(nyse7$GIL, rstudent(mod2a))
#patern seems random

# Draw a QQ-plot. (return the plotting region to normal first)
plot(mod2a, which = c(2))
#there appear to be issues with normality assumption

shapiro.test(rstudent(mod2a)) #not normal

#get boxcox
boxcox(GOOGL~CDE+GIL, data = nyse7)
#lamda is near 0.5

plot(mod2a)
plot(nyse$CDE, nyse$GOOGL)
