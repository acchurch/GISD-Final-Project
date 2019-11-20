# Publicly available from CDC WONDER linked birth/infant death files form 2011-2015
# Area Health Resource File county-level variables 
# linked these files by county FIPS codes
# any raw number variables converted to percentages or rates based on population that
# are also in ARHF
# get links to CDC data and AHRF data 


# Import data set from SPSS
Thesis_Final_Data_Set_2 <- read_sav("~/Documents/Thesis /Thesis Final Data Set_2.sav")

# see what Var names I have in dataset
names(Thesis_Final_Data_Set_2)

#structure of objects
str(Thesis_Final_Data_Set_2)

# displays summary of data
summary(Thesis_Final_Data_Set_2)

# dimensions of data
dim(Thesis_Final_Data_Set_2)

# table for Black IMR
table(Thesis_Final_Data_Set_2$BLACK_IMR)

# descriptive statistics for Black IMR
summary(Thesis_Final_Data_Set_2$BLACK_IMR)

# descriptive statistics for white IMR
summary(Thesis_Final_Data_Set_2$WHITE_IMR)

# just the mean Black IMR
mean(Thesis_Final_Data_Set_2$BLACK_IMR)

# just the mean white IMR
mean(Thesis_Final_Data_Set_2$WHITE_IMR)

# histogram of Black IMR
hist(Thesis_Final_Data_Set_2$BLACK_IMR)

# histogram of white IMR
hist(Thesis_Final_Data_Set_2$WHITE_IMR)

# Scatterplot for Black IMR & Midwives
plot(Thesis_Final_Data_Set_2$BLACK_IMR, Thesis_Final_Data_Set_2$V_midwife, main = 'Scatterplot',
    xlab= 'Black IMR', ylab = 'Rate of Midwives',
    col= 'blue', pch= 3, cex= 2)

# Scatterplot for Black IMR & White IMR
plot(Thesis_Final_Data_Set_2$BLACK_IMR, Thesis_Final_Data_Set_2$WHITE_IMR, main = 'Scatterplot',
     xlab= 'Black IMR', ylab = 'White IMR',
     col= 'blue', pch= 3, cex= 2)


## not important 
# linear regression of median household income on Black IMR
model.1 <- lm(Thesis_Final_Data_Set_2$V_med_hhld_income ~ Thesis_Final_Data_Set_2$BLACK_IMR)
# produces regression results
summary(model.1)
# store the predicted values
model.1.pred <- predict(model.1)

# plot predictions
# first data
plot(x=Thesis_Final_Data_Set_2$BLACK_IMR,y=Thesis_Final_Data_Set_2$V_med_hhld_income,xlab="Black IMR",ylab="Median Household Income")
# now predicted values
points(x=Thesis_Final_Data_Set_2$BLACK_IMR,y=model.1.pred,type="l",col="red")
# the predicted values are also in model.1 ...
points(x=Thesis_Final_Data_Set_2$BLACK_IMR,y=model.1$fitted.values,col="blue",cex=0.5)
## not important above this 


#**** linear regression of Black IMR on median household income-- this one is 
# the one to look at 
model.1 <- lm(Thesis_Final_Data_Set_2$BLACK_IMR ~ Thesis_Final_Data_Set_2$V_med_hhld_income)
# produces regression results
summary(model.1)
# store the predicted values
model.1.pred <- predict(model.1)

# plot predictions for the most interesting relationship 
# first data
plot(x=Thesis_Final_Data_Set_2$V_med_hhld_income,y=Thesis_Final_Data_Set_2$BLACK_IMR,xlab="Median Household Income",ylab="Black IMR")
# now predicted values
points(x=Thesis_Final_Data_Set_2$V_med_hhld_income,y=model.1.pred,type="l",col="red")
# the predicted values are also in model.1 ...
points(x=Thesis_Final_Data_Set_2$V_med_hhld_income,y=model.1$fitted.values,col="blue",cex=0.5)


# linear regression of white IMR on household income
model.2 <- lm(Thesis_Final_Data_Set_2$WHITE_IMR ~ Thesis_Final_Data_Set_2$V_med_hhld_income)
# produces regression results
summary(model.2)
# store the predicted values
model.2.pred <- predict(model.2)

# plot predictions for the most interesting relationship 
# first data
plot(x=Thesis_Final_Data_Set_2$V_med_hhld_income,y=Thesis_Final_Data_Set_2$WHITE_IMR,xlab="Median Household Income",ylab="White IMR")
# now predicted values
points(x=Thesis_Final_Data_Set_2$V_med_hhld_income,y=model.2.pred,type="l",col="red")
# the predicted values are also in model.1 ...
points(x=Thesis_Final_Data_Set_2$V_med_hhld_income,y=model.2$fitted.values,col="blue",cex=0.5)


# Presentation 
# need table and figure for results, talk for less than 5 mins, few 
# slides to talk about what I am doing, also need to have link to website
# and twitter handle -- due Dec. 3rd 

