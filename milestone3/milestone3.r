
#data
df <- read.csv("hypo.data.csv")
str(df)
df$Cocoa.Percent  <- as.numeric(sub("%", "", df$Cocoa.Percent))

#Step 7: Scatterplots and correlation 
#1. For at least one pair of quantitative columns/variables: 
#Plot a scatterplot of the data in the two columns/variables.
plot(df$Cocoa.Percent,df$Rating,col="blue", xlab="Coca.Percent",ylab="Rating",
     main="Scatterplot of Coca Percent and Rating")
#Compute the correlation between the columns/variables. 
cor <- cor.test(df$Cocoa.Percent, df$Rating, method="pearson")
cor$estimate
cor$p.value
cor

# Step 8: Confidence Intervals 
# 1. Choose at least 2 quantitative columns, and do the following for each column: 
# Compute the 95% confidence interval for the mean. 
# For Cocoa Percent
mean <- mean(df$Cocoa.Percent)
mean
se <- sd(df$Cocoa.Percent) / sqrt(60)
lower <- mean - 1.96 * se
upper <- mean + 1.96 * se
c(lower, upper)

#for Rating
mean1 <- mean(df$Rating)
mean1
se <- sd(df$Rating) / sqrt(60)
lower1 <- mean1 - 1.96 * se
upper1 <- mean1 + 1.96 * se
c(lower1, upper1)
str(df)
# Step 9: Linear regression 
#Compute the linear regression model using one or more of the other data columns as the independent variable(s) 
model=lm(Rating~Review.Date+REF+Cocoa.Percent,df)
#model Coefficents
round(model$coefficients,5)
#Assess the fit of the model by computing R-squared, plotting a histogram of the residuals, and plotting a scatter plot of the actual observed response value (x axis) vs. residual (y axis). 
#R-squared
summary(model)$r.squared
#Histogram
hist(model$residuals ,col="sky blue", 
     main = "Histogram of Residuals",xlab = "Residuals")
# residuals vs fitted plot
plot(model$fitted.values,model$residuals, col="blue", xlab="Fitted Values",
ylab="Residuals", main="Residuals vs Fitted Plot")

# Step 10: Hypothesis Testing 
#1st Hypothesis : average rating is differnt if company located in USA than other ountries average rating
df <- df %>% 
  mutate( 
    Company.Location = as.factor(case_when(
      Company.Location == "U.S.A." ~ "U.S.A",      
      T~"Others"
    )))

# T test for testing hypothesis on alpha =0.05 
library(car)
car::leveneTest(df$Rating~df$Company.Location)

# T test
t.test(df$Rating~df$Company.Location, var.equal=TRUE)

#2nd Hypothesis : average Cocoa.Percent is greater than 50%
# 1 sample t test
t.test(df$Cocoa.Percent, mu=50, alternative = "greater")

# Step 11: Your choice 
#1. Perform any 1 other analyses on your dataset. You may also perform one of the previous analysis on a different variable. 
#Here we can test that wether average Cocoa.Percent is different for USA than other countries

# Variance test
car::leveneTest(df$Cocoa.Percent~df$Company.Location)
# T test
t.test(df$Cocoa.Percent~df$Company.Location,var.equal=F)

# Boxplots
boxplot(df$Cocoa.Percent~df$Company.Location,ylab="Rating",xlab ="Company.Location", 
    main="Boxplots of Cocoa Percent by Company Location", col="sky blue")

# Part 12: Three Extra For each of the Three extra analysis
# Analysis 1
#Whether the rating and cocoa percent variable have normal distribution?
par(mfrow=c(1,2))
hist(df$Rating, col="brown",xlab="Rating",
     main="Histogram of Rating")
hist(df$Cocoa.Percent, col="dark green",xlab="Cocoa Percent",
     main="Histogram of Rating")

# Analysis 2
# Boxplots of rating by review date
df$Review.Date <-as.factor(df$Review.Date)
boxplot(df$Rating~df$Review.Date,ylab="Rating",
xlab ="Review Date", main="Boxplots of Rating by Review Date", col="sky blue")

# Analysis 3
# Boxplots of Cocoa Percent by review date
df$Review.Date <-as.factor(df$Review.Date)
boxplot(df$Cocoa.Percent~df$Review.Date,ylab="Cocoa Percent",
        xlab ="Review Date", main="Boxplots of Cocoa Percent by Review Date", col="sky blue")
