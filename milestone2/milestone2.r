
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