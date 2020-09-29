## Part 1: Single variable distribution plots

df <- read.csv("flavors_of_cacao.csv")
hist(df$Rating, xlab = "Cocoa Rating", ylab = "Number of Chocolates", main = "Distribution of Cocoa Rating")

# The rating shows that the values vary between 1 and 5. The distribution does not have a normal distribution but a negative skewness. Most ratings are located at between 2.5 - 4.0. The ratings have continuous distribution between 1 and 4. After 4, there are only 2 records with 5 ratings which are also outliers.

hist(df$Review.Date, xlab = "Review Year", ylab = "Number of Chocolates", main = "Distribution of Cocoa Review Date")

# The histogram of years in which the chocolates were reviewed shows that the trend seems to be that there were more reviews as time went on, except for 2017, maybe when the data stopped being collected.

counts <- sort(table(df$Company.Location), decreasing = T)
freq   <- counts/sum(counts)*100

barplot(head(freq, 5), 
        main = "Top 5 Company Location by Number of Reviewed Product",
        xlab = "Bean Origin Country", ylab = "Percentage (%)")

# A bar plot was generated using the frequency table based on company locations of reviewed products. The distribution showed that nearly half of the products reviewed belong to companies located in the USA, followed by France, Canada.


Cocoa_percent <- as.numeric(gsub("%","",df$Cocoa.Percent))

hist(Cocoa_percent,
        main = "Cocoa Percentages Distribution of Reviewed Products",
        xlab = "Percentage (%)", ylab = "Frequency")


# The rating shows that the Cocoa percentage changes between 40% and 100%. The distribution show pattern similar to symmetrical distribution with no visible significant skewness. The mean and median percentage is located between 65 - 75. There are some records with 100% percentage Cocoa which are outliers according to distribution.

## Part 2: Measures of center and spread

cocoa_percent <- as.numeric(gsub("%","",df$Cocoa.Percent))
mean(cocoa_percent)
median(cocoa_percent)
var(cocoa_percent)
sd(cocoa_percent)

# The mean and median values of cocoa percentages are too close to each other. The distributions with the higher difference between mean and median are generally shown skewness. As the percentage values have similar mean and median, we can interpret this distribution as symmetrical with a ~70% peak point. The standard deviation is also relatively small indicates that most values are clustered nearby the peak point and are not dispersed much.

mean(df$Rating)
median(df$Rating)
var(df$Rating)
sd(df$Rating)

# While the rating scores mean and median values are slightly different from each other. As the median is larger than the mean, we can interpret this data as slightly negative skewed. The standard deviation is also relatively small indicates that most values are clustered nearby the peak point and are not dispersed much.