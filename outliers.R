cars1 <- cars[1:30,]
cars_outliers <- data.frame(speed = c(19,19,20,20,20), dist = c(190, 186, 210, 220, 218))

cars2 <- rbind(cars1, cars_outliers)

par(mfrow = c(1, 2))
plot(cars2$speed, cars2$dist, xlim=c(0, 28), ylim=c(0, 230), main="With Outliers", 
     xlab="speed", ylab="dist", pch="*", col="red", cex=2)

abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), main="Without Outliers", 
     xlab="speed", ylab="dist", pch="*", col="red", cex=2)

abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)

url <- "http://rstatistics.net/wp-content/uploads/2015/09/ozone.csv"
inputData <- read.csv(url)  # import data

outlier_values <- boxplot.stats(inputData$pressure_height)$out
boxplot(inputData$pressure_height, main = 'Pressure Height', boxwex = 0.1)
#paste -> converted to a character vector
mtext(paste('Outliers: ', paste(outlier_values, colapse= ",")), cex = 0.6)

ozone <- read.csv(url)

# For continuous variable (convert to categorical if needed.)
boxplot(ozone_reading ~ pressure_height, 
        data=ozone, 
        main="Boxplot for Pressure height (continuos var) vs Ozone")

boxplot(ozone_reading ~ cut(pressure_height,
                            pretty(inputData$pressure_height, n = 20)),
        data=ozone, main="Boxplot for Pressure height (categorial) vs Ozone",
        cex.axis=0.5)

#===================================================#
#
#          MULTIVARIATE MODEL APPROACH
#
#===================================================#

mod <- lm(ozone_reading ~., data = ozone)
cooksd <- cooks.distance(mod)
mean_value <- mean(cooksd, na.rm = TRUE)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4*mean_value, col="red")
labels <- ifelse(cooksd > 4*mean_value, names(cooksd), "" )
text(x = 1:length(cooksd)+1, y = cooksd, labels = labels , col = "green")

# Let's find out the influential rows from the original data.
influential <- names(cooksd)[cooksd > 4*mean_value]

head(ozone[influential,])

#===================================================#
#
#                   OUTLIERS TEST
#
#===================================================#
install.packages("outliers")
library("outliers")

car::outlierTest(mod)
set.seed(1234)
x = rnorm(10)

scores(x)
scores(x, type = "chisq")
scores(x, type="t")


#===================================================#
#
#          TREATING OUTLIERS (CAPPING)
#
#===================================================#
# For missing values that lie outside the 1.5*IQR limits
# Those observation outside the lower limit --> 5% percentile
# Those observation outside the upper limit --> 95% percentile

H <- 1.5*IQR(ozone$pressure_height, na.rm = T)

lower_limit <- as.numeric(quantile(ozone$pressure_height,
                                   probs = .25, na.rm = T))
lower_value <- as.numeric(quantile(ozone$pressure_height,
                                   probs = .05, na.rm = T))
upper_limit <- as.numeric(quantile(ozone$pressure_height,
                                   probs = .75, na.rm = T))
upper_value <- as.numeric(quantile(ozone$pressure_height,
                                   probs = .95, na.rm = T))

ozone$pressure_height <- ifelse (!complete.cases(ozone$pressure_height),
                                 mean(ozone$pressure_height, na.rm = TRUE),
                                 ozone$pressure_height) 

ozone$pressure_height <- as.numeric(ozone$pressure_height)

ozone$pressure_height[ozone$pressure_height > upper_limit] <- upper_value
ozone$pressure_height[ozone$pressure_height < lower_limit] <- lower_value









