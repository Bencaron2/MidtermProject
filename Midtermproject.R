
library(dplyr)
library(ggplot2)
library(tidyr)
data <- read.csv("hospitals.csv")

# 1. Perform exploratory analysis
# a) How big is the dataset?
dim(data)
## size is 2000

# b) What are the names of the columns?
colnames(data)

# c) What data types are each column?
sapply(data, class)
print("Column data types:")
print(column_types)

# d) Are there missing values?
colSums(is.na(data))
anyNA(data)


# e) Which hospital has the lowest number of beds?
hospital_lowest_beds <-data %>% filter(Beds == min(Beds)) %>% select(Hospital.Number, Beds)
print("Hospital with the lowest number of beds:")
print(hospital_lowest_beds)

# f) Which hospital has the lowest expense?
hospital_lowest_expense <- hospital_data %>%
  filter(Total.Expense == min(Total.Expense)) %>% select (Hospital.Number, Total.Expense)
print(c("Hospital with the lowest expense:",hospital_lowest_expense))


# g) How many hospitals deliver babies?
deliver_babies <- sum(data$Births.or.Not == 1)
print(hospitals_deliver_babies)      

# h) Using ggplot, scatterplot number of beds vs Total Expense
ggplot(data, aes(x = Beds, y = Total.Expense, col=Region)) +
  geom_point() +
  labs(x = "Number of Beds", y = "Total Expense") +
  ggtitle("Scatterplot of Number of Beds vs Total Expense")

# i) Using ggplot, scatterplot Admissions vs Total Expense
ggplot(data, aes(x = Admissions, y = Total.Expense, col=Region)) +
  geom_point() +
  labs(x = "Admissions", y = "Total Expense") +
  ggtitle("Scatterplot of Admissions vs Total Expense")

# j) Using dplyr and ggplot, scatterplot beds vs Total Expense but only for hospitals that deliver babies
ggplot(data %>% filter(Births == 1), aes(x = Beds, y = Total.Expense, col=Region)) +
  geom_point() +
  labs(x = "Number of Beds", y = "Total Expense") +
  ggtitle("Scatterplot of Number of Beds vs Total Expense for Hospitals that Deliver Babies")

# k) One more question that you believe would be useful.
# For example, let's find out the distribution of hospitals by region
data %>%
  count(Region) %>%
  ggplot(aes(x = Region, y = n, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Number of Hospitals") +
  ggtitle("Distribution of Hospitals by Region")

###########
# Descriptive Analysis
# i. For Pie Chart: Admissions and Births
admissions_sum <- sum(data$Admissions)
Beds_sum <- sum(data$Beds)
pdata <- data.frame(Category = c("Admissions", "Beds"),
                       Value = c(admissions_sum, births_sum))
ggplot(pdata, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  labs(title = "Admissions vs Beds Pie Chart") +
  theme_void()

# ii. For the column/bar charts: Admissions and Outpatient.Visits
data %>%
  summarise(Admissions = sum(Admissions), Outpatient.Visits = sum(Outpatient.Visits)) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  ggplot(aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Value") +
  ggtitle("Total Admissions vs Total Outpatient Visits")

# iii. For Line Chart: Expense and Payroll Expense
data$Date <- as.Date(data$Date)

ggplot(data, aes(x = Total.Expense)) +
  geom_line(aes(y = Payroll.Expense, colour = "Payroll Expense")) +
  geom_line(aes(y = Total.Expense, colour = "Total expense")) +
  labs(title = "Trend of Payroll Expense and Total Expense Over Time", x = "Date", y = "Value") +
  theme_minimal() +
  scale_colour_manual(values = c("Payroll Expense" = "purple", "Total expense" = "red"))

# 3. Simple Regression
regression <- lm(Total.Expense ~ Beds, data = data)

summary(regression)

#ii. The dependent variable should be Total Expense. Choose anindependent variable from one of the remaining attributes.
#I picked number of beds since that is an important factor when choosing hospital size 
#iii. What is the value of the R²?
#The value of the R² (R-squared) in this model is 0.6043. This indicates the proportion of variance in the total expense that can be explained by the number of beds.

  # iv. What does the R^2 measure in this case? ( Hint: Percentage of variation in … explained by …)
#R-squared measures the proportion of the variance in the dependent variable (Total Expense) that is predictable from the independent variable (Beds). 
#In this case, it indicates that about 60.43% of the variation in total expenses can be explained by the number of beds in the hospital.

#v. What are the pvalues ? How many pvalues are reported, why ? What does each pvalue mean? (Hint: pvalues have are related to 
# generalizing from the sample to the population: what is the samplehere, what is the population?) (1-2 sentences)
#p-value for the intercept (-16060.93): 0.00688, which indicates that the intercept is significantly different from zero.
#p-value for the slope (1084.56): < 2e-16, indicating that the number of beds is significantly associated with total expense. This is the p-value for testing whether the coefficient of Beds is zero. 
#The extremely low p-value here rejects the null hypothesis, implying a significant linear relationship between the number of beds and the total expense.

#vi. Explain R square, pvalues.
#R-squared (R²): This statistic measures the proportion of variance in the dependent variable that is predictable from the independent variable(s). It is a measure of how well the observed outcomes are replicated by the model, based on the proportion of total variation of outcomes explained by the model. Higher R² values represent a better fit of the model.
#P-values: These values assess the significance of your results. The p-value for each coefficient tests the null hypothesis that the coefficient is equal to zero (no effect). A low p-value (< 0.05) indicates that you can reject the null hypothesis. In the context of regression, p-values below 0.05 typically indicate that there is a statistically significant association between the predictor and the response variable.

#vii. What would be the right attribute size (independent variable) that
#seems most appropriate to lead you in the expense range of $55–$75 million?
 # 50,727 beds for a total expense of $55 million.
#69,167 beds for a total expense of $75 million.

# 4. Multivariate Regression
multivariate_regression <- lm(Total.Expense ~ Beds + Admissions, data = data)

summary(multivariate_regression)

# ii.  R^2 is 0.7398.

# iii. The R-squared (R²) measures the percentage of variation
# in Total Expense that is explained by the independent variables
# (Outpatient Visits and Births). It indicates the goodness of fit of the
# regression model. For example, an R² of 0.7398 means that approximately
# 74% of the variability in Total Expense can be explained by the variation
# in Outpatient Visits and Births.

# iv. The p-value for Admissions is less than 2e-16, indicating that it is
# highly statistically significant.
# The p-value for Beds is 7.25e-13, also indicating high statistical
# significance.
# These p-values represent the probability of observing the given result
# (or more extreme results) under the null hypothesis that the coefficient
# is zero. Since both p-values are much smaller than the conventional
# significance level of 0.05, we reject the null hypothesis for both
# Admissions and Beds, indicating that both variables have a significant
# effect on Total Expense.

# v. R-squared (R²):
  #   R-squared measures how well the independent variables explain the variability in the dependent variable.
  #  R-squared is 0.7398

# P-values:
# P-values indicate the significance of each independent variable's impact on the model.
# Both Admissions and Beds have extremely low p-values, < 2e-16 and 7.25e-13 so there is a high statistical significance.
# Both Admissions and Beds have an impact on Total Expense.
