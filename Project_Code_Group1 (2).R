rm(list = ls())

library(ggplot2)
library(dplyr)

HeartDisease <- read.csv("C:/Heart Disease/archive (2)/heart_statlog_cleveland_hungary_final.csv")
View(HeartDisease)

head(HeartDisease)

#noofcolumns
ncol(HeartDisease)


#noofrows
nrow(HeartDisease)

sum(is.na(HeartDisease))
colSums(is.na(HeartDisease))
HeartDisease


str(HeartDisease)



# Transform categorical variable to R factors
HeartDisease$sex <- as.factor(HeartDisease$sex)
HeartDisease$chest.pain.type <- as.factor(HeartDisease$chest.pain.type)

HeartDisease$exercise.angina<- as.factor(HeartDisease$exercise.angina)
HeartDisease$resting.ecg<- as.factor(HeartDisease$resting.ecg)
HeartDisease$target<- as.factor(HeartDisease$target)

levels(HeartDisease$sex) <- c("Female", "Male")
levels(HeartDisease$target) <- c("Normal", "Heart Disease")
levels(HeartDisease$chest.pain.type) <- c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")
levels(HeartDisease$resting.ecg) <- c("Normal","Having St-t wave abnormality", "showing probable or definite left ventricular 
hypertrophy by Estes' criteria")
levels(HeartDisease$exercise.angina) <- c("No", "Yes")

View(HeartDisease)
head(HeartDisease,22)

missing_values<- colSums(is.na(HeartDisease))
missing_values


checkUniqueValues <- function(data) {
  for (col in names(data)) {
    unique_values <- unique(data[[col]])
    print(paste("Unique values in column", col, ":"))
    print(unique_values)
    print("-----------------------------")
  }
}

checkUniqueValues(HeartDisease)

#This is the barchart

ggplot(HeartDisease, aes(x = ST.slope, fill=target)) + 
  geom_bar() +
  labs(title = "Heart Disease Present", x="ST Slope", y="No. of Observations")


ggplot(HeartDisease, aes(x = resting.ecg, fill=target)) + 
  geom_bar() +
  labs(title = "Resting ECG Types in Heart Disease Study", x="Resting ECG Results", y="No. of Observations")

ggplot(HeartDisease, aes(x = exercise.angina, fill=target)) + 
  geom_bar() +
  labs(title = "Exercise-Induced Angina in Heart Disease Study", x="Exercise Angina", y="No. of Observations") 

ggplot(HeartDisease, aes(x = sex, fill=target)) + 
  geom_bar() +
  labs(fill="Heart Disease", x="Sex", y="Number of patients")

ggplot(HeartDisease, aes(x = chest.pain.type, fill=target)) + 
  geom_bar() +
  labs(fill="Heart Disease", x="Chest Pain Type", y="Number of patients")


#piechart

create_pie_chart <- function(table_data, main_title) {
  pie_percent <- prop.table(table_data) * 100  # Calculate percentages
  
  # Create the pie chart with formatted percentage labels and matching legend colors
  pie(table_data, labels = sprintf("%.1f%%", pie_percent), main = main_title, col = rainbow(length(table_data)))
  
  # Add legend to the right of the chart
  x_legend <- 1  # Adjust as needed
  y_legend <- 1  # Adjust as needed
  
  # Add legend to the specified position
  legend(x = x_legend, y = y_legend, legend = names(table_data), fill = rainbow(length(table_data)), cex = 0.5)
  
}

create_pie_chart(table(HeartDisease$chest.pain.type), "Chest Pain Distribution")
create_pie_chart(table(HeartDisease$resting.ecg), "Resting ECG Types in Heart Disease Study")

#stem&leaf

Cholesterol <- HeartDisease$cholesterol


stem(Cholesterol)


Maximum.HR <- HeartDisease$max.heart.rate
stem(Maximum.HR)



#Histogram

ggplot(HeartDisease, aes(x = age, fill=target)) +
  geom_histogram(binwidth=10) +
  labs(fill="Heart Disease", x="Age", y="Number of patients")

ggplot(HeartDisease, aes(x = resting.bp.s, fill=target)) +
  geom_histogram(binwidth=3) +
  labs(fill="Heart Disease", x="Heart Patient", y="Number of patients")

ggplot(HeartDisease, aes(x = cholesterol, fill=target)) +
  geom_histogram(binwidth=10) +
  labs(fill="Heart Disease", x="Cholesterol", y="Number of patients")

ggplot(HeartDisease, aes(x= oldpeak, fill=target)) +
  geom_histogram(binwidth=0.25) +
  labs(fill="Heart Disease", x="ST Depression", y="Number of patients")

restingBP <- HeartDisease$resting.bp.s
STSlope <-HeartDisease$oldpeak
Gender <- HeartDisease$sex
#Box plot
min_value_oldpeak <- min(HeartDisease$oldpeak, na.rm = TRUE)
max_value_oldpeak <- max(HeartDisease$oldpeak, na.rm = TRUE)


# Set up the plotting layout
par(mfrow = c(3, 1))

# Define the variables of interest
variables_of_interest <- c( "oldpeak")


# Loop through each variable and generate boxplots
for (var in variables_of_interest) {
  boxplot(HeartDisease[[var]], col = "skyblue", xlab = var, main = var,ylim = c(-2.6,7))}




summary(HeartDisease)

Cholesterol <- HeartDisease$cholesterol

# Selecting only numeric variables

cont_data <- HeartDisease %>%
  select(age, resting.bp.s, cholesterol, max.heart.rate, ST.slope) %>%
  mutate(across(everything(), as.numeric))


custom_summary <- function(x) {
  round(
    c(Min = min(x),
      Quartile_1 = quantile(x, 0.25),
      Median = median(x),
      Mean = mean(x),
      Quartile_3 = quantile(x, 0.75),
      Max = max(x),
      SD = sd(x),
      Variance = var(x)),
    digits = 3)
}

summary_table <- sapply(cont_data, custom_summary)
print(summary_table)

# Hypothesis Testing - 1-Sample T-Test
mean_age <- mean(HeartDisease$age, na.rm = TRUE)
print(mean_age)
#Hypothesis Testing 1
t_test_1 <- t.test(HeartDisease$age, mu = 54)
print(t_test_1)


# Hypothesis Testing - 2-Sample T-Test

# Separate data into two groups based on heart disease status
HeartDiseases <- HeartDisease$age[HeartDisease$target == 'Heart Disease']
no_heart_disease <- HeartDisease$age[HeartDisease$target == 'Normal']

# Perform 2-sample t-test
t_test_2 <- t.test(HeartDiseases, no_heart_disease)
print(t_test_2)


# Goodness of Fit Test
observed_frequencies <- table(HeartDisease$`chest.pain.type`)
print(observed_frequencies)

goodness_fit_test <- chisq.test(table(HeartDisease$`chest.pain.type`), p = c(0.10, 0.20, 0.30, 0.40))
print(goodness_fit_test)


# Chi-Square Test of Independence
chisq.test(table(HeartDisease$sex, HeartDisease$`exercise.angina`))


# Correlation

cor_test <- cor.test(HeartDisease$resting.bp.s, HeartDisease$cholesterol)

# Scatter Plot
par(pin = c(3, 1))
plot(HeartDisease$resting.bp.s, HeartDisease$cholesterol, main="Blood Pressure vs Cholesterol", xlab="Blood Pressure", ylab="Cholesterol", pch=19, col='yellow')
model_2 <- lm(HeartDisease$cholesterol ~ HeartDisease$resting.bp.s, data=HeartDisease)
abline(model_2, col="blue")

# Print Correlation Test Result
print(cor_test)

# Simple Linear Regression

# Fit a linear regression model
Age <- HeartDisease$age
lm_model <- lm(Maximum.HR ~ Age, data = HeartDisease)

# Plotting scatter plot
plot(HeartDisease$age, HeartDisease$max.heart.rate, main = "Age vs. Max Heart Rate",
     xlab = "Age", ylab = "Max Heart Rate", pch = 16, col = "purple")


# Adding regression line to the plot
abline(lm_model, col = "red")

# Display the regression equation and R-squared value
summary(lm_model)


# ANOVA

# Perform ANOVA
anova_result <- aov(Maximum.HR ~ chest.pain.type, data = HeartDisease)
# Summary of ANOVA
summary(anova_result)
