library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC)
library(ggplot2)
library(vip)
library(rpart.plot)
library(DALEXtra)

hr_train=read.csv("D:\\data ANALYTICS AND SCIENCE\\R EDVANCER\\PROJECT SUBMISSION DATA\\HR\\hr_train.csv",stringsAsFactors = FALSE)
View(hr_train)
hr_test=read.csv("D:\\data ANALYTICS AND SCIENCE\\R EDVANCER\\PROJECT SUBMISSION DATA\\HR\\hr_test.csv",stringsAsFactors = FALSE)
View(hr_test)
colSums(is.na(hr_train))
glimpse(hr_train)
#vis_dat(hr_train)

dp_pipe=recipe(left~.,data=hr_train) %>% 
  update_role(sales,salary,new_role ="to_dummies") %>% 
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
  step_dummy(has_role("to_dummies")) 
dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data=NULL)
View(train)
test=bake(dp_pipe,new_data=hr_test)
#vis_dat(train)


## Random Forest
rf_model = rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# Convert outcome variable to factor
train$left <- as.factor(train$left)

#tuning the parameter
folds = vfold_cv(train, v = 10)

rf_grid = grid_regular(mtry(c(5,25)), trees(c(100,500)),
                       min_n(c(2,10)),levels = 5)

# will tell which are the best parameters
my_res=tune_grid(
  rf_model,
  left~.,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE)
)


fold_metrics=collect_metrics(my_res)

my_res %>% show_best()

final_rf_fit=rf_model %>% 
  set_engine("ranger",importance='permutation') %>% 
  finalize_model(select_best(my_res,"roc_auc")) %>% 
  fit(left~.,data=train)


# variable importance 

final_rf_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))


# predicitons

train_pred=predict(final_rf_fit,new_data = train,type="prob") %>% select(.pred_1)
test_pred=predict(final_rf_fit,new_data = test,type="prob") %>% select(.pred_1)
write.csv(test_pred,"SAYAK_DAS_P4_part2.csv",row.names=F)










#Find out total promotions happened in last 5 years
# Assuming the dataset is named "employees_data"
total_promotions <- sum(hr_train$promotion_last_5years == 1)
total_promotions



# Find out the variance in statisfaction_level for category 0 of variable ‘left’ (round off to 4 decimal places).
left_0_data <- hr_train[hr_train$left == 0, ]  # Filter for category 0 of 'left'
variance <- var(left_0_data$satisfaction_level)  # Calculate variance of 'satisfaction_level'
# Round off to 4 decimal places
variance <- round(variance, 4)
variance 




#Does average_monthly_hours follow normal distribution?
"To determine whether the  average_monthly_hours variable follows a normal distribution, we can assess its distribution visually and perform a statistical test.
Here's an approach to evaluate the normality assumption:
Visual Assessment: Plotting a histogram and a probability plot (such as a Q-Q plot)
can provide insights into the distribution's shape. 
These visualizations can help identify departures from normality, such as skewness or heavy tails.
By examining the histogram and Q-Q plot, if the data points are approximately 
linear in the Q-Q plot and the histogram exhibits a bell-shaped distribution, 
it suggests that the variable may follow a normal distribution."

hist(hr_train$average_montly_hours, breaks = "FD", col = "blue", main = "Histogram - average_monthly_hours")
# Q-Q plot
qqnorm(hr_train$average_montly_hours)
qqline(hr_train$average_montly_hours, col = "red")

"Statistical Test: A formal statistical test can provide a quantitative 
measure of how closely the variable follows a normal distribution.
The Shapiro-Wilk test is a commonly used test for normality.
The Shapiro-Wilk test provides a p-value. If the p-value is greater than 
a significance level (e.g., 0.05), it suggests that the variable does not 
significantly deviate from a normal distribution.
By considering both the visual assessment and the statistical test, 
you can determine whether the average_monthly_hours variable follows 
a normal distribution or deviates from it."
# Assuming the dataset is named "employees_data"
shapiro.test(hr_train$average_montly_hours)


#Find out which category of salary has maximum employee resignation.
# Filter the dataset for resignations
resignations <- hr_train[hr_train$left == 1, ]
# Calculate count of resignations by salary category
resignations_count <- table(resignations$salary)
# Identify the category with maximum resignations
category_max_resignations <- names(resignations_count[resignations_count == max(resignations_count)])
# Print the category with maximum resignations
cat("The category of salary with maximum employee resignations is:", category_max_resignations)



# Find out correlation coefficient between last_evaluation and average_monthly_hours (round it off to 2 decimal places).
# Read the CSV file
hr_train <- read.csv("D:/data ANALYTICS AND SCIENCE/R EDVANCER/PROJECT SUBMISSION DATA/HR/hr_train.csv")
# Calculate correlation coefficient
correlation <- cor(hr_train$last_evaluation, hr_train$average_montly_hours)
# Round off to 2 decimal places
correlation <- round(correlation, 2)
# Print the correlation coefficient
print(paste("Correlation coefficient:", correlation))






# According to given data what is the probability that someone will leave the organisation if they were involved in a work accident? (round off 2 decimal places)
# Calculate the probability
probability <- sum(hr_train$left[hr_train$Work_accident == 1] == 1) / sum(hr_train$Work_accident == 1)
# Round off to 2 decimal places
probability <- round(probability, 2)
# Print the probability
print(paste("Probability of leaving if work accident:", probability))





#What is the median time spent with the company among people leaving the company?
# Calculate the median time spent with the company among people leaving
median_time_leaving <- median(hr_train$time_spend_company[hr_train$left == 1])
# Print the median time spent with the company
print(paste("Median time spent with the company among people leaving:", median_time_leaving))



#Which sales category has maximum median average_monthly_hours?
# Calculate the median average monthly hours for each sales category
median_hours_by_sales <- aggregate(hr_train$average_monthly_hours, by = list(hr_train$sales), FUN = median)
# Identify the sales category with the maximum median average monthly hours
max_median_sales <- median_hours_by_sales[which.max(median_hours_by_sales$x), "Group.1"]
# Print the sales category with the maximum median average monthly hours
print(paste("Sales category with maximum median average monthly hours:", max_median_sales))

#oR

# Which sales category has maximum median average_monthly_hours?
library(dplyr)

# Calculate the median average monthly hours for each sales category
median_hours_by_sales <- hr_train %>%
  group_by(sales) %>%
  summarise(median_average_monthly_hours = median(average_montly_hours))
# Identify the sales category with the maximum median average monthly hours
max_median_sales <- median_hours_by_sales$sales[which.max(median_hours_by_sales$median_average_monthly_hours)]
# Print the sales category with the maximum median average monthly hours
print(paste("Sales category with maximum median average monthly hours:", max_median_sales))




# Does number of projects significantly differ between two categories of the target variable “left”?
library(readr)
library(dplyr)
library(stats)
# Read the CSV file
hr_train <- read.csv("D:\\data ANALYTICS AND SCIENCE\\R EDVANCER\\PROJECT SUBMISSION DATA\\HR\\hr_train.csv", stringsAsFactors = FALSE)
# Separate the number of projects for each category of "left"
projects_left_0 <- hr_train %>%
  filter(`left` == 0) %>%
  select(number_project) %>%
  unlist()
projects_left_1 <- hr_train %>%
  filter(`left` == 1) %>%
  select(number_project) %>%
  unlist()
# Perform the independent two-sample t-test
result <- t.test(projects_left_0, projects_left_1)
# Print the p-value
print(paste("p-value:", result$p.value))




