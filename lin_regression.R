# import necessary libraries
library("data.table")
library("dplyr")
library("tidyr")
library('openxlsx')
library('fastDummies')
library('ggplot2')

# setting seed for reproducability
set.seed(123)

setwd("/home/jovyan/Stat_assignment")

# loading raw data
biomarkers <- as.data.table(read.xlsx("biomarkers.xlsx"))
covariates <- as.data.table(read.xlsx("covariates.xlsx"))

# producing a data frame with all raw data
# preprocessing includes: split of 1st column of "biomarkers.xlsx" into two columns
# with PatientID and timestamp
full_data <- biomarkers %>% separate(Biomarker, into=c("PatientID", "timestamp"), sep="-", convert=TRUE) %>%
  left_join(covariates, by = "PatientID")

# saving markers' names
markers <- colnames(biomarkers)[2:10]

full_set <- filter(full_data, timestamp == "0weeks")

# clearing data from rows with NA
full_set <- na.omit(full_set)

# conversion of categorical variables into dummies
full_set <- dummy_cols(full_set, select_columns=c('Sex.(1=male,.2=female)', 'Smoker.(1=yes,.2=no)'))

#cleaning dataset from unused columns
full_set[,c("PatientID", "timestamp", "Sex.(1=male,.2=female)", "Smoker.(1=yes,.2=no)"):=NULL]

# renaming columns to avoid special chars
col_n <- colnames(full_set)
setnames(full_set, old = col_n, new=c('IL8', 'VEGFA', 'OPG', 'TGFbeta1', 'IL6', 'CXCL9', 'CXCL1', 'IL18', 'CSF1', 'Age', 'VASincl', 'VAS12m', 'sex', 'female', 'smoker','nonsmoker'))

# split the dataset into training and testing
full_set$id <- 1:nrow(full_set)
train <- full_set %>% dplyr::sample_frac(0.8)
test  <- dplyr::anti_join(full_set, train, by = 'id')

# ordering training and testing datasets by VAS-12months value
train <- train[order(train$VAS12m)]
test <- test[order(test$VAS12m)]

# check correlation matrix in train
print(cor(train))

# creating a formula with set of explanatory variables
var_names <- col_n[-12]
var_names <- var_names[! var_names %in% c("VEGFA", "IL8", "CXCL1", "female", "nonsmoker")]
fmla <- as.formula(paste("VAS12m ~ ", paste(var_names, collapse= "+")))

# fit the model on training data and print the summary table
model <- lm(fmla, data=train)
print(summary(model))


# making predictions based on train data
y_pred <- predict(model, train)
fit_df <- data.frame(pred=y_pred, actual=train$VAS12m)
fit_df$index <- 1:nrow(fit_df)

# adjusting position on chart titles
theme_update(plot.title = element_text(hjust = 0.5))

# plot standard model fit charts
plot(model)

# creating visualisation of predictions vs. actuals of training dataset
print(ggplot(fit_df, aes(x=index, y=pred)) + geom_point(color="red") 
      + geom_smooth(method = "lm")  + geom_point(aes(x=index, y=actual), color="green")
      + labs(title="Training data", x=" ", y="VAS 12-months"))

# making predictions based on test data
y_pred <- predict(model, test)
fit_df <- data.frame(pred=y_pred, actual=test$VAS12m)
fit_df$index <- 1:nrow(fit_df)
# creating visualisation of predictions vs. actuals of test dataset
print(ggplot(fit_df, aes(x=index, y=pred)) + geom_point(color="red")
      + geom_smooth(method = "lm") + geom_point(aes(x=index, y=actual), color="green")
      + labs(title="Test data", x=" ", y="VAS 12-months"))

# calculation of RSE for test dataset
print(sqrt(sum((fit_df$pred-fit_df$actual)^2)/(nrow(test)-(length(var_names)+3))))

