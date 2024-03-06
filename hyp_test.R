# import necessary libraries
library("data.table")
library("dplyr")
library("tidyr")
library('openxlsx')
library('ggplot2')

# loading raw data
biomarkers <- as.data.table(read.xlsx("biomarkers.xlsx"))
covariates <- as.data.table(read.xlsx("covariates.xlsx"))

# setting significance level
alpha = 0.05

# producing a data frame with all raw data
# preprocessing includes: split of 1st column of "biomarkers.xlsx" into two columns
# with PatientID and timestamp
full_data <- biomarkers %>% separate(Biomarker, into=c("PatientID", "timestamp"), sep="-", convert=TRUE) %>%
  left_join(covariates, by = "PatientID")

# saving markers' names
markers <- colnames(biomarkers)[2:10]


# question:
# Do the biomarker levels at inclusion for patients with high VAS (>=5) differ from those for patients with low VAS (< 5)
#
# create test sets by selecting data with timestamp = "0weeks" and VAS at inclusion higher or lower than 5.

high_VAS_set <- full_data[timestamp == "0weeks" & `VAS-at-inclusion` >= 5]
low_VAS_set <- full_data[timestamp == "0weeks" & `VAS-at-inclusion` < 5]

# create empty data frame to store results
result <- data.frame(marker=c(), p_value=c())

# create empty data frame to store data for visualisation
graph.df <- data.frame(vas_val=c(), vas_level=c())

# perform t-tests for each marker
# store results in result data frame
for (col_n in markers) {

  set_high = high_VAS_set[ ,get(col_n)]
  set_low = low_VAS_set[ ,get(col_n)]
  
  test_result <- t.test(set_high, set_low, conf.level=(1-alpha), alternative = "two.sided")

  result_tmp <- data.frame(marker=col_n, p_value=test_result$p.value)
  result <- rbind(result, result_tmp)

# saving confidence intervals and markers for visualisation  
  graph.df <- rbind(graph.df, data.frame(conf_inter=c(test_result$conf.int[1], test_result$conf.int[2]), marker=col_n))
  
}

print(c("Given level of significance: ", alpha))
print("Reject Ho for these markers:")
print(filter(result, p_value<alpha))
print("Confirm Ho:")
print(filter(result, p_value>=alpha))
print('With Bonferroni correction')
print(filter(result, p_value<(alpha/length(markers))))

# making visualisation for the report
print(ggplot(graph.df, aes(x=conf_inter, y=marker)) + geom_point(colour="red") +
        geom_line() + geom_vline(xintercept = 0, size = 0.5, color="blue") +
        ylab("Biomarker") + xlab("Confidence intervals for mean differences") +
        labs(title="Summary of t-test results"))
