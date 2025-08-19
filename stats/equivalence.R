library(dplyr)
library(TOSTER)

# Load filtered data
d = read.csv("../data/LLM-Persuasion-FINAL-July1_R.csv")
nrow(d)
head(d)


# Dummy code topics and conditions
d$topic_1 <- ifelse(d$topic=="1",1,0)
d$topic_2 <- ifelse(d$topic=="2",1,0)
d$topic_3 <- ifelse(d$topic=="3",1,0)
d$topic_4 <- ifelse(d$topic=="4",1,0)

d$ai_condition <- ifelse(d$condition=="LLM",1,0)
d$human_condition <- ifelse(d$condition=="HUM",1,0)
d$nolabel_condition <- ifelse(d$condition=="NONE",1,0)


# Equivalence test
run_equivalence_test_raw <- function(condition_1, condition_2, equivalence_bound = 3) {
  # Calculate the differences between post_support and pre_support
  diff_1 <- d$post_support[d[[condition_1]] == 1] - d$pre_support[d[[condition_1]] == 1]
  diff_2 <- d$post_support[d[[condition_2]] == 1] - d$pre_support[d[[condition_2]] == 1]
  
  # Calculate sample means and standard errors for the differences
  cond1_mean <- mean(diff_1, na.rm = TRUE)
  cond1_sd <- sd(diff_1, na.rm = TRUE) 
  cond1_n <- length(diff_1[!is.na(diff_1)])
  
  cond2_mean <- mean(diff_2, na.rm = TRUE)
  cond2_sd <- sd(diff_2, na.rm = TRUE) 
  cond2_n <- length(diff_2[!is.na(diff_2)])
  
  # Run the equivalence test using TOSTER
  tost_results <- tsum_TOST(
    m1 = cond1_mean,
    sd1 = cond1_sd,
    n1 = cond1_n,
    m2 = cond2_mean,
    sd2 = cond2_sd,
    n2 = cond2_n,
    eqb = equivalence_bound, 
    alpha = 0.05
  )
  
  return(tost_results)
}


# AI v. Human Label
equivalence_bounds <- seq(0.05, 10, by = 0.05)
results_df_human <- data.frame()

for (eqb in equivalence_bounds) {
  tost_result <- run_equivalence_test_raw('ai_condition', 'human_condition', eqb)
  
  # Extract relevant information
  result_row <- data.frame(
    equivalence_bound = eqb,
    p_value = tost_result$TOST$p.value[1],        # Extract p-value from the t-test
    lower_bound = tost_result$TOST$p.value[2],    # Extract p-value from TOST Lower
    upper_bound = tost_result$TOST$p.value[3]     # Extract p-value from TOST Upper
  )
  results_df_human <- rbind(results_df_human, result_row)
}
write.csv(results_df_human, "../results/equivalence_test_ai_human.csv", row.names = FALSE)


# AI v. No Label
equivalence_bounds <- seq(0, 10, by = 0.05)
results_df_none <- data.frame()

for (eqb in equivalence_bounds) {
  tost_result <- run_equivalence_test_raw('ai_condition', 'nolabel_condition', equivalence_bound = eqb)
  
  # Extract relevant information
  result_row <- data.frame(
    equivalence_bound = eqb,
    p_value = tost_result$TOST$p.value[1],        # Extract p-value from the t-test
    lower_bound = tost_result$TOST$p.value[2],    # Extract p-value from TOST Lower
    upper_bound = tost_result$TOST$p.value[3]     # Extract p-value from TOST Upper
  )
  results_df_none <- rbind(results_df_none, result_row)
}
write.csv(results_df_none, "../results/equivalence_test_ai_none.csv", row.names = FALSE)
