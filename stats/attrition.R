library(dplyr)
library(mgcv)
library(ggplot2)
library(tidyr)
library(quantreg)
library(kableExtra)


# Load filtered data
d = read.csv("../data/LLM-Persuasion-FINAL-July1_attrition_R.csv")


# Dummy code topics and conditions
d$topic_1 <- ifelse(d$topic=="1",1,0)
d$topic_2 <- ifelse(d$topic=="2",1,0)
d$topic_3 <- ifelse(d$topic=="3",1,0)
d$topic_4 <- ifelse(d$topic=="4",1,0)

d$ai_condition <- ifelse(d$condition=="LLM",1,0)
d$human_condition <- ifelse(d$condition=="HUM",1,0)
d$nolabel_condition <- ifelse(d$condition=="NONE",1,0)

d$valid_participant <- ifelse(d$valid_participant == "True", 1, 0)


# Formatting
extract_model_info <- function(model) {
  summary_model <- summary(model)
  confint_model <- confint(model)
  
  coefs <- summary_model$coefficients
  
  p_col <- ifelse("Pr(>|z|)" %in% colnames(coefs),
                  "Pr(>|z|)",
                  "Pr(>|t|)")
  
  p_values <- coefs[, p_col]
  
  formatted_p_values <- ifelse(p_values < 0.001, "<0.001",
                               formatC(p_values, format = "f", digits = 3))
  
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*",
                                       ifelse(p_values < 0.10, "\\dag", ""))))
  
  b_values <- paste0(formatC(coefs[, "Estimate"], format = "f", digits = 2), significance)
  
  data.frame(
    b = b_values,
    SE = formatC(coefs[, "Std. Error"], format = "f", digits = 2),
    p_value = formatted_p_values,
    CI_Lower = formatC(confint_model[, 1], format = "f", digits = 2),
    CI_Upper = formatC(confint_model[, 2], format = "f", digits = 2)
  )
}


# Model
model <- glm(valid_participant ~ human_condition + nolabel_condition + topic_2 + topic_3 + topic_4, 
             data=d, family=binomial)
summary(model)
model_info <- extract_model_info(model)


# Table
model_name <- "Attrition"
combined_info <- cbind(Variable = rownames(model_info), model_info)
rownames(combined_info) <- paste(model_name, combined_info$Variable, sep = "_")
if ("Model" %in% colnames(combined_info)) {
  combined_info$Model <- NULL
}

combined_info_rounded <- combined_info
numeric_columns <- sapply(combined_info_rounded, is.numeric)
combined_info_rounded[numeric_columns] <- lapply(
  combined_info_rounded[numeric_columns], 
  signif, 
  digits = 1
)

table_latex <- kable(
  combined_info_rounded,
  format = "latex",
  booktabs = TRUE,
  caption = model_name
) %>%
  kable_styling(latex_options = c("hold_position"))

# Save LaTeX table to file
cat(table_latex, file = "../results/lms_attrition.tex")
