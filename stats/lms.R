library(dplyr)
library(knitr)
library(kableExtra)

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


# Formatting
extract_model_info <- function(model) {
  summary <- summary(model)
  confint <- confint(model)
  
  coefs <- summary$coefficients
  p_values <- coefs[, "Pr(>|t|)"]
  
  # Format p-values
  formatted_p_values <- ifelse(p_values < 0.001, "<0.001", formatC(p_values, format = "f", digits = 3))
  
  # Add significance stars to coefficients
  significance <- ifelse(p_values < 0.001, "***", 
                         ifelse(p_values < 0.01, "**", 
                                ifelse(p_values < 0.05, "*", 
                                       ifelse(p_values < 0.10, "\\dag", ""))))
  
  b_values <- paste0(formatC(coefs[, "Estimate"], format = "f", digits = 2), significance)
  
  data.frame(
    b = b_values,
    SE = formatC(coefs[, "Std. Error"], format = "f", digits = 2),
    p_value = formatted_p_values,
    CI_Lower = formatC(confint[, 1], format = "f", digits = 2),
    CI_Upper = formatC(confint[, 2], format = "f", digits = 2)
  )
}


###
### Model 1: AI v. Human ###
###
model1 <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d)
model1_confidence <- lm(post_confidence ~ pre_confidence + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d)
model1_sharing <- lm(post_sharing ~ ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d)
model1_accuracy <- lm(post_accuracy ~  ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d)
topic1 <- lm(post_support ~ pre_support + ai_condition + nolabel_condition, data=d[which(d$topic=="1"),])
topic2 <- lm(post_support ~ pre_support + ai_condition + nolabel_condition, data=d[which(d$topic=="2"),])
topic3 <- lm(post_support ~ pre_support + ai_condition + nolabel_condition, data=d[which(d$topic=="3"),])
topic4 <- lm(post_support ~ pre_support + ai_condition + nolabel_condition, data=d[which(d$topic=="4"),])
summary(model1)
summary(model1_confidence)
summary(model1_sharing)
summary(model1_accuracy)


###
### Model 2: AI v. No Label ###
###
model2 <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d)
model2_confidence <- lm(post_confidence ~ pre_confidence + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d)
model2_sharing <- lm(post_sharing ~ ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d)
model2_accuracy <- lm(post_accuracy ~  ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d)
summary(model2)
summary(model2_confidence)
summary(model2_sharing)
summary(model2_accuracy)

###
### Interaction with political party ###
###
d$democrat <- ifelse(d$PARTY=="Democrat",1,0)
d$republican <- ifelse(d$PARTY=="Republican",1,0)
d$independent <- ifelse(d$PARTY=="Independent",1,0)
model1_party <- lm(post_support ~ pre_support + 
                    ai_condition*democrat + ai_condition*republican + ai_condition*independent + 
                    nolabel_condition*democrat + nolabel_condition*republican + nolabel_condition*independent + 
                    topic_2 + topic_3 + topic_4, 
                  data=d)
model2_party <- lm(post_support ~ pre_support + 
                     ai_condition*democrat + ai_condition*republican + ai_condition*independent + 
                     human_condition*democrat + human_condition*republican + human_condition*independent + 
                     topic_2 + topic_3 + topic_4, 
                   data=d)
summary(model1_party)
summary(model2_party)

# Subgroups
d_democrat <- subset(d, PARTY=="Democrat")
d_republican <- subset(d, PARTY=="Republican")
d_independent <- subset(d, PARTY=="Independent")

model1_democrat <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4,data=d_democrat)
model1_republican <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_republican)
model1_independent <- lm(post_support ~ pre_support +  ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_independent)

model2_democrat <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_democrat)
model2_republican <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_republican)
model2_independent <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_independent)

summary(model1_democrat)
summary(model1_republican)
summary(model1_independent)
summary(model2_democrat)
summary(model2_republican)
summary(model2_independent)


###
### Interaction with prior knowledge ###
####
ordered_levels_knowledge <- c(
  'I have little to no knowledge about this topic.',
  'I am slightly knowledgeable about this topic.',
  'I am moderately knowledgeable about this topic.',
  'I am very knowledgeable about this topic.',
  'I am an expert on this topic.'
)
d$pre_knowledge_num <- match(d$pre_knowledge, ordered_levels_knowledge) - 1
d$pre_knowledge_continuous <- d$pre_knowledge_num / (length(ordered_levels_knowledge) - 1)
model1_knowledge <- lm(post_support ~ pre_support + 
                        ai_condition*pre_knowledge_continuous + 
                        nolabel_condition*pre_knowledge_continuous + 
                        topic_2 + topic_3 + topic_4, 
                      data=d)
model2_knowledge <- lm(post_support ~ pre_support + 
                        ai_condition*pre_knowledge_continuous + 
                        human_condition*pre_knowledge_continuous + 
                        topic_2 + topic_3 + topic_4, 
                      data=d)
summary(model1_knowledge)
summary(model2_knowledge)

# Subgroups
d_low_knowledge <- subset(d, pre_knowledge %in% c("I have little to no knowledge about this topic.", "I am slightly knowledgeable about this topic."))
d_moderate_knowledge <- subset(d, pre_knowledge == "I am moderately knowledgeable about this topic.")
d_high_knowledge <- subset(d, pre_knowledge %in% c("I am very knowledgeable about this topic.", "I am an expert on this topic."))

model1_low_know <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_low_knowledge)
model1_moderate_know <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_moderate_knowledge)
model1_high_know <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_high_knowledge)

model2_low_know <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_low_knowledge)
model2_moderate_know <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_moderate_knowledge)
model2_high_know <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_high_knowledge)

summary(model1_low_know)
summary(model1_moderate_know)
summary(model1_high_know)
summary(model2_low_know)
summary(model2_moderate_know)
summary(model2_high_know)

###
### Interaction with prior experience ###
###
ordered_levels_experience <- c(
  'I have never heard of conversational AI or LLMs.',
  'I never use conversational AI or LLMs.',
  'I use conversational AI or LLMs less than once a month.',
  'I use conversational AI or LLMs about once a month.',
  'I use conversational AI or LLMs about once a week.',
  'I use conversational AI or LLMs more than once a week.'
)
d$prior_experience_num <- match(d$EXPERIENCE, ordered_levels_experience) - 1
d$prior_experience_continuous <- d$prior_experience_num / (length(ordered_levels_experience) - 1)
model1_experience <- lm(post_support ~ pre_support + 
                         ai_condition*prior_experience_continuous + 
                         nolabel_condition*prior_experience_continuous + 
                         topic_2 + topic_3 + topic_4, 
                       data=d)
model2_experience <- lm(post_support ~ pre_support + 
                         ai_condition*prior_experience_continuous + 
                         human_condition*prior_experience_continuous + 
                         topic_2 + topic_3 + topic_4, 
                       data=d)
summary(model1_experience)
summary(model2_experience)

# Subgroups
d_low_experience <- subset(d, EXPERIENCE %in% c("I have never heard of conversational AI or LLMs.", "I never use conversational AI or LLMs.", "I use conversational AI or LLMs less than once a month."))
d_high_experience <- subset(d, EXPERIENCE %in% c("I use conversational AI or LLMs about once a month.", "I use conversational AI or LLMs about once a week.", "I use conversational AI or LLMs more than once a week."))

model1_low_exp <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_low_experience)
model1_high_exp <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_high_experience)

model2_low_exp <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_low_experience)
model2_high_exp <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_high_experience)

summary(model1_low_exp)
summary(model1_high_exp)
summary(model2_low_exp)
summary(model2_high_exp)


###
### Interaction with education level ###
###
ordered_levels_education <- c(
  "Did not receive high school diploma",
  "High school graduate",
  "GED or equivalent",
  "Some college",
  "2-year degree (e.g., associate degree)",
  "Bachelor degree",
  "Master's degree",
  "Professional or academic doctorate degree"
)
d$education_num <- match(d$EDUC, ordered_levels_education) - 1
d$education_continuous <- d$education_num / (length(ordered_levels_education) - 1)
model1_education <- lm(post_support ~ pre_support + 
                        ai_condition*education_continuous + 
                        nolabel_condition*education_continuous + 
                        topic_2 + topic_3 + topic_4, 
                      data=d)
model2_education <- lm(post_support ~ pre_support + 
                        ai_condition*education_continuous + 
                         human_condition*education_continuous + 
                        topic_2 + topic_3 + topic_4, 
                      data=d)
summary(model1_education)
summary(model2_education)

# Subgroups
d_low_educ <- subset(d, EDUC %in% c("Did not receive high school diploma", "High school graduate", "GED or equivalent", "Some college", "2-year degree (e.g., associate degree)"))
d_high_educ <- subset(d, EDUC %in% c("Bachelor degree", "Master's degree", "Professional or academic doctorate degree"))

model1_low_educ <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_low_educ)
model1_high_educ <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_high_educ)

model2_low_educ <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_low_educ)
model2_high_educ <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_high_educ)

summary(model1_low_educ)
summary(model1_high_educ)
summary(model2_low_educ)
summary(model2_high_educ)

###
#### Interaction with age ###
###
model1_age <- lm(post_support ~ pre_support + 
                  ai_condition*Age + 
                  nolabel_condition*Age + 
                  topic_2 + topic_3 + topic_4, 
                data=d)
model2_age <- lm(post_support ~ pre_support + 
                  ai_condition*Age + 
                   human_condition*Age + 
                  topic_2 + topic_3 + topic_4, 
                data=d)
summary(model1_age)
summary(model2_age)

# Subgroups
d_below_median_age <- subset(d, Age < 38)
d_above_median_age <- subset(d, Age >= 38)

model1_below_median_age <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_below_median_age)
model1_above_median_age <- lm(post_support ~ pre_support + ai_condition + nolabel_condition + topic_2 + topic_3 + topic_4, data=d_above_median_age)

model2_below_median_age <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_below_median_age)
model2_above_median_age <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d_above_median_age)

summary(model1_below_median_age)
summary(model1_above_median_age)
summary(model2_below_median_age)
summary(model2_above_median_age)

# GAM Model
gam_model <- gam(post_support ~ pre_support + s(Age, by=ai_condition) + s(Age, by=nolabel_condition) + topic_2 + topic_3 + topic_4, data=d)
summary(gam_model)
png("age_gam.png", width = 7, height = 5, units = "in", res=300)
plot(gam_model, select = 1, se = TRUE, shade = TRUE,
     main = "", ylab = "Effect of Age on Change in Support (AI Label)")
dev.off()

### 
### Extract and combine the data for all models ###
###
model1_info <- extract_model_info(model1)
model1_confidence_info <- extract_model_info(model1_confidence)
model1_sharing_info <- extract_model_info(model1_sharing)
model1_accuracy_info <- extract_model_info(model1_accuracy)

topic1_info <- extract_model_info(topic1)
topic2_info <- extract_model_info(topic2)
topic3_info <- extract_model_info(topic3)
topic4_info <- extract_model_info(topic4)

model2_info <- extract_model_info(model2)
model2_confidence_info <- extract_model_info(model2_confidence)
model2_sharing_info <- extract_model_info(model2_sharing)
model2_accuracy_info <- extract_model_info(model2_accuracy)

model1_party_info <- extract_model_info(model1_party)
model1_knowledge_info <- extract_model_info(model1_knowledge)
model1_experience_info <- extract_model_info(model1_experience)
model1_education_info <- extract_model_info(model1_education)
model1_age_info <- extract_model_info(model1_age)

model2_party_info <- extract_model_info(model2_party)
model2_knowledge_info <- extract_model_info(model2_knowledge)
model2_experience_info <- extract_model_info(model2_experience)
model2_education_info <- extract_model_info(model2_education)
model2_age_info <- extract_model_info(model2_age)

model1_democrat_info <- extract_model_info(model1_democrat)
model1_republican_info <- extract_model_info(model1_republican)
model1_independent_info <- extract_model_info(model1_independent)

model2_democrat_info <- extract_model_info(model2_democrat)
model2_republican_info <- extract_model_info(model2_republican)
model2_independent_info <- extract_model_info(model2_independent)

model1_low_know_info <- extract_model_info(model1_low_know)
model1_moderate_know_info <- extract_model_info(model1_moderate_know)
model1_high_know_info <- extract_model_info(model1_high_know)

model2_low_know_info <- extract_model_info(model2_low_know)
model2_moderate_know_info <- extract_model_info(model2_moderate_know)
model2_high_know_info <- extract_model_info(model2_high_know)

model1_low_exp_info <- extract_model_info(model1_low_exp)
model1_high_exp_info <- extract_model_info(model1_high_exp)

model2_low_exp_info <- extract_model_info(model2_low_exp)
model2_high_exp_info <- extract_model_info(model2_high_exp)

model1_low_educ_info <- extract_model_info(model1_low_educ)
model1_high_educ_info <- extract_model_info(model1_high_educ)

model2_low_educ_info <- extract_model_info(model2_low_educ)
model2_high_educ_info <- extract_model_info(model2_high_educ)

model1_below_median_age_info <- extract_model_info(model1_below_median_age)
model1_above_median_age_info <- extract_model_info(model1_above_median_age)

model2_below_median_age_info <- extract_model_info(model2_below_median_age)
model2_above_median_age_info <- extract_model_info(model2_above_median_age)

# Add model names to the extracted data
model1_info$Model <- "Model 1"
model1_confidence_info$Model <- "Model 1 Confidence"
model1_sharing_info$Model <- "Model 1 Sharing"
model1_accuracy_info$Model <- "Model 1 Accuracy"

topic1_info$Model <- "Topic 1"
topic2_info$Model <- "Topic 2"
topic3_info$Model <- "Topic 3"
topic4_info$Model <- "Topic 4"

model2_info$Model <- "Model 2"
model2_confidence_info$Model <- "Model 2 Confidence"
model2_sharing_info$Model <- "Model 2 Sharing"
model2_accuracy_info$Model <- "Model 2 Accuracy"

model1_party_info$Model <- "Model 1 Party"
model1_knowledge_info$Model <- "Model 1 Knowledge"
model1_experience_info$Model <- "Model 1 Experience"
model1_education_info$Model <- "Model 1 Education"
model1_age_info$Model <- "Model 1 Age"

model2_party_info$Model <- "Model 2 Party"
model2_knowledge_info$Model <- "Model 2 Knowledge"
model2_experience_info$Model <- "Model 2 Experience"
model2_education_info$Model <- "Model 2 Education"
model2_age_info$Model <- "Model 2 Age"

model1_democrat_info$Model <- "Model 1 Democrat"
model1_republican_info$Model <- "Model 1 Republican"
model1_independent_info$Model <- "Model 1 Independent"

model2_democrat_info$Model <- "Model 2 Democrat"
model2_republican_info$Model <- "Model 2 Republican"
model2_independent_info$Model <- "Model 2 Independent"

model1_low_know_info$Model <- "Model 1 Low Knowledge"
model1_moderate_know_info$Model <- "Model 1 Moderate Knowledge"
model1_high_know_info$Model <- "Model 1 High Knowledge"

model2_low_know_info$Model <- "Model 2 Low Knowledge"
model2_moderate_know_info$Model <- "Model 2 Moderate Knowledge"
model2_high_know_info$Model <- "Model 2 High Knowledge"

model1_low_exp_info$Model <- "Model 1 Low Experience"
model1_high_exp_info$Model <- "Model 1 High Experience"

model2_low_exp_info$Model <- "Model 2 Low Experience"
model2_high_exp_info$Model <- "Model 2 High Experience"

model1_low_educ_info$Model <- "Model 1 Low Education"
model1_high_educ_info$Model <- "Model 1 High Education"

model2_low_educ_info$Model <- "Model 2 Low Education"
model2_high_educ_info$Model <- "Model 2 High Education"

model1_below_median_age_info$Model <- "Model 1 Below Median Age"
model1_above_median_age_info$Model <- "Model 1 Above Median Age"

model2_below_median_age_info$Model <- "Model 2 Below Median Age"
model2_above_median_age_info$Model <- "Model 2 Above Median Age"

# Combine all extracted information
combined_info <- rbind(
  model1_info, 
  model1_confidence_info,
  model1_sharing_info,
  model1_accuracy_info,
  topic1_info,
  topic2_info,
  topic3_info,
  topic4_info,
  model2_info, 
  model2_confidence_info,
  model2_sharing_info,
  model2_accuracy_info,
  model1_party_info, 
  model1_knowledge_info, 
  model1_experience_info, 
  model1_education_info, 
  model1_age_info,
  model2_party_info, 
  model2_knowledge_info, 
  model2_experience_info, 
  model2_education_info, 
  model2_age_info,
  model1_democrat_info, 
  model1_republican_info, 
  model1_independent_info, 
  model2_democrat_info, 
  model2_republican_info, 
  model2_independent_info,
  model1_low_know_info, 
  model1_moderate_know_info, 
  model1_high_know_info,
  model2_low_know_info, 
  model2_moderate_know_info, 
  model2_high_know_info,
  model1_low_exp_info, 
  model1_high_exp_info,
  model2_low_exp_info, 
  model2_high_exp_info,
  model1_low_educ_info, 
  model1_high_educ_info,
  model2_low_educ_info, 
  model2_high_educ_info,
  model1_below_median_age_info,
  model1_above_median_age_info,
  model2_below_median_age_info,
  model2_above_median_age_info
)


# Format for Latex
combined_info <- cbind(Variable = rownames(combined_info), combined_info)
rownames(combined_info) <- paste(combined_info$Model, combined_info$Variable, sep = "_")
combined_info$Model <- NULL

combined_info_rounded <- combined_info
numeric_columns <- sapply(combined_info_rounded, is.numeric)
combined_info_rounded[numeric_columns] <- lapply(combined_info_rounded[numeric_columns], signif, digits = 1)

combined_info_no_topics <- combined_info_rounded[!grepl("topic", combined_info_rounded$Variable), ]


modify_text <- function(x) {
  modified_text <- gsub("_.*", "", x) 
  modified_text <- gsub("_", " ", modified_text)
  return(modified_text)
}

create_latex_table <- function(data) {
  data$Variable <- sapply(data$Variable, modify_text)
  
  # Initialize the LaTeX table with kable
  table_latex <- kable(data, format = "latex", booktabs = TRUE, caption = "Model Comparison") %>%
    kable_styling(latex_options = c("hold_position"))
  
  for (model in models) {
    model_rows <- grep(model, rownames(data))
    if (length(model_rows) > 0) {
      table_latex <- gsub(paste0("\\\\", model), paste0("\\\\hline \n", model), table_latex)
    }
  }
  
  return(table_latex)
}

table_latex <- create_latex_table(combined_info_no_topics)
cat(table_latex, file = "../results/lms_new2.tex")

