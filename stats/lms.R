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


# Model 1
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

# Model 2
model2 <- lm(post_support ~ pre_support + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d)
model2_confidence <- lm(post_confidence ~ pre_confidence + ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d)
model2_sharing <- lm(post_sharing ~ ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d)
model2_accuracy <- lm(post_accuracy ~  ai_condition + human_condition + topic_2 + topic_3 + topic_4, data=d)
summary(model2)
summary(model2_confidence)
summary(model2_sharing)
summary(model2_accuracy)


# Interaction with political party
d$democrat <- ifelse(d$PARTY=="Democrat",1,0)
d$republican <- ifelse(d$PARTY=="Republican",1,0)
d$independent <- ifelse(d$PARTY=="Independent",1,0)
model_party <- lm(post_support ~ pre_support + 
                    ai_condition*democrat + ai_condition*republican + ai_condition*independent + 
                    nolabel_condition*democrat + nolabel_condition*republican + nolabel_condition*independent + 
                    topic_2 + topic_3 + topic_4, 
                  data=d)


# Interaction with prior knowledge
ordered_levels_knowledge <- c(
  'I have little to no knowledge about this topic.',
  'I am slightly knowledgeable about this topic.',
  'I am moderately knowledgeable about this topic.',
  'I am very knowledgeable about this topic.',
  'I am an expert on this topic.'
)
d$pre_knowledge_num <- match(d$pre_knowledge, ordered_levels_knowledge) - 1
d$pre_knowledge_continuous <- d$pre_knowledge_num / (length(ordered_levels_knowledge) - 1)
model_knowledge <- lm(post_support ~ pre_support + 
                        ai_condition*pre_knowledge_continuous + 
                        nolabel_condition*pre_knowledge_continuous + 
                        topic_2 + topic_3 + topic_4, 
                      data=d)


# Interaction with prior experience
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
model_experience <- lm(post_support ~ pre_support + 
                         ai_condition*prior_experience_continuous + 
                         nolabel_condition*prior_experience_continuous + 
                         topic_2 + topic_3 + topic_4, 
                       data=d)


# Interaction with education level
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
model_education <- lm(post_support ~ pre_support + 
                        ai_condition*education_continuous + 
                        nolabel_condition*education_continuous + 
                        topic_2 + topic_3 + topic_4, 
                      data=d)


# Interaction with age
model_age <- lm(post_support ~ pre_support + 
                  ai_condition*Age + 
                  nolabel_condition*Age + 
                  topic_2 + topic_3 + topic_4, 
                data=d)


# Extract and combine the data for all models
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

model_party_info <- extract_model_info(model_party)
model_knowledge_info <- extract_model_info(model_knowledge)
model_experience_info <- extract_model_info(model_experience)
model_education_info <- extract_model_info(model_education)
model_age_info <- extract_model_info(model_age)


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

model_party_info$Model <- "Model Party"
model_knowledge_info$Model <- "Model Knowledge"
model_experience_info$Model <- "Model Experience"
model_education_info$Model <- "Model Education"
model_age_info$Model <- "Model Age"


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
  model_party_info, 
  model_knowledge_info, 
  model_experience_info, 
  model_education_info, 
  model_age_info
)

combined_info <- cbind(Variable = rownames(combined_info), combined_info)
rownames(combined_info) <- paste(combined_info$Model, combined_info$Variable, sep = "_")
combined_info$Model <- NULL

combined_info_rounded <- combined_info
numeric_columns <- sapply(combined_info_rounded, is.numeric)
combined_info_rounded[numeric_columns] <- lapply(combined_info_rounded[numeric_columns], signif, digits = 1)

table_latex <- kable(
  combined_info_rounded, 
  format = "latex", 
  booktabs = TRUE, 
  caption = "Model Comparison"
) %>%
  kable_styling(latex_options = c("hold_position"))


# Save the LaTeX table to a .tex file
cat(table_latex, file = "../results/lms.tex")

