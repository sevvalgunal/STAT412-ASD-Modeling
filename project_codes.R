# https://embrace-autism.com/aq-10/
# https://www.kaggle.com/datasets/shivamshinde123/autismprediction


# Data Cleaning and Tidying

library(data.table)
train <- fread("train.csv")
test <- fread("test.csv")

library(dplyr)
glimpse(train)
glimpse(test)
summary(train)

sapply(train, function(x) sum(is.na(x)))
str(train)

sapply(train, function(x) sum(x == "?", na.rm = TRUE))
train[train == "?"] <- NA
sapply(train, function(x) sum(is.na(x)))

table(train$ethnicity)
train$ethnicity[train$ethnicity == "others"] <- "Other"
train$ethnicity[train$ethnicity == "Others"] <- "Other"

sapply(train, function(x) if(is.character(x) || is.factor(x)) unique(x))
sapply(train, function(x) length(unique(x)))

train$result <- as.numeric(train$result)
train$age <- as.numeric(train$age)

train$age <- round(train$age, digits = 0)

sum(train$result < 0 | train$result > 10)
sum(train$age < 18)

train <- train %>% mutate(correct_result = rowSums(across(A1_Score:A10_Score), na.rm = TRUE)) 

library(magrittr)
train %>%
  mutate(age_numeric = as.numeric(age)) %>%
  mutate(is_under18 = age_numeric < 18) %>%
  group_by(relation) %>%
  summarise(
    under18_count = sum(is_under18, na.rm = TRUE),
    total = n(),
    under18_rate = mean(is_under18, na.rm = TRUE)
  )

train$age <- as.numeric(train$age)
train$age_missing <- ifelse(train$age < 18, 1, 0)
train %>%
  group_by(`Class/ASD`) %>%
  summarise(
    n = n(),
    n_invalid_age = sum(age_missing, na.rm = TRUE),
    rate_invalid = mean(age_missing, na.rm = TRUE)
  )
table_age_class <- table(train$age_missing, train$`Class/ASD`)
chisq.test(table_age_class)

train$age <- ifelse(train$age < 18, NA, train$age)


# Visualizing Missingness

library(naniar)
library(visdat)
library(VIM)
library(ggplot2)

gg_miss_var(train) + labs(title = "Missing Values by Variable", x = "Variables", y = "Number of Missing Observations")

vis_miss(train)

gg_miss_upset(train)

train$ethnicity_missing <- ifelse(is.na(train$ethnicity), 1, 0)
train$relation_missing <- ifelse(is.na(train$relation), 1, 0)


# Example: Ethnicity missingness vs ASD class
chisq.test(table(train$ethnicity_missing, train$`Class/ASD`))
chisq.test(table(train$relation_missing, train$`Class/ASD`))

train %>%
  group_by(`Class/ASD`) %>%
  summarise(
    ethnicity_missing = mean(ethnicity_missing, na.rm = TRUE),
    relation_missing = mean(relation_missing, na.rm = TRUE)
  )

chisq.test(table(train$ethnicity_missing, train$`Class/ASD`))



# EDA

summary(train[c(12:22,27)])
sum(is.na(train$ethnicity))
sum(is.na(train$relation))

library(plyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(dplyr)

summary(train$age)

ggplot(train, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age") +
  theme_minimal()

summary(train$correct_result)

ggplot(train, aes(x = correct_result)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of AQ Scores", x = "AQ Score (0???10)", y = "Count") +
  theme_minimal()

ggplot(train, aes(x = gender, fill = factor(`Class/ASD`))) +
  geom_bar(position = "fill") +
  labs(title = "Gender Proportion by ASD Diagnosis", y = "Proportion", fill = "ASD") +
  theme_minimal()

train %>%
  select(age, gender, ethnicity, jaundice, austim, used_app_before, correct_result, `Class/ASD`) %>%
  tbl_summary(by = `Class/ASD`, missing = "no") %>%
  add_p() %>%
  bold_labels()

library(ggalluvial)
install.packages("ggalluvial")
train %>%
  filter(!is.na(gender), !is.na(austim), !is.na(`Class/ASD`)) %>%
  ggplot(aes(axis1 = gender, axis2 = austim, axis3 = `Class/ASD`)) +
  geom_alluvium(aes(fill = gender), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "Family Autism", "ASD Class")) +
  labs(title = "Pathways to Diagnosis: Gender, Autism History, and ASD Class") +
  theme_minimal()


# Do AQ scores differ by Class/ASD?
library(dplyr)
library(gtsummary)
library(magrittr)

train %>%
  select(correct_result, `Class/ASD`) %>%
  mutate(`Class/ASD` = factor(`Class/ASD`, labels = c("No ASD", "ASD"))) %>%
  tbl_summary(
    by = `Class/ASD`,
    statistic = list(all_continuous() ~ "{mean} +/- {sd}")
  ) %>%
  add_p() %>%
  bold_labels()

library(ggplot2)

ggplot(train, aes(x = factor(`Class/ASD`, labels = c("No ASD", "ASD")),
                  y = correct_result, fill = factor(`Class/ASD`))) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "gray40") +
  labs(title = "AQ Score by ASD Diagnosis",
       x = "ASD Class",
       y = "AQ Score (0???10)",
       fill = "ASD Diagnosis") +
  theme_minimal()

shapiro.test(train$correct_result[train$`Class/ASD` == 0])
shapiro.test(train$correct_result[train$`Class/ASD` == 1])

t.test(correct_result ~ `Class/ASD`, data = train)

wilcox.test(correct_result ~ `Class/ASD`, data = train)


# Are diagnosed patients generally younger or older than non-diagnosed ones?

age_data <- train %>% filter(!is.na(age))

age_data %>%
  select(age, `Class/ASD`) %>%
  mutate(`Class/ASD` = factor(`Class/ASD`, labels = c("No ASD", "ASD"))) %>%
  tbl_summary(
    by = `Class/ASD`,
    statistic = list(all_continuous() ~ "{mean} +/- {sd}")
  ) %>%
  add_p() %>%
  bold_labels()

ggplot(age_data, aes(x = factor(`Class/ASD`, labels = c("No ASD", "ASD")),
                     y = age, fill = factor(`Class/ASD`))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "grey") +
  labs(title = "Age Distribution by ASD Diagnosis",
       x = "ASD Class", y = "Age (years)", fill = "Diagnosis") +
  theme_minimal() 

ggplot(age_data, aes(x = age, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Age by ASD Diagnosis",
       x = "Age (years)", y = "Density", fill = "ASD Class") +
  theme_minimal() +
  scale_fill_manual(values = c("No ASD" = "steelblue", "ASD" = "tomato"))

shapiro.test(age_data$age[age_data$`Class/ASD` == 0])
shapiro.test(age_data$age[age_data$`Class/ASD` == 1])

t.test(age ~ `Class/ASD`, data = age_data)

wilcox.test(age ~ `Class/ASD`, data = age_data)


# Are more males or females being diagnosed with ASD?

table(train$gender, train$`Class/ASD`)

gender_data <- train %>% filter(!is.na(gender))
chisq.test(table(gender_data$gender, gender_data$`Class/ASD`))

gender_data %>%
  ggplot(aes(x = gender, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ASD Diagnosis by Gender",
       x = "Gender", y = "Proportion", fill = "ASD Diagnosis") +
  theme_minimal()

gender_data %>%
  ggplot(aes(x = gender, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of ASD Diagnosis by Gender",
       x = "Gender", y = "Count", fill = "ASD Diagnosis") +
  theme_minimal()


# Is having a family history of autism (austim) associated with ASD diagnosis?

table(train$austim, train$`Class/ASD`)

austim_data <- train %>% filter(!is.na(austim))

chisq.test(table(austim_data$austim, austim_data$`Class/ASD`))

ggplot(austim_data, aes(x = austim, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of ASD Diagnosis by Family Autism History",
       x = "Family Autism History", y = "Count", fill = "ASD Diagnosis") +
  theme_minimal()

ggplot(austim_data, aes(x = austim, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ASD by Family Autism History",
       x = "Family Autism History", y = "Proportion", fill = "ASD Diagnosis") +
  theme_minimal()

# Do certain ethnic groups appear disproportionatily diagnosed?

ethnicity_data <- train %>% filter(!is.na(ethnicity))
table(ethnicity_data$ethnicity, ethnicity_data$`Class/ASD`)

library(forcats)

chisq.test(table(ethnicity_data$ethnicity, ethnicity_data$`Class/ASD`))
ethnicity_data <- ethnicity_data %>%
  mutate(ethnicity_grouped = fct_lump(factor(ethnicity), n = 6))
chisq.test(table(ethnicity_data$ethnicity_grouped, ethnicity_data$`Class/ASD`))

ggplot(ethnicity_data, aes(x = ethnicity_grouped,
                           fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "dodge") +
  labs(title = "ASD Diagnosis Count by Ethnicity",
       x = "Ethnicity", y = "Count", fill = "Diagnosis") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(ethnicity_data, aes(x = ethnicity_grouped,
                           fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ASD Diagnosis by Ethnicity",
       x = "Ethnicity", y = "Proportion", fill = "Diagnosis") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Are there outliers in age?

ggplot(train, aes(y = age)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Age", y = "Age (years)") +
  theme_minimal()

age_clean <- na.omit(train$age)

Q1 <- quantile(age_clean, 0.25)
Q3 <- quantile(age_clean, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers <- age_clean[age_clean < lower_bound | age_clean > upper_bound]
length(outliers)
outliers

z_scores <- scale(age_clean)
age_outliers_z <- age_clean[abs(z_scores) > 3]
length(age_outliers_z)

## 1. Are Older Adults More or Less Likely to Be Diagnosed with ASD?
older_group <- train %>% filter(!is.na(age) & age >= 72)
rest_group  <- train %>% filter(!is.na(age) & age < 72)

table(older_group$`Class/ASD`)
table(rest_group$`Class/ASD`)

prop.table(table(older_group$`Class/ASD`))
prop.table(table(rest_group$`Class/ASD`))

chisq.test(rbind(
  table(older_group$`Class/ASD`),
  table(rest_group$`Class/ASD`)
))

## 2. Do Their AQ Scores Behave Differently?
summary(older_group$correct_result)
summary(rest_group$correct_result)

combined_group <- train %>%
  filter(!is.na(age)) %>%
  mutate(AgeGroup = ifelse(age >= 72, "Older (72+)", "Younger (<72)"))

ggplot(combined_group, aes(x = AgeGroup, y = correct_result, fill = AgeGroup)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "gray40") +
  labs(title = "AQ Score by Age Group (Older Outliers vs Others)",
       x = "Age Group", y = "AQ Score (0???10)") +
  theme_minimal()


# Is having jaundice or prior use of screening tests related to being diagnosed?

table(train$jaundice, train$`Class/ASD`)
chisq.test(table(train$jaundice, train$`Class/ASD`))

ggplot(train, aes(x = jaundice, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of ASD by Jaundice History",
       x = "Jaundice at Birth", y = "Count", fill = "Diagnosis") +
  theme_minimal()

ggplot(train, aes(x = jaundice, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ASD by Jaundice History",
       x = "Jaundice at Birth", y = "Proportion", fill = "Diagnosis") +
  theme_minimal()

table(train$used_app_before, train$`Class/ASD`)
chisq.test(table(train$used_app_before, train$`Class/ASD`))

ggplot(train, aes(x = used_app_before, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of ASD by Prior Use of App",
       x = "Used App Before", y = "Count", fill = "Diagnosis") +
  theme_minimal()

ggplot(train, aes(x = used_app_before, fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ASD by Prior Use of App",
       x = "Used App Before", y = "Proportion", fill = "Diagnosis") +
  theme_minimal()


# Are missing age, ethnicity and relation more common in a specific group?

chisq.test(table(train$age_missing, train$`Class/ASD`))
ggplot(train, aes(x = factor(age_missing, labels = c("Available", "Missing")),
                  fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ASD by Age Missingness",
       x = "Age Missing", y = "Proportion", fill = "Diagnosis") +
  theme_minimal()

chisq.test(table(train$ethnicity_missing, train$`Class/ASD`))
ggplot(train, aes(x = factor(ethnicity_missing, labels = c("Available", "Missing")),
                  fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ASD by Ethnicity Missingness",
       x = "Ethnicity Missing", y = "Proportion", fill = "Diagnosis") +
  theme_minimal()

chisq.test(table(train$relation_missing, train$`Class/ASD`))
ggplot(train, aes(x = factor(relation_missing, labels = c("Available", "Missing")),
                  fill = factor(`Class/ASD`, labels = c("No ASD", "ASD")))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of ASD by Relation Missingness",
       x = "Relation Missing", y = "Proportion", fill = "Diagnosis") +
  theme_minimal()




# AGE Imputation

train$correct_result_std <- scale(train$correct_result)
cor(train$result, train$correct_result_std, use = "complete.obs")

train$age[train$age < 18] <- NA

library(mice)
imp_data <- train %>% select(age, gender, contry_of_res, correct_result_std, `Class/ASD`)
imp_data$gender <- as.factor(imp_data$gender)
imp <- mice(imp_data, method = "pmm", seed = 123)
train$age_imputed <- complete(imp)$age

densityplot(imp, ~age)


# Ethnicity and Relation Imputation

train$correct_result_std <- as.numeric(scale(train$correct_result))

imp_data_cat <- train %>%
  select(ethnicity, relation, gender, contry_of_res, `Class/ASD`, correct_result_std)

imp_data_cat <- imp_data_cat %>%
  mutate(across(c(ethnicity, relation, gender, contry_of_res), as.factor))

imp_methods <- make.method(imp_data_cat)
imp_methods["ethnicity"] <- "polyreg"
imp_methods["relation"]  <- "polyreg"

imp_cat <- mice(imp_data_cat, method = imp_methods, seed = 123)

train$ethnicity_imputed <- complete(imp_cat)$ethnicity
train$relation_imputed  <- complete(imp_cat)$relation



# Validating Imputations

plot(imp_cat, main = "Ethnicity & Relation") 

plot(imp, main = "Age")

library(lattice)

stripplot(imp_cat, ethnicity ~ .imp, pch = 20, cex = 1.2, col = "steelblue")

stripplot(imp_cat, relation ~ .imp, pch = 20, cex = 1.2, col = "darkgreen")

stripplot(imp, age ~ .imp, pch = 20, cex = 1.2, col = "darkgreen")

table(complete(imp_cat)$ethnicity)
table(complete(imp_cat)$relation)

table(train$ethnicity, useNA = "ifany")
table(train$relation, useNA = "ifany")




# CV

library(tidyverse)
library(caret)
library(mice)

train <- train %>%
  mutate(Class = factor(`Class/ASD`, levels = c(0, 1), labels = c("NoASD", "ASD")))

model_data <- train %>%
  select(Class, correct_result, age = age_imputed, gender, ethnicity = ethnicity_imputed, relation = relation_imputed,
         jaundice, austim, used_app_before, contry_of_res) %>%
  mutate(across(where(is.character), as.factor))

set.seed(72)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")

cv_model <- train(Class ~ ., 
                  data = model_data,
                  method = "glm",
                  family = "binomial",
                  trControl = ctrl,
                  metric = "ROC")

print(cv_model)
summary(cv_model$finalModel)
library(car)
vif(cv_model$finalModel)




# CV AGAIN

library(tidyverse)
library(forcats)
library(caret)

model_data_clean <- model_data %>%
  select(-relation) %>%
  mutate(
    ethnicity = fct_lump(ethnicity, n = 5),
    contry_of_res = fct_lump(contry_of_res, n = 10) 
  )

set.seed(72)

model_data_clean$Class <- factor(model_data_clean$Class, labels = c("NoASD", "ASD"))

ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")

cv_model_clean <- train(Class ~ ., 
                        data = model_data_clean,
                        method = "glm",
                        family = "binomial",
                        trControl = ctrl,
                        metric = "ROC")

print(cv_model_clean)
summary(cv_model_clean$finalModel)
vif(cv_model_clean$finalModel)


# Visuals

library(pROC)

roc_obj <- roc(cv_model_clean$pred$obs, cv_model_clean$pred$ASD)

plot(roc_obj, col = "darkred", lwd = 2,
     main = "Cross-Validated ROC Curve (Logistic Regression)")
abline(a = 0, b = 1, lty = 2, col = "gray")
text(0.6, 0.2, paste0("AUC = ", round(auc(roc_obj), 3)))

varImp(cv_model_clean) %>% 
  plot(main = "Variable Importance (Logistic Regression)")


# LASSO
library(caret)
library(glmnet)

model_data_clean$Class <- as.factor(model_data_clean$Class)

set.seed(72)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

lasso_model <- train(
  Class ~ ., 
  data = model_data_clean,
  method = "glmnet",
  trControl = ctrl,
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(
    alpha = 1,
    lambda = seq(0.0001, 0.1, length = 50)
  )
)

print(lasso_model)
plot(lasso_model)

best_lambda <- lasso_model$bestTune$lambda
coef(lasso_model$finalModel, s = best_lambda)


# Confusion Matrix for LASSO
train_pred_prob <- predict(lasso_model, newdata = model_data_clean, type = "prob")[, "ASD"]
train_pred_class <- ifelse(train_pred_prob > 0.5, "ASD", "NoASD")
train_pred_class <- factor(train_pred_class, levels = levels(model_data_clean$Class))
confusionMatrix(train_pred_class, model_data_clean$Class, positive = "ASD")

# Trying a New Threshold
train_pred_class_0.4 <- ifelse(train_pred_prob > 0.4, "ASD", "NoASD")
train_pred_class_0.4 <- factor(train_pred_class_0.4, levels = levels(model_data_clean$Class))
confusionMatrix(train_pred_class_0.4, model_data_clean$Class, positive = "ASD")

# Comparing LASSO to Log Reg
logreg_prob <- predict(cv_model_clean, newdata = model_data_clean, type = "prob")[, "ASD"]
logreg_pred_class <- ifelse(logreg_prob > 0.5, "ASD", "NoASD")
logreg_pred_class <- factor(logreg_pred_class, levels = levels(model_data_clean$Class))
confusionMatrix(logreg_pred_class, model_data_clean$Class, positive = "ASD")
