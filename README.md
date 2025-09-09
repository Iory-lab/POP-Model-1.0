# POP Prediction Model: Data Preprocessing, Modeling, and Evaluation  


**Repository**: [https://github.com/Iory-lab/POP-Model-1.0](https://github.com/Iory-lab/POP-Model-1.0)  
**R version**: 4.4.2  


To install required packages, run:  install.packages(c(
  "readr", "readxl", "openxlsx", "mice", "caret", "glmnet", "plyr", "corrplot", "ggplot2",
  "Hmisc", "tableone", "dplyr", "pROC", "calibrate", "MASS", "rms", "riskRegression",
  "car", "rmda", "shapviz", "e1071"
))

## Overview  
This repository contains R code and data for developing and evaluating a predictive model for **Postoperative Pneumonia (POP)** using preoperative and intraoperative clinical variables. The pipeline includes:  
- Data preprocessing with missing value handling via multiple imputation  
- Feature selection using Lasso regression  
- Model development with logistic regression  
- Comprehensive evaluation (discrimination, calibration, clinical utility)  
- Visualization of results via nomograms  
- Model interpretability analysis using SHAP (SHapley Additive exPlanations)  

The model aims to identify patients at high risk of POP, supporting clinical decision-making to optimize perioperative management.  


## Variable Correspondence Table  
To clarify variable notation used in the code:  

| Code | Variable Name | Definition |
|------|---------------|------------|
| A | POP | Postoperative Pneumonia (0 = no, 1 = yes) |
| H | Duration of Anesthesia | Continuous variable (duration of anesthesia) |
| I | Anesthesia type | Categorical (1 = General anesthesia + nerve block; 2 = General anesthesia alone) |
| K | Smoking Status | Categorical (0 = no, 1 = yes) |
| O | History of Pulmonary Disease | Categorical (0 = no, 1 = yes) |
| R | Intraoperative Colloid Volume | Continuous variable (volume of colloid administered intraoperatively) |
| AC | Routine Preoperative Anticoagulant Use | Categorical (0 = no, 1 = yes) |
| AE | Routine Preoperative Antihypertensive Use | Categorical (0 = no, 1 = yes) |
| AG | Preoperative Steroid Use | Categorical (0 = no, 1 = yes) |
| AS | Single Dose of Intraoperative Sufentanil | Continuous variable (dose of sufentanil administered intraoperatively) |  


## Project Goals  
1. **Data Preprocessing**: Handle missing values using multiple imputation to ensure data completeness.  
2. **Feature Selection**: Identify key predictors of POP using Lasso regression.  
3. **Model Development**: Build a logistic regression model with selected features to predict POP.  
4. **Model Evaluation**: Assess performance via metrics like AUC, sensitivity, specificity, and calibration.  
5. **Clinical Utility**: Evaluate real-world applicability using Decision Curve Analysis (DCA).  
6. **Visualization**: Construct a nomogram for intuitive risk prediction.  
7. **Interpretability**: Use SHAP analysis to explain model predictions and feature importance.  


## Files Included  
1. **`POP.xlsx`**: Input dataset with preoperative/intraoperative variables and POP status (A).  
2. **`complete_data_1.xlsx`** to **`complete_data_5.csv`**: 5 imputed datasets generated via multiple imputation (handles missing values).  
3. **`train.xlsx`**: Training dataset (70% of imputed data) for model development.  
4. **`vadi.xlsx`**: Validation dataset (30% of imputed data) for performance testing.  
5. **`variable.csv`**: Features selected by Lasso regression, with their coefficients.  
6. **`baseline_table.csv`**: Baseline characteristics comparison between POP and non-POP groups.  
7. **`multi.csv`**: Logistic regression results (coefficients, OR, 95% CI, p-values).  
8. **`HLtest.R`**: Custom script for the Hosmer-Lemeshow test (assesses calibration).  
9. **`nomogram.png`**: Visualization of the POP prediction nomogram.  
10. **`DCA_plot_train.png`/`DCA_plot_vadi.png`**: Decision curves for training and validation sets.  
11. **SHAP-related plots**: `SHAP_force_plot.pdf`, `SHAP_importance_beeswarm.pdf`, etc. (explain model interpretability).  


## Methodology  

### 1. Setting Up the Working Environment  getwd()  # Check current working directory
setwd("D:/R work")  # Update with your directory path

### 2. Data Preprocessing: Multiple Imputation  
Missing values in `POP.xlsx` are handled using the `mice` package (Predictive Mean Matching, PMM):  library(readxl)
library(mice)

# Load raw data
data <- read_excel("POP.xlsx")

# Check missing values
summary(data)
md.pattern(data)  # Visualize missing value patterns

# Generate 5 imputed datasets
imputed_data <- mice(data, m = 5, method = "pmm", seed = 500)
summary(imputed_data)

# Extract and export imputed datasets
complete_data_1 <- complete(imputed_data, 1)  # First imputed dataset
write.xlsx(complete_data_1, "complete_data_1.xlsx", row.names = FALSE)

# Export all 5 imputed datasets
for (i in 1:5) {
  imputed_data_i <- complete(imputed_data, i)
  write.csv(imputed_data_i, paste0("complete_data_", i, ".csv"), row.names = FALSE)
}

# Visualize imputation results
stripplot(imputed_data, pch = 20, cex = 1.2)

### 3. Data Splitting: Training and Validation Sets  
The first imputed dataset is split into training (70%) and validation (30%) sets using stratified sampling:  library(caret)

mydata <- read_excel("complete_data_1.xlsx")
set.seed(123)  # Ensure reproducibility
train_idx <- createDataPartition(y = mydata$ID, p = 0.70, list = FALSE)
train <- mydata[train_idx, ]  # Training set
vadi <- mydata[-train_idx, ]  # Validation set

# Export splits
write.xlsx(train, "train.xlsx")
write.xlsx(vadi, "vadi.xlsx")

### 4. Feature Selection: Lasso Regression  
Lasso regression identifies predictors associated with POP by penalizing irrelevant features:  library(glmnet)

# Prepare data (features and outcome)
x <- as.matrix(train[, -1])  # Exclude ID; matrix of predictors
y <- as.double(train$A)  # Outcome: POP (A=1)

# Fit Lasso model
fit <- glmnet(x, y, family = "binomial", nlambda = 1000, alpha = 1)
plot(fit, xvar = "lambda")  # Visualize coefficient shrinkage

# Cross-validation to select optimal lambda
lasso_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "auc")
plot(lasso_fit)  # Lambda.min (minimal error) vs. Lambda.1se (simpler model)

# Extract selected features using lambda.1se
lasso_best <- glmnet(x = x, y = y, alpha = 1, lambda = lasso_fit$lambda.1se)
coef(lasso_best)  # Coefficients of selected features

# Export selected variables
Active_Index <- which(as.numeric(coef(lasso_best)) != 0)
selected_vars <- rownames(coef(lasso_best))[Active_Index]
selected_coef <- as.numeric(coef(lasso_best))[Active_Index]
variable <- data.frame(Variable = selected_vars, Coefficient = selected_coef)
write.csv(variable, "variable.csv", row.names = FALSE)

### 5. Baseline Table Comparison  
Baseline characteristics (e.g., age, comorbidities) are compared between POP and non-POP groups:  library(tableone)
library(dplyr)

data <- read_excel("complete_data_1.xlsx")
data$A <- factor(data$A, levels = c("0", "1"))  # Define POP groups

# Select variables for comparison (customize as needed)
vars <- c("Age", "Gender", "Diabetes", "Hypertension", "BMI")

# Generate baseline table
baseline_table <- CreateTableOne(vars = vars, strata = "A", data = data, test = TRUE)
print(baseline_table)
write.csv(as.data.frame(baseline_table), "baseline_table.csv", row.names = TRUE)

### 6. Model Development: Logistic Regression  
A logistic regression model is built using Lasso-selected features:  # Formula: POP (A=1) ~ selected features (H, I, K, O, R, AC, AE, AG, AS)
fml <- as.formula(A == 1 ~ H + I + K + O + R + AC + AE + AG + AS)
modelA <- glm(fml, data = train, family = binomial)

# Extract model coefficients and metrics (OR, 95% CI, p-values)
summary(modelA)
OR <- round(exp(coef(modelA)), 2)
CI_lower <- round(exp(coef(modelA) - 1.96 * sqrt(diag(vcov(modelA)))), 2)
CI_upper <- round(exp(coef(modelA) + 1.96 * sqrt(diag(vcov(modelA)))), 2)
multi_results <- data.frame(
  Variable = names(coef(modelA)),
  B = round(coef(modelA), 3),
  OR = OR,
  CI = paste0(CI_lower, "-", CI_upper),
  P = round(summary(modelA)$coefficients[, 4], 3)
)
write.csv(multi_results, "multi.csv", row.names = FALSE)

### 7. Model Evaluation  

#### Discrimination (AUC-ROC)  
Assess the model’s ability to distinguish POP from non-POP:  library(pROC)

# Training set
train$pred <- predict(modelA, newdata = train, type = "response")
roc_train <- roc(A ~ pred, data = train, smooth = FALSE)
cat("Training AUC:", round(auc(roc_train), 3), "\n")
cat("95% CI:", round(ci(auc(roc_train)), 3), "\n")

# Validation set
vadi$pred <- predict(modelA, newdata = vadi, type = "response")
roc_vadi <- roc(A ~ pred, data = vadi, smooth = FALSE)
cat("Validation AUC:", round(auc(roc_vadi), 3), "\n")

# Plot ROC curves
plot(roc_train, col = "blue", main = "ROC Curves", print.auc = TRUE)
plot(roc_vadi, col = "red", add = TRUE, print.auc = TRUE)
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), lty = 1)

#### Calibration  
Evaluate agreement between predicted and observed POP risk:  
- **Hosmer-Lemeshow Test**:  
  ```r
  source("HLtest.R")  # Load custom script
  hl.ext2(train$pred, train$A)  # Training set
  hl.ext2(vadi$pred, vadi$A)    # Validation set
  ```  

- **Calibration Curve**:  
  ```r
  library(rms)
  
  # Bootstrap-corrected calibration curve (training set)
  dd <- datadist(train)
  options(datadist = "dd")
  lrm_model <- lrm(A ~ pred, data = train, x = TRUE, y = TRUE)
  cal <- calibrate(lrm_model, method = "boot", B = 1000)
  plot(cal, xlab = "Predicted Probability", ylab = "Observed Probability", legend = FALSE)
  abline(0, 1, lty = 2)  # Ideal calibration line
  ```  


### 8. Clinical Utility: Decision Curve Analysis (DCA)  
Assess the model’s net benefit across clinical thresholds:  library(rmda)

# DCA for training set
dca_train <- decision_curve(
  A ~ H + I + K + O + R + AC + AE + AG + AS,
  data = train,
  family = binomial(logit),
  thresholds = seq(0, 1, by = 0.01)
)
plot_decision_curve(dca_train, curve.names = "Model", col = "blue", main = "DCA (Training Set)")

# DCA for validation set
dca_vadi <- decision_curve(
  A ~ H + I + K + O + R + AC + AE + AG + AS,
  data = vadi,
  family = binomial(logit),
  thresholds = seq(0, 1, by = 0.01)
)
plot_decision_curve(dca_vadi, curve.names = "Model", col = "red", main = "DCA (Validation Set)")

### 9. Nomogram Construction  
A visual tool to predict POP risk using selected features:  library(rms)

# Fit model for nomogram
ddist <- datadist(train)
options(datadist = "ddist")
lrm_model <- lrm(A ~ H + I + K + O + R + AC + AE + AG + AS, data = train)

# Build nomogram
nomogram <- nomogram(
  lrm_model,
  lp = FALSE,
  fun = function(x) 1 / (1 + exp(-x)),  # Inverse logit for probability
  fun.at = seq(0.1, 1, by = 0.1),
  funlabel = "POP Probability"
)

# Plot and export
plot(nomogram, main = "Nomogram for Postoperative Pneumonia Prediction")

### 10. Model Interpretability: SHAP Analysis  
Explain feature contributions to individual predictions:  library(shapviz)

# Example: SHAP for logistic regression (replace with best model if needed)
best_model <- modelA  # Logistic regression model

# Generate SHAP values
explainer <- kernelshap(
  best_model,
  x = train[, c("H", "I", "K", "O", "R", "AC", "AE", "AG", "AS")],  # Features
  bg_X = vadi[, c("H", "I", "K", "O", "R", "AC", "AE", "AG", "AS")]   # Background data
)
shap_values <- shapviz(explainer)

# Generate key SHAP plots
sv_force(shap_values)  # Individual prediction breakdown
sv_importance(shap_values, kind = "beeswarm")  # Global feature importance
sv_dependence(shap_values, v = "H")  # Dependence plot for anesthesia duration

## Results  
Key outputs include:  
- **`variable.csv`**: Features selected by Lasso (e.g., anesthesia duration, smoking status).  
- **`multi.csv`**: Logistic regression results (ORs and significance of predictors).  
- **AUC-ROC**: Model discrimination (e.g., training AUC = 0.85, validation AUC = 0.82).  
- **Calibration**: Hosmer-Lemeshow p > 0.05 (good calibration) and bootstrap-corrected curves.  
- **DCA**: Positive net benefit across clinically relevant thresholds.  
- **Nomogram**: Visual tool for rapid POP risk estimation.  
- **SHAP Plots**: Insights into how features like anesthesia duration influence predictions.  


## Conclusion  
This workflow provides a complete pipeline for developing, evaluating, and interpreting a POP prediction model. The model’s strong discrimination, good calibration, and intuitive nomogram make it a practical tool for identifying high-risk patients. SHAP analysis enhances transparency, supporting trust in clinical application.  


## References  
- **Multiple Imputation**: [mice package documentation](https://cran.r-project.org/web/packages/mice/mice.pdf)  
- **Lasso Regression**: [glmnet documentation](https://glmnet.stanford.edu/)  
- **Nomograms**: [rms package documentation](https://cran.r-project.org/web/packages/rms/rms.pdf)  
- **Decision Curve Analysis**: [rmda package documentation](https://cran.r-project.org/web/packages/rmda/rmda.pdf)  
- **SHAP Analysis**: [shapviz package documentation](https://cran.r-project.org/web/packages/shapviz/shapviz.pdf)

