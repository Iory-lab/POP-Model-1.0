getwd()
setwd("D:/R work")

# Variable Correspondence Table
#A=POP
#H = Duration of Anesthesia
#I = Anesthesia type
#K = Smoking Status
#O = History of Pulmonary Disease
#R = Intraoperative Colloid Volume
#AC = Routine Preoperative Anticoagulant Use
#AE = Routine Preoperative Antihypertensive Use
#AG = Preoperative Steroid Use
#AS = Single Dose of Intraoperative Sufentanil


# Variable understanding
# POP                            0=no         1=yes
# Anesthesia type                1=General anesthesia combined with nerve block        2=General anesthesia
# Smoking Status                 0=no         1=yes
# History of Pulmonary Disease   0=no         1=yes
# Routine Preoperative Anticoagulant Use  0=no         1=yes
# Routine Preoperative Antihypertensive Use 0=no         1=yes  
# Preoperative Steroid Use       0=no         1=yes

# Perform multiple imputation

library(readr)
data <- read_excel("POP.xlsx")

# Install and load the mice package
install.packages("mice")
library(mice)

# Check missing values in the dataset
summary(data)
md.pattern(data)

# Use mice for multiple imputation
imputed_data <- mice(data, m = 5, method = 'pmm', seed = 500)
summary(imputed_data)

# Extract the first imputed dataset
complete_data_1 <- complete(imputed_data, 1)

# Export the first imputed dataset to a CSV file
write.xlsx(complete_data_1, "complete_data_1.xlsx", row.names = FALSE)

# Export all imputed datasets as separate CSV files
for (i in 1:5) {  # m=5 indicates 5 imputed datasets
  imputed_data_i <- complete(imputed_data, i)  # Get the i-th imputed dataset
  write.csv(imputed_data_i, paste0("complete_data_", i, ".csv"), row.names = FALSE)
}

# Visualize imputation results
stripplot(imputed_data, pch = 20, cex = 1.2)

# Split data into training and validation sets at 70:30 ratio

# Randomly split the file by proportion
library(readr)
mydata <- read_excel("complete_data_1.xlsx")
install.packages("caret")
library(caret)
set.seed(123)
trianandvad <- createDataPartition(y = mydata$ID, p = 0.70, list = FALSE)
train <- mydata[trianandvad, ]
vadi <- mydata[-trianandvad, ] 
write.xlsx(train, "train.xlsx")
write.xlsx(vadi, "vadi.xlsx")
# Read data
library(readr)
train <- read_excel("train.xlsx")
library(readr)
vadi <- read_excel("vadi.xlsx")

# LASSO regression for variable selection
library(glmnet)
library(readxl)
library(plyr)
library(caret)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(openxlsx)
x <- as.matrix(train[,-c(1)])# Remove the first row (dependent variable), x should have no missing values, otherwise use dtcom<-na.omit(dtcom) to remove missing values
y <- as.double(train$A)# Set dependent variable
fit <- glmnet(x, y, family = "binomial", nlambda = 1000, alpha = 1)
View(fit)
print(fit)
plot(fit, xvar = "lambda")# Create plot
lasso_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "auc", nlambda = 1000)# Build equation for variable selection, with two lines: lambda.min (left) and lambda.lse (right), choose based on actual situation.
plot(lasso_fit)
print(lasso_fit)
lasso_best <- glmnet(x = x, y = y, alpha = 1, lambda = lasso_fit$lambda.1se)# Choose lambda.min or lambda.lse based on the number of variables
coef(lasso_best)
coefficient <- coef(lasso_best, s = lasso_best$lambda.min)
coe <- coefficient@x
coe <- as.data.frame(coe)
Active_Index <- which(as.numeric(coefficient) != 0)
active_coefficients <- as.numeric(coefficient)[Active_Index]
variable <- rownames(coefficient)[Active_Index]
variable <- as.data.frame(variable)
variable <- cbind(variable, coe)
View(variable)# Display final selected variables and regression coefficients
write.csv(variable, "variable.csv", row.names = FALSE)

# Baseline table comparison
# Read data
library(readr)
data <- read_excel("complete_data_1")

# Install tableone and dplyr packages (if not installed)
install.packages("tableone")
install.packages("dplyr")

# Load packages
library(tableone)
library(dplyr)

# Ensure A=POP is a factor, grouped by dependent variable
data$A <- factor(data$A, levels = c("0", "1"))# Can also group by ID numbers of train and vadi

# Select baseline features for comparison
vars <- c("Age", "Gender", "Diabetes", "Hypertension", "BMI")# Variables can be modified

# Create baseline table, stratified by Group column
baseline_table <- CreateTableOne(vars = vars, strata = "A", data = data, test = TRUE)

# View the generated baseline table
print(baseline_table)

# Export baseline table to CSV file
write.csv(as.data.frame(baseline_table), "baseline_table.csv", row.names = TRUE)

# Build model with selected variables
fml <- as.formula(A == 1 ~H + I + K + O + R + AC + AE + AG + AS)
modelA <- glm(fml, data = train, family = binomial)

# Prepare regression equation parameters

glm3 <- summary(modelA)
glm3


# Prepare multivariate analysis results for publication format
glm3$coefficients

OR <- round(exp(glm3$coefficients[,1]), 2)
OR


SE <- round(glm3$coefficients[,2], 3)
CI2.5 <- round(exp(coef(modelD)-1.96*SE), 2)
CI97.5 <- round(exp(coef(modelD)+1.96*SE), 2)
CI <- paste0(CI2.5,'-',CI97.5)
B <- round(glm3$coefficients[,1], 3)
Z <- round(glm3$coefficients[,3], 3)
P <- round(glm3$coefficients[,4], 3)

# Create data frame
mlogit <- data.frame( 
  'B' = B,
  'SE' = SE,
  'OR' = OR,
  'CI' = CI,
  'Z' = Z,
  'P' = P)[-1,]   # Remove constant term
mlogit

# Remove the first row (constant term), if [-1,] is not used above, the constant term will be generated, leading to merging issues
# mlogit <- mlogit[-1,] 

View(mlogit)

mlogit

View(mlogit)

# Write multivariate analysis results to CSV, create regression coefficient table
write.csv(mlogit, "multi.csv")

# Discrimination (AUC) for training and validation sets

fml <- as.formula(A == 1 ~ H + I + K + O + R + AC + AE + AG + AS)

modelA <- glm(fml, data = train, family = binomial(logit))


# Calculate predicted values in modeling population
train$predmodelA <- predict(newdata = train, modelA, "response")
View(train)

# Calculate predicted values in validation population
vadi$predmodelA <- predict(newdata = vadi, modelA, "response")
View(vadi)

library(pROC)

# AUC and ROC analysis for training set, modelA
devmodelA <- roc(A ~ predmodelA, data = train, smooth = F)

devmodelA

round(auc(devmodelA), 3)
round(ci(auc(devmodelA)), 3)


# Draw ROC curve
plot(devmodelA, print.auc = TRUE, print.thres = TRUE, main = "ROC CURVE", 
     col = "blue", print.thres.col = "blue", identity.col = "blue",
     identity.lty = 1, identity.lwd = 1)


## AUC and ROC analysis for validation set, modelA
devmodelA <- roc(A ~ predmodelA, data = vadi, smooth = F)
round(auc(devmodelA), 3)
round(ci(auc(devmodelA)), 3)


# Draw ROC curve
plot(devmodelA, print.auc = TRUE, print.thres = TRUE, main = "ROC CURVE", 
     col = "red", print.thres.col = "red", identity.col = "red",
     identity.lty = 1, identity.lwd = 1)

# Hosmer-Lemeshow test
# install.packages("calibrate")
library(calibrate)
library(MASS)
# install.packages("rms")
library(rms)



# Perform Hosmer-Lemeshow test in modeling population
source("HLtest.R") # Ensure HLtest.R is placed in the previously set working directory.
hl.ext2(train$predmodelA, train$A)

# Perform Hosmer-Lemeshow test in validation population
source("HLtest.R") # Ensure HLtest.R is placed in the previously set working directory.
hl.ext2(vadi$predmodelA, vadi$A)


# Plot calibration curve

# Load necessary packages
library(riskRegression)
library(rms)

# Global settings: use common sans-serif font to avoid Unicode character support issues
par(family = "sans")

# 1. Data preprocessing (for train dataset)
required_vars <- c("A", "H", "I", "K", "O", "R", "AC", "AE", "AG", "AS")
train_clean <- na.omit(train[, required_vars])
train_clean$A <- as.factor(train_clean$A)

# 2. Fit logistic regression main model
formula <- A ~ H + I + K + O + R + AC + AE + AG + AS
fit1 <- glm(formula, data = train_clean, family = binomial())

# 3. Performance evaluation: ROC and Brier (optional to retain)
set.seed(123)
xb <- Score(
  list(fit = fit1),
  formula    = A ~ 1,
  data       = train_clean,
  null.model = FALSE,
  plots      = "ROC",
  metrics    = c("auc", "brier"),
  B          = 1000,
  cens.model = NULL
)

# 4. Calculate calibration intercept & slope
train_clean$lp <- predict(fit1, type = "link")
calib_model <- glm(as.numeric(A) - 1 ~ lp, data = train_clean, family = binomial())
coef_vals <- coef(calib_model)
ci_vals   <- confint(calib_model)

intercept <- coef_vals[1]
slope     <- coef_vals[2]
int_ci    <- ci_vals[1, ]
slope_ci  <- ci_vals[2, ]

# 5. Generate bootstrap‐corrected calibration curve using rms package
dd <- datadist(train_clean)
options(datadist = 'dd')
lrm_mod <- lrm(A ~ lp, data = train_clean, x = TRUE, y = TRUE)
cal <- calibrate(lrm_mod, method = 'boot', B = 1000)

# 6. Plot calibration curve and add annotations
plot(cal,
     xlim   = c(0, 1), ylim = c(0, 1),
     xlab   = 'Predicted probability',
     ylab   = 'Observed probability',
     legend = FALSE)
abline(0, 1, lty = 2)  # 45° ideal line

# Add intercept & slope annotations to the plot (use ordinary "-")
text(
  x      = 0.65, 
  y      = 0.15,
  labels = sprintf(
    "Intercept = %.3f (95%% CI %.3f-%.3f)\nSlope     = %.3f (95%% CI %.3f-%.3f)",
    intercept, int_ci[1], int_ci[2],
    slope,     slope_ci[1], slope_ci[2]
  ),
  adj = c(0, 0),
  cex = 0.8
)

# Add legend description (also use ordinary "-")
legend(
  "bottomright",
  legend = "Bootstrap-corrected (B=1000)",
  bty    = "n"
)
# Create correlation heatmap
# Install necessary packages (if not installed)
install.packages("corrplot")
install.packages("ggplot2")

# Load necessary packages
library(corrplot)
library(ggplot2)
library(car)

# Assume train is the training dataset
# Select predictor variables retained in the final model
variables <- train[, c("H", "I", "K", "O", "R", "AC", "AE", "AG", "AS")]

# Generate correlation matrix between predictor variables
cor_matrix <- cor(variables, use = "complete.obs")  # Adjust use parameter based on actual data

# Calculate VIF and tolerance values
# Assume A is the dependent variable
model <- lm(A ~ H + I + K + O + R + AC + AE + AG + AS, data = train)
vif_values <- car::vif(model)
tolerance_values <- 1 / vif_values

# Output VIF and tolerance values
print(vif_values)
print(tolerance_values)

# Generate correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addrect = 2, 
         cl.ratio = 0.15, cl.align.c = "center", 
         diag = FALSE, title = "Correlation Matrix of Predictors",
         tl.cex = 1.2, cl.cex = 1.2, rect.col = "darkgrey")
# Create DCA (Decision Curve Analysis)
library(rmda)

### Plot DCA curve

model_1 <- decision_curve(A ~ H + I + K + O + R + AC + AE + AG + AS,
                          data = train,
                          family = binomial(logit),
                          thresholds = seq(0, 1, by = 0.01),
                          confidence.intervals = 0.95,
                          study.design = 'case-control',
                          population.prevalence = 0.3)
model_2 <- decision_curve(A ~ H + I + K + O + R + AC + AE + AG + AS,
                          data = vadi,
                          family = binomial(logit),
                          thresholds = seq(0, 1, by = 0.01),
                          confidence.intervals = 0.95,
                          study.design = 'case-control',
                          population.prevalence = 0.3)
# Plot the curve
plot_decision_curve(model_1, curve.names = c('Model'),
                    xlim = c(0, 0.8),
                    cost.benefit.axis = FALSE,
                    col = c('blue'),
                    confidence.intervals = FALSE,
                    standardize = FALSE)


plot_decision_curve(model_2, curve.names = c('Model'),
                    xlim = c(0, 0.8),
                    cost.benefit.axis = FALSE,
                    col = c('red'),
                    confidence.intervals = FALSE,    # TRUE to show confidence intervals
                    standardize = FALSE)

#Standard Nomogram Construction
# Load required packages
library(readr)
library(readxl)
library(rms)

# Read training data
mydata <- read_excel("train.xlsx")

# Remove rows with missing values
train <- na.omit(mydata)

# Set up datadist object for use with the rms package
ddist <- datadist(train)
options(datadist = "ddist")

# Fit logistic regression model using the lrm() function from rms
# The response variable is A; predictors include H, I, K, O, R, AC, AE, AG, AS
modelA <- lrm(A ~ H + I + K + O + R + AC + AE + AG + AS, data = train)

# Construct nomogram based on the logistic regression model
# The fun argument applies the inverse logit function to map linear predictors to predicted probabilities
# fun.at sets the tick marks for the probability scale
# lp = FALSE suppresses the linear predictor axis

nomomodelA <- nomogram(modelA,
                       lp = FALSE, 
                       fun = function(x) 1 / (1 + exp(-x)),
                       fun.at = seq(0.1, 1, by = 0.1),
                       funlabel = "Diagnostic Probability")

# Plot the nomogram
plot(nomomodelA)

# SHAP Interpretation of Best Performing Model 

# Assign models to a named list
ML_class_model$LightGBM <- lightgbm_model
ML_class_model$CatBoost <- Catboost_model

# Number of samples used for SHAP explanation
n_train <- 100
n_test <- 100

# Define the best model (currently CatBoost not supported by kernelSHAP)
best_model <- "Logistic"  # Replace with the actual best model name if needed

# Create SHAP explainer using kernel SHAP
explain_kernel <- kernelshap(
  ML_class_model[[best_model]],
  dev[1:n_train, -1],
  bg_X = vad[1:n_test, -1]
)

# Generate SHAP values with interaction effects
shap_values <- shapviz(
  explain_kernel,
  X_pred = dev[1:n_train, -1],
  interactions = TRUE
)

############## 1. Force Plot for Individual Prediction ##############
pdf(paste0("SHAP_", best_model, "_force_plot.pdf"), width = 7, height = 5)
sv_force(shap_values$Yes, 
         row_id = 12,
         size = 9) +
  ggtitle(paste0(best_model)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))
dev.off()

############## 2. Beeswarm Plot for Global Feature Importance ##############
pdf(paste0("SHAP_", best_model, "_importance_beeswarm.pdf"), width = 7, height = 5)
sv_importance(shap_values$Yes, 
              kind = "beeswarm", 
              viridis_args = list(begin = 0.25, end = 0.85, option = "B"),
              show_numbers = FALSE) +
  ggtitle(paste0(best_model)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))
dev.off()

############## 3. Bar Plot for Feature Importance ##############
pdf(paste0("SHAP_", best_model, "_importance_bar.pdf"), width = 7, height = 5)
sv_importance(shap_values$Yes, 
              kind = "bar", 
              show_numbers = FALSE,
              fill = "#fca50a",
              class = "Yes") +
  theme_bw() +
  ggtitle(paste0(best_model)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))
dev.off()

############## 4. Dependence Plot for Selected Feature ##############
# Change "Age" and "Size" to variables of interest in your dataset
pdf(paste0("SHAP_", best_model, "_dependence_plot.pdf"), width = 5, height = 5)
sv_dependence(shap_values$Yes,
              v = "H",           # Feature of interest on x-axis
              color = "#3b528b",   # Color tone
              color_var = "H"   # Optional second variable
) +
  theme_bw() +
  ggtitle(paste0(best_model)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))
dev.off()

############## 5. Waterfall Plot for a Single Prediction ##############
pdf(paste0("SHAP_", best_model, "_waterfall_plot.pdf"), width = 5, height = 5)
sv_waterfall(shap_values$Yes,
             row_id = 12,
             fill_colors = c("#f7d13d", "#a52c60")) +
  theme_bw() +
  ggtitle(paste0(best_model)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))
dev.off()

############## 6. SHAP Interaction Beeswarm Plot (Optional) ##############
sv_interaction(
  shap_values$Yes,
  kind = "beeswarm",
  max_display = 5,
  alpha = 0.3,
  bee_width = 0.3,
  bee_adjust = 0.5,
  viridis_args = getOption("shapviz.viridis_args"),
  color_bar_title = "Feature Value"
)




