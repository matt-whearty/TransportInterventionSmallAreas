# Load necessary libraries
library(dplyr)
library(broom)
library(MASS) # for ordinal logistic regression

# Create a sample dataset
# Replace this with your actual dataset
# Y is the outcome variable, T is the binary time variable, G is the binary treatment variable
data <- data.frame(
  Y = c(10, 15, 12, 13, 18, 25, 14, 19, 10, 20, 22, 24),
  T = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
  G = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1)
)

# Fit the Difference-in-Difference model
did_model <- lm(Y ~ T + G + T * G, data = data)

# Summarize the model results
summary(did_model)

# Get tidy results
tidy(did_model)

# Read in the datasets
ForestHill = read.csv("jupyter/DiDregression_ForestHill.csv")
AbbeyRoad = read.csv("jupyter/DiDregression_AbbeyRoad_NEW.csv")
ImperialWharf = read.csv("jupyter/DiDregression_ImperialWharf.csv")

# Fit the Difference-in-Difference model
FH_did_model = lm(IMD_rank ~ Treatment + Period + Treatment * Period, data = ForestHill)
AR_did_model = lm(IMD_rank ~ Treatment + Period + Treatment * Period, data = AbbeyRoad)
IW_did_model = lm(IMD_rank ~ Treatment + Period + Treatment * Period, data = ImperialWharf)

# Summarize the model results
summary(FH_did_model)
summary(AR_did_model)
summary(IW_did_model)

# Get tidy results
tidy(FH_did_model)
tidy(AR_did_model)
tidy(IW_did_model)

# Linearity (Residuals vs Fitted)
plot(fitted(FH_did_model), residuals(FH_did_model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# Normality (Histogram and Q-Q Plot)
hist(residuals(FH_did_model),
     main = "Histogram of Residuals",
     xlab = "Residuals")
qqnorm(residuals(FH_did_model))
qqline(residuals(FH_did_model), col = "red")


# Load necessary libraries
library(lmtest) # For statistical tests
library(car)    # For VIF calculations

# Test for Linearity (Rainbow Test)
cat("Rainbow Test for Linearity:\n")
rainbow_test <- raintest(FH_did_model)
cat("  p-value:", rainbow_test$p.value, "\n")
if (rainbow_test$p.value > 0.05) {
  cat("  Interpretation: The test for linearity is passed; the relationship between predictors and the outcome is linear.\n\n")
} else {
  cat("  Interpretation: The test for linearity is failed; the relationship between predictors and the outcome may not be linear.\n\n")
}

# Test for Homoscedasticity (Breusch-Pagan Test)
bp_test <- bptest(FH_did_model)
cat("Breusch-Pagan Test for Homoscedasticity:\n")
cat("  p-value:", bp_test$p.value, "\n")
if (bp_test$p.value > 0.05) {
  cat("  Interpretation: The test for homoscedasticity is passed; residuals have constant variance.\n\n")
} else {
  cat("  Interpretation: The test for homoscedasticity is failed; residuals do not have constant variance.\n\n")
}

# Test for Normality of Residuals (Shapiro-Wilk Test)
shapiro_test <- shapiro.test(residuals(FH_did_model))
cat("Shapiro-Wilk Test for Normality:\n")
cat("  p-value:", shapiro_test$p.value, "\n")
if (shapiro_test$p.value > 0.05) {
  cat("  Interpretation: The data is normally distributed; residuals pass the normality test.\n\n")
} else {
  cat("  Interpretation: The data is not normally distributed; residuals fail the normality test.\n\n")
}

# Test for Multicollinearity (Variance Inflation Factor - VIF)
cat("Variance Inflation Factor (VIF) for Predictors:\n")
vif_values <- vif(FH_did_model)
print(vif_values) # Print the VIF values for all predictors
if (max(vif_values) < 5) {
  cat("  Interpretation: The test for multicollinearity is passed; VIF values are within acceptable limits.\n")
} else {
  cat("  Interpretation: The test for multicollinearity is failed; VIF values exceed acceptable limits.\n")
}

# Create a data frame with model outputs
did_model_results <- data.frame(
  Intervention = c("FH_did_model", "AR_did_model", "IW_did_model"),
  Adj_R_squared = c(summary(FH_did_model)$adj.r.squared, summary(AR_did_model)$adj.r.squared, summary(IW_did_model)$adj.r.squared),
  F_statistic = c(summary(FH_did_model)$fstatistic[1], summary(AR_did_model)$fstatistic[1], summary(IW_did_model)$fstatistic[1]),
  Prob_F_statistic = c(summary(FH_did_model)$fstatistic[3], summary(AR_did_model)$fstatistic[3], summary(IW_did_model)$fstatistic[3]),
  T_i_Estimate = c(coef(summary(FH_did_model))["Period", "Estimate"], coef(summary(AR_did_model))["Period", "Estimate"], coef(summary(IW_did_model))["Period", "Estimate"]),
  G_i_Estimate = c(coef(summary(FH_did_model))["Treatment", "Estimate"], coef(summary(AR_did_model))["Treatment", "Estimate"], coef(summary(IW_did_model))["Treatment", "Estimate"]),
  T_i_G_i_Estimate = c(coef(summary(FH_did_model))["Treatment:Period", "Estimate"], coef(summary(AR_did_model))["Treatment:Period", "Estimate"], coef(summary(IW_did_model))["Treatment:Period", "Estimate"]),
  T_i_P_Value = c(coef(summary(FH_did_model))["Period", "Pr(>|t|)"], coef(summary(AR_did_model))["Period", "Pr(>|t|)"], coef(summary(IW_did_model))["Period", "Pr(>|t|)"]),
  G_i_P_Value = c(coef(summary(FH_did_model))["Treatment", "Pr(>|t|)"], coef(summary(AR_did_model))["Treatment", "Pr(>|t|)"], coef(summary(IW_did_model))["Treatment", "Pr(>|t|)"]),
  T_i_G_i_P_Value = c(coef(summary(FH_did_model))["Treatment:Period", "Pr(>|t|)"], coef(summary(AR_did_model))["Treatment:Period", "Pr(>|t|)"], coef(summary(IW_did_model))["Treatment:Period", "Pr(>|t|)"])
)

# Print the results in table format
print(did_model_results)


#-------------- STATISTICAL SIGNIFICANCE OF SIMPLE DID

group_means <- read_csv("jupyter/DiD_GroupMeans.csv", col_types = cols(Pre_treat = col_number(), Post_treat = col_number()))

# View the structure of the dataset
str(group_means)

# Compute the IMD changes for each group
group_means$IMD_change <- group_means$Post_treat - group_means$Pre_treat

# Compute IMD changes for each neighbourhood
group_means$IMD_change <- group_means$Post_treat - group_means$Pre_treat

# Forest Hill vs. Cricklewood
FH_change <- group_means$IMD_change[group_means$Neighbourhood == "Forest Hill"]
CR_change <- group_means$IMD_change[group_means$Neighbourhood == "Cricklewood"]
DiD_FH <- FH_change - CR_change
print(paste("DiD Estimate for Forest Hill:", DiD_FH))

# Abbey Road vs. Wembley Central
AR_change <- group_means$IMD_change[group_means$Neighbourhood == "Abbey Road"]
WC_change <- group_means$IMD_change[group_means$Neighbourhood == "Wembley Central"]
DiD_AR <- AR_change - WC_change
print(paste("DiD Estimate for Abbey Road:", DiD_AR))

# Imperial Wharf vs. Harlesden
IW_change <- group_means$IMD_change[group_means$Neighbourhood == "Imperial Wharf"]
HA_change <- group_means$IMD_change[group_means$Neighbourhood == "Harlesden"]
DiD_IW <- IW_change - HA_change
print(paste("DiD Estimate for Imperial Wharf:", DiD_IW))

# Compute the DiD estimate
DiD_estimate <- FH_change - C_change
print(DiD_estimate)

# ---------------- LOAD IN ORIGINAL DATA

BaseData <- read_csv("jupyter/BaseData.csv", 
                     col_types = cols(`2004_POP` = col_number(), 
                                      `2004_SCORE` = col_number(), `2004_RANK` = col_number(), 
                                      `2007_POP` = col_number(), `2007_SCORE` = col_number(), 
                                      `2007_RANK` = col_number(), `2010_POP` = col_number(), 
                                      `2010_IMD` = col_number(), `2010_RANK` = col_number(), 
                                      `2015_POP` = col_number(), `2015_SCORE` = col_number(), 
                                      `2015_RANK` = col_number(), `2019_POP` = col_number(), 
                                      `2019_SCORE` = col_number(), `2019_RANK` = col_number(), 
                                      TT_2007_5000emplPT40 = col_number(), 
                                      TT_2010_5000emplPT40 = col_number(),
                                      TT_2015_5000EmpPT45n = col_number(), 
                                      TT_2019_5000EmpPT45n = col_number(), 
                                      TT_2019_2007PT = col_number(), `2019_2007IMDS` = col_number()))

# View the structure of the dataset
str(BaseData)


# Compute population-weighted average ranks for each neighborhood (pre- and post-treatment)
weighted_ranks <- BaseData %>%
  group_by(Neighbourhood) %>%
  summarise(
    Weighted_Avg_Pre = sum(`2007_RANK` * `2007_POP`) / sum(`2007_POP`),
    Weighted_Avg_Post = sum(`2019_RANK` * `2019_POP`) / sum(`2019_POP`)
  )

print(weighted_ranks)

# Compute IMD changes for each neighborhood
weighted_ranks <- weighted_ranks %>%
  mutate(IMD_Change = Weighted_Avg_Post - Weighted_Avg_Pre)

# Define treatment and control neighborhood pairs
case_studies <- tibble(
  Treatment = c("Forest Hill", "Abbey Road", "Imperial Wharf"),
  Control = c("Cricklewood", "Wembley Central", "Harlesden")
)

# Merge treatment group values
DiD_comparisons <- case_studies %>%
  left_join(weighted_ranks, by = c("Treatment" = "Neighbourhood")) %>%
  rename(Treatment_Weighted_Avg_Pre = Weighted_Avg_Pre,
         Treatment_Weighted_Avg_Post = Weighted_Avg_Post,
         Treatment_Change = IMD_Change) %>%
  
  # Merge control group values
  left_join(weighted_ranks, by = c("Control" = "Neighbourhood")) %>%
  rename(Control_Weighted_Avg_Pre = Weighted_Avg_Pre,
         Control_Weighted_Avg_Post = Weighted_Avg_Post,
         Control_Change = IMD_Change) %>%
  
  # Compute Difference-in-Differences (DiD) estimates
  mutate(DiD = Treatment_Change - Control_Change)

print(DiD_comparisons)



# Compute standard deviation of IMD ranks within neighborhoods (pre- and post-treatment)
IMD_SD <- BaseData %>%
  group_by(Neighbourhood) %>%
  summarise(
    SD_Pre = sd(`2007_RANK`),
    SD_Post = sd(`2019_RANK`)
  )

print(IMD_SD)

# Count LSOAs per neighborhood
LSOA_counts <- BaseData %>%
  group_by(Neighbourhood) %>%
  summarise(Num_LSOAs = n())

print(LSOA_counts)

# Merge standard deviations and LSOA counts into the DiD comparisons dataset
DiD_comparisons <- DiD_comparisons %>%
  left_join(IMD_SD, by = c("Treatment" = "Neighbourhood")) %>%
  rename(SD_Pre_Treatment = SD_Pre, SD_Post_Treatment = SD_Post) %>%
  
  left_join(IMD_SD, by = c("Control" = "Neighbourhood")) %>%
  rename(SD_Pre_Control = SD_Pre, SD_Post_Control = SD_Post) %>%
  
  left_join(LSOA_counts, by = c("Treatment" = "Neighbourhood")) %>%
  rename(N_Treatment = Num_LSOAs) %>%
  
  left_join(LSOA_counts, by = c("Control" = "Neighbourhood")) %>%
  rename(N_Control = Num_LSOAs)

# Compute Z-scores and p-values
DiD_comparisons <- DiD_comparisons %>%
  mutate(
    SE_Treatment = SD_Post_Treatment / sqrt(N_Treatment),
    SE_Control = SD_Post_Control / sqrt(N_Control),
    SE_DiD = sqrt(SE_Treatment^2 + SE_Control^2),  # Standard error for DiD
    Z_value = DiD / SE_DiD,  # Compute Z-score
    p_value = 2 * (1 - pnorm(abs(Z_value)))  # Compute two-tailed p-value
  )

print(DiD_comparisons[, c("Treatment", "Control", "DiD", "Z_value", "p_value")])
