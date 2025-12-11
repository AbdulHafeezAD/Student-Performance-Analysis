# =========================================
# (Hafeez) - Step 1: Load Clean Dataset & Initial Preview
# =========================================

# Load cleaned dataset created by Pooji
student_data <- read.csv("Final_Clean_Student_Data.csv", stringsAsFactors = FALSE)

# View structure of dataset
str(student_data)

# View first 6 rows
head(student_data)

# Quick counts
cat("Total rows:", nrow(student_data), " Total columns:", ncol(student_data), "\n")



# =========================================
# (Hafeez) - Step 2: Exploratory correlations & pairwise checks
# =========================================

# Ensure variables we will analyse are numeric (convert factors if necessary)
analysis_vars <- c("GPA",
                   "Study_Hours",
                   "Attendance.to.classes",
                   "Taking.notes.in.classes",
                   "Listening.in.classes",
                   "Preparation.to.midterm.exams.1",
                   "Final_Grade")

for (v in analysis_vars) {
  if (v %in% names(student_data)) {
    if (is.factor(student_data[[v]])) {
      student_data[[v]] <- as.numeric(as.character(student_data[[v]]))
    }
  }
}

# Select numeric subset (complete cases for correlation)
num_subset <- na.omit(student_data[ , intersect(analysis_vars, names(student_data))])

# Correlation matrix (pairwise.complete.obs if you prefer)
cor_mat <- round(cor(num_subset, use = "pairwise.complete.obs"), 3)

# Save correlation matrix for report
if (!dir.exists("report")) dir.create("report")
if (!dir.exists("report/tables")) dir.create("report/tables")
write.csv(as.data.frame(cor_mat), file = "report/tables/Hafeez_correlation_matrix.csv", row.names = TRUE)

# Print correlation matrix to console
print(cor_mat)

# Pairwise scatterplot matrix saved to file
if (!dir.exists("report/figures")) dir.create("report/figures")
png(filename = "report/figures/Hafeez_pairwise_scatter.png", width = 900, height = 900)
pairs(num_subset, main = "Pairwise scatter: predictors + GPA")
dev.off()


# =========================================
# (Hafeez) - Step 3: Multiple regression (GPA ~ predictors) & diagnostics
# =========================================

# Define formula (same predictors as Zaheer used)
formula_model <- GPA ~ Study_Hours + Attendance.to.classes + Taking.notes.in.classes +
  Listening.in.classes + Preparation.to.midterm.exams.1

# Fit linear model using base R
lm_model <- lm(formula_model, data = student_data)

# Summarize and save main outputs
lm_summary <- summary(lm_model)

# Create tidy-like coefficients table (base R)
coeffs <- as.data.frame(coef(lm_summary))
coeffs$term <- rownames(coeffs)
names(coeffs) <- c("estimate", "std.error", "t.value", "p.value", "term")
coeffs <- coeffs[ , c("term","estimate","std.error","t.value","p.value")]

# Save coefficient table and full summary
write.csv(coeffs, file = "report/tables/Hafeez_lm_coefficients.csv", row.names = FALSE)
capture.output(lm_summary, file = "report/tables/Hafeez_lm_full_summary.txt")

# Save model stats (R-squared, adj R2, F-stat)
lm_stats <- data.frame(
  r.squared = lm_summary$r.squared,
  adj.r.squared = lm_summary$adj.r.squared,
  f.statistic = if(!is.null(lm_summary$fstatistic)) lm_summary$fstatistic[1] else NA,
  df1 = if(!is.null(lm_summary$fstatistic)) lm_summary$fstatistic[2] else NA,
  df2 = if(!is.null(lm_summary$fstatistic)) lm_summary$fstatistic[3] else NA
)
write.csv(lm_stats, file = "report/tables/Hafeez_lm_model_stats.csv", row.names = FALSE)

# =========================================
# VIF calculation using base R (no extra packages)
# VIF_j = 1 / (1 - R2_j) where R2_j is R^2 from regressing predictor j on other predictors
# =========================================
predictors <- c("Study_Hours", "Attendance.to.classes", "Taking.notes.in.classes",
                "Listening.in.classes", "Preparation.to.midterm.exams.1")

vif_calc <- function(df, pred) {
  others <- setdiff(predictors, pred)
  f <- as.formula(paste(pred, "~", paste(others, collapse = " + ")))
  mod <- lm(f, data = df)
  r2 <- summary(mod)$r.squared
  vif <- ifelse(1 - r2 == 0, NA, 1 / (1 - r2))
  return(vif)
}

vif_vals <- sapply(predictors, function(p) {
  if (p %in% names(student_data)) vif_calc(student_data, p) else NA
})
vif_df <- data.frame(predictor = names(vif_vals), VIF = round(as.numeric(vif_vals), 3), row.names = NULL)
write.csv(vif_df, "report/tables/Hafeez_vif.csv", row.names = FALSE)
print(vif_df)

# =========================================
# Residual diagnostics (base R)
# =========================================
resid_vals <- residuals(lm_model)
fitted_vals <- fitted(lm_model)

# Save residuals / fitted table
write.csv(data.frame(fitted = fitted_vals, residuals = resid_vals),
          "report/tables/Hafeez_residuals_fitted.csv", row.names = FALSE)

# Shapiro-Wilk test for normality of residuals (note: >5000 obs gives error; here n=145 so ok)
shapiro_res <- shapiro.test(resid_vals)
capture.output(shapiro_res, "report/tables/Hafeez_shapiro_residuals.txt")
print(shapiro_res)

# Basic diagnostic plots saved as PNG
png("report/figures/Hafeez_resid_vs_fitted.png", width = 800, height = 600)
plot(fitted_vals, resid_vals, main = "Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

png("report/figures/Hafeez_qqplot.png", width = 800, height = 600)
qqnorm(resid_vals); qqline(resid_vals, col = "red")
dev.off()



# =========================================
# (Hafeez) - Step 4: Effect sizes, partial R2 approximation & save outputs
# =========================================

# Standardized coefficients (simple approach: scale variables and re-fit)
std_lm <- lm(scale(GPA) ~ scale(Study_Hours) + scale(Attendance.to.classes) +
               scale(Taking.notes.in.classes) + scale(Listening.in.classes) +
               scale(Preparation.to.midterm.exams.1), data = student_data)
std_coefs <- as.data.frame(coef(summary(std_lm)))
std_coefs$term <- rownames(std_coefs)
names(std_coefs)[1:4] <- c("estimate","std.error","t.value","p.value")
std_coefs <- std_coefs[ , c("term","estimate","std.error","t.value","p.value")]
write.csv(std_coefs, "report/tables/Hafeez_standardized_coefficients.csv", row.names = FALSE)

# Partial R2 approx: compare full model R2 to model without each predictor
full_r2 <- summary(lm_model)$r.squared
partial_R2 <- sapply(predictors, function(p) {
  others <- setdiff(predictors, p)
  f <- as.formula(paste("GPA ~", paste(others, collapse = " + ")))
  m <- lm(f, data = student_data)
  max(0, full_r2 - summary(m)$r.squared)
})
partial_R2_df <- data.frame(predictor = predictors, partial_R2 = round(partial_R2, 4))
write.csv(partial_R2_df, "report/tables/Hafeez_partial_R2.csv", row.names = FALSE)
print(partial_R2_df)

# Save model object (RData) for reproducibility
save(lm_model, file = "report/tables/Hafeez_lm_model.RData")

# One-line summary saved for quick paste into report
one_line <- paste0("Linear model: GPA ~ Study_Hours + Attendance + TakingNotes + Listening + ExamPrep; R2 = ",
                   round(full_r2, 3, digits = 3))
cat(one_line, file = "report/tables/Hafeez_one_line_summary.txt")

