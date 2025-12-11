# =========================================
# (Taslim) - Step 1: Load data and (if present) saved model
# =========================================

# Load cleaned dataset
student_data <- read.csv("Final_Clean_Student_Data.csv", stringsAsFactors = FALSE)

# Try to load Hafeez's saved lm_model if it exists
if (file.exists("report/tables/Hafeez_lm_model.RData")) {
  load("report/tables/Hafeez_lm_model.RData") # should load object 'lm_model' if saved
  cat("Loaded Hafeez_lm_model.RData\n")
} else {
  cat("Hafeez_lm_model.RData not found — model will be fitted locally.\n")
}

# Quick preview
str(student_data)
cat("Rows:", nrow(student_data), "Cols:", ncol(student_data), "\n")


# =========================================
# (Taslim) - Step 2: Ensure analysis variables numeric and fit model if missing
# =========================================

# Analysis variables (must match Hafeez predictors)
analysis_vars <- c("GPA", "Study_Hours", "Attendance.to.classes",
                   "Taking.notes.in.classes", "Listening.in.classes",
                   "Preparation.to.midterm.exams.1", "Final_Grade")

# Convert factor-coded variables to numeric if needed
for (v in intersect(names(student_data), analysis_vars)) {
  if (is.factor(student_data[[v]])) {
    student_data[[v]] <- as.numeric(as.character(student_data[[v]]))
  }
}

# If lm_model is not in environment, fit the same model Hafeez used
if (!exists("lm_model")) {
  formula_model <- GPA ~ Study_Hours + Attendance.to.classes + Taking.notes.in.classes +
    Listening.in.classes + Preparation.to.midterm.exams.1
  lm_model <- lm(formula_model, data = student_data)
  cat("Fitted local lm_model.\n")
} else {
  cat("Using loaded lm_model from Hafeez.\n")
}

# Save a copy of the model used by Taslim (so versioning is clear)
save(lm_model, file = "report/tables/Taslim_lm_model_used.RData")

# Basic summary printed
print(summary(lm_model))


# =========================================
# (Taslim) - Step 3: Extract p-values and test decisions (overall & per-predictor)
# =========================================

# Create output folder if missing
if (!dir.exists("report/tables")) dir.create("report/tables", recursive = TRUE)

# Overall model F-test p-value (from anova or summary)
anova_tab <- anova(lm_model)
# If anova gives the model row first (depends on LM), capture the model p-value via the F-stat in summary
lm_sum <- summary(lm_model)
# Overall F-statistic p-value (if present)
overall_p <- if (!is.null(lm_sum$fstatistic)) {
  pf_val <- pf(lm_sum$fstatistic[1], lm_sum$fstatistic[2], lm_sum$fstatistic[3], lower.tail = FALSE)
  pf_val
} else {
  NA
}

# Per-predictor p-values from coef summary
coef_tab <- as.data.frame(coef(summary(lm_model)))
coef_tab$term <- rownames(coef_tab)
names(coef_tab)[1:4] <- c("estimate", "std.error", "t.value", "p.value")
coef_tab <- coef_tab[, c("term", "estimate", "std.error", "t.value", "p.value")]

# Decision rule: alpha = 0.05
alpha <- 0.05

# Overall test decision
overall_decision <- if (!is.na(overall_p) && overall_p < alpha) "Reject H0 (overall model significant)" else "Do not reject H0 (overall model not significant)"

# Per-predictor decisions
coef_tab$decision <- ifelse(coef_tab$p.value < alpha, "Reject H0 (significant)", "Do not reject H0 (not significant)")

# Save outputs
write.csv(coef_tab, "report/tables/Taslim_predictor_pvalues.csv", row.names = FALSE)
write.csv(data.frame(overall_p = overall_p, overall_decision = overall_decision), "report/tables/Taslim_overall_test_decision.csv", row.names = FALSE)

# Print summary to console for quick inspection
cat("Overall model p-value:", overall_p, "\nDecision:", overall_decision, "\n\n")
print(coef_tab)

