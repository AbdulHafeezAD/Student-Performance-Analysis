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
