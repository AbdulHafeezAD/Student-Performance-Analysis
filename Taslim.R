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
