# =========================================
# (Bhavani) - FINAL merged Analysis.R
# Merge of: Pooji (cleaning), Zaheer (descriptives), Hafeez (effect sizes), Taslim (hypothesis testing)
# Follow-up: export git log, select top 3 commits, add Appendix B
# =========================================

# -----------------
# STEP 1: Load cleaned dataset & quick preview
# -----------------
# Purpose: load the final cleaned dataset produced by Pooji and confirm structure.
# Run -> produces: quick console preview

student_data <- read.csv("Final_Clean_Student_Data.csv", stringsAsFactors = FALSE)

# quick structure and head
str(student_data)
head(student_data)
cat("Rows:", nrow(student_data), "Cols:", ncol(student_data), "\n")
