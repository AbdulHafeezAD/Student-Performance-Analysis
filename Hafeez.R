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
