# =========================================
# (Zaheer) - Descriptive Statistics
# Step 1: Load Clean Dataset & Initial Preview
# =========================================

student_data <- read.csv("Final_Clean_Student_Data.csv", stringsAsFactors = FALSE)

# View structure of dataset
str(student_data)

# View first 6 rows
head(student_data)

# =========================================
# Step 2: Descriptive Statistics
# =========================================

# -----------------------------------------
# 2.1 Numerical Summary Statistics
# -----------------------------------------

# Select numerical columns for summary
numeric_cols <- c(
  "Study_Hours",
  "GPA",
  "Expected_GPA",
  "Final_Grade",
  "Number.of.sisters.brothers"
)

# Summary statistics (min, max, mean, median, quartiles)
summary(student_data[ , numeric_cols])

# Standard deviation for each numeric variable
sapply(student_data[ , numeric_cols], sd)


# -----------------------------------------
# 2.2 Frequency Tables for Categorical Variables
# -----------------------------------------

# Example: Sex distribution
table(student_data$Sex)

# Example: Attendance to classes
table(student_data$Attendance.to.classes)

# Example: Reading frequency
table(student_data$Reading.frequency)

# Example: Partner status
table(student_data$Do.you.have.a.partner)


# -----------------------------------------
# 2.3 Cross-Tabulation (Two-way Table)
# -----------------------------------------

# Relationship between Sex and Final Grade
table(student_data$Sex, student_data$Final_Grade)

# Relationship between Study Hours groups and Grade
study_group <- cut(student_data$Study_Hours,
                   breaks = c(0,1,2,3,4),
                   labels = c("0–1 hr", "1–2 hrs", "2–3 hrs", "3–4 hrs"))

table(study_group, student_data$Final_Grade)

