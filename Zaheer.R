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

# =========================================
# Step 3: Create Summary Tables & Save Outputs
# =========================================

# -----------------------------------------
# 3.0 Create output folders
# -----------------------------------------
# (These folders will hold tables and figures for the report)
if (!dir.exists("report")) dir.create("report")
if (!dir.exists("report/tables")) dir.create("report/tables")
if (!dir.exists("report/figures")) dir.create("report/figures")

# -----------------------------------------
# 3.1 Tidy numeric summary table (mean, sd, median, IQR, min, max, n)
# -----------------------------------------
numeric_cols <- c("Study_Hours", "GPA", "Expected_GPA", "Final_Grade", "Number.of.sisters.brothers")

# helper functions
col_mean <- function(x) mean(x, na.rm = TRUE)
col_sd   <- function(x) sd(x, na.rm = TRUE)
col_median <- function(x) median(x, na.rm = TRUE)
col_IQR  <- function(x) IQR(x, na.rm = TRUE)
col_min  <- function(x) min(x, na.rm = TRUE)
col_max  <- function(x) max(x, na.rm = TRUE)
col_n    <- function(x) sum(!is.na(x))

# Build summary data.frame
numeric_summary <- data.frame(
  variable = numeric_cols,
  mean = sapply(student_data[numeric_cols], col_mean),
  sd = sapply(student_data[numeric_cols], col_sd),
  median = sapply(student_data[numeric_cols], col_median),
  IQR = sapply(student_data[numeric_cols], col_IQR),
  min = sapply(student_data[numeric_cols], col_min),
  max = sapply(student_data[numeric_cols], col_max),
  n = sapply(student_data[numeric_cols], col_n),
  row.names = NULL,
  stringsAsFactors = FALSE
)

# Round numeric columns for nicer reporting
numeric_summary[ , 2:7] <- round(numeric_summary[ , 2:7], 3)

# Save CSV for report tables
write.csv(numeric_summary, file = "report/tables/Zaheer_numeric_summary.csv", row.names = FALSE)

# -----------------------------------------
# 3.2 Frequency tables for key categorical variables (save as CSV)
# -----------------------------------------
cat_vars <- c("Sex", "Student.Age", "Scholarship.type", "Attendance.to.classes",
              "Taking.notes.in.classes", "Preparation.to.midterm.exams.1")

for (v in cat_vars) {
  tab <- as.data.frame(table(student_data[[v]], useNA = "ifany"))
  names(tab) <- c(v, "count")
  tab$percent <- round(100 * tab$count / sum(tab$count), 1)
  write.csv(tab, file = paste0("report/tables/Zaheer_freq_", v, ".csv"), row.names = FALSE)
}

# -----------------------------------------
# 3.3 Contingency tables required by rubric (clear labeling) and save
# -----------------------------------------
# Example 1: Attendance to classes x GPA category (Low/Medium/High)
student_data$GPA_cat <- cut(student_data$GPA,
                            breaks = c(-Inf, 1.5, 2.5, Inf),
                            labels = c("Low", "Medium", "High"))

ct1 <- table(student_data$Attendance.to.classes, student_data$GPA_cat, useNA = "ifany")
# convert to data.frame for saving
ct1_df <- as.data.frame.matrix(ct1)
# add rownames as a column
ct1_df <- cbind(Attendance = rownames(ct1_df), ct1_df)
rownames(ct1_df) <- NULL
write.csv(ct1_df, file = "report/tables/Zaheer_ct_Attendance_GPAcat.csv", row.names = FALSE)

# Example 2: Sex x Scholarship.type (row percentages saved)
ct2 <- prop.table(table(student_data$Sex, student_data$Scholarship.type), margin = 1) * 100
ct2_df <- as.data.frame.matrix(round(ct2, 1))
ct2_df <- cbind(Sex = rownames(ct2_df), ct2_df)
rownames(ct2_df) <- NULL
write.csv(ct2_df, file = "report/tables/Zaheer_ct_Sex_Scholarship_rowpct.csv", row.names = FALSE)

# -----------------------------------------
# 3.4 Correlation matrix for numeric variables and save
# -----------------------------------------
num_for_cor <- na.omit(student_data[ , numeric_cols])
if (nrow(num_for_cor) >= 2) {
  cor_mat <- round(cor(num_for_cor), 3)
  write.csv(as.data.frame(cor_mat), file = "report/tables/Zaheer_correlation_matrix.csv", row.names = TRUE)
} else {
  cat("Not enough complete rows for correlation matrix.\n")
}

# -----------------------------------------
# 3.5 Export simple figures required by rubric (histogram, scatter, boxplot)
# -----------------------------------------
# Histogram of GPA
png(filename = "report/figures/Zaheer_hist_GPA.png", width = 800, height = 600)
hist(student_data$GPA,
     main = "Distribution of GPA",
     xlab = "GPA (0-4)",
     ylab = "Frequency",
     breaks = 8,
     col = "lightblue",
     border = "white")
dev.off()

# Scatter: Study hours vs GPA (with simple smoothing line if available)
png(filename = "report/figures/Zaheer_scatter_StudyHours_GPA.png", width = 800, height = 600)
plot(student_data$Study_Hours, student_data$GPA,
     main = "Study Hours vs GPA",
     xlab = "Weekly Study Hours",
     ylab = "GPA",
     pch = 19, col = "darkgreen")
if (sum(!is.na(student_data$Study_Hours) & !is.na(student_data$GPA)) >= 2) {
  model <- lm(GPA ~ Study_Hours, data = student_data)
  abline(model, col = "red", lwd = 2)
}
dev.off()

# Boxplot: GPA by Attendance
png(filename = "report/figures/Zaheer_box_GPA_by_Attendance.png", width = 800, height = 600)
boxplot(GPA ~ Attendance.to.classes, data = student_data,
        main = "GPA by Attendance to Classes",
        xlab = "Attendance (coded)",
        ylab = "GPA",
        col = "lightgray")
dev.off()

# -----------------------------------------
# 3.6 Save a compact RData for quick reuse
# -----------------------------------------
save(student_data, numeric_summary, file = "report/tables/Zaheer_summary_workspace.RData")

