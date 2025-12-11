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


# -----------------
# STEP 2: Ensure consistent types & basic cleaning (convert appropriate columns to factors)
# -----------------
# Purpose: mirror Pooji's factor conversions to guarantee later steps run reproducibly.
# Run -> converts listed columns to factor (if present) and renames key columns.
factor_cols <- c(
  "Student.Age",
  "Sex",
  "Graduated.high.school.type",
  "Scholarship.type",
  "Additional.work",
  "Regular.artistic.or.sports.activity",
  "Do.you.have.a.partner",
  "Total.salary.if.available",
  "Transportation.to.the.university",
  "Accommodation.type.in.Cyprus",
  "Mother.s.education",
  "Father.s.education",
  "Parental.status",
  "Mother.s.occupation",
  "Father.s.occupation",
  "Reading.frequency",
  "Reading.frequency.1",
  "Attendance.to.the.seminars.conferences.related.to.the.department",
  "Impact.of.your.projects.activities.on.your.success",
  "Attendance.to.classes",
  "Preparation.to.midterm.exams.1",
  "Preparation.to.midterm.exams.2",
  "Taking.notes.in.classes",
  "Listening.in.classes",
  "Discussion.improves.my.interest.and.success.in.the.course",
  "Flip.classroom",
  "COURSE.ID"
)

for (v in factor_cols) {
  if (v %in% names(student_data)) {
    # only convert if not already a factor and not NA
    if (!is.factor(student_data[[v]])) {
      student_data[[v]] <- as.factor(student_data[[v]])
    }
  }
}

# Rename key columns for readability (same as Pooji)
if ("Weekly.study.hours" %in% names(student_data))
  names(student_data)[names(student_data) == "Weekly.study.hours"] <- "Study_Hours"

if ("Cumulative.grade.point.average.in.the.last.semester...4.00." %in% names(student_data))
  names(student_data)[names(student_data) == "Cumulative.grade.point.average.in.the.last.semester...4.00."] <- "GPA"

if ("Expected.Cumulative.grade.point.average.in.the.graduation...4.00." %in% names(student_data))
  names(student_data)[names(student_data) == "Expected.Cumulative.grade.point.average.in.the.graduation...4.00."] <- "Expected_GPA"

if ("GRADE" %in% names(student_data))
  names(student_data)[names(student_data) == "GRADE"] <- "Final_Grade"

# Save an intermediate cleaned file for the team (optional)
write.csv(student_data, "Final_Clean_Student_Data_v2.csv", row.names = FALSE)



# -----------------
# STEP 3: Descriptive Statistics 
# -----------------
# Purpose: numeric summaries, frequency tables, contingency tables, correlation, simple figures

# (create folders)
if (!dir.exists("report")) dir.create("report")
if (!dir.exists("report/tables")) dir.create("report/tables")
if (!dir.exists("report/figures")) dir.create("report/figures")

# Numeric columns chosen by Zaheer
numeric_cols <- c("Study_Hours", "GPA", "Expected_GPA", "Final_Grade", "Number.of.sisters.brothers")
numeric_cols <- numeric_cols[numeric_cols %in% names(student_data)]

# Summary and sd
if (length(numeric_cols) > 0) {
  numeric_summary <- data.frame(
    variable = numeric_cols,
    mean = sapply(student_data[numeric_cols], function(x) mean(as.numeric(x), na.rm = TRUE)),
    sd   = sapply(student_data[numeric_cols], function(x) sd(as.numeric(x), na.rm = TRUE)),
    median = sapply(student_data[numeric_cols], function(x) median(as.numeric(x), na.rm = TRUE)),
    IQR = sapply(student_data[numeric_cols], function(x) IQR(as.numeric(x), na.rm = TRUE)),
    min = sapply(student_data[numeric_cols], function(x) min(as.numeric(x), na.rm = TRUE)),
    max = sapply(student_data[numeric_cols], function(x) max(as.numeric(x), na.rm = TRUE)),
    n = sapply(student_data[numeric_cols], function(x) sum(!is.na(x))),
    stringsAsFactors = FALSE
  )
  numeric_summary[ , 2:7] <- round(numeric_summary[ , 2:7], 3)
  write.csv(numeric_summary, file = "report/tables/Bhavani_numeric_summary.csv", row.names = FALSE)
}

# Frequency tables for a shortlist of categorical vars (same list used by Zaheer)
cat_vars <- c("Sex", "Student.Age", "Scholarship.type", "Attendance.to.classes",
              "Taking.notes.in.classes", "Preparation.to.midterm.exams.1")
cat_vars <- cat_vars[cat_vars %in% names(student_data)]

for (v in cat_vars) {
  tab <- as.data.frame(table(student_data[[v]], useNA = "ifany"))
  names(tab) <- c(v, "count")
  tab$percent <- round(100 * tab$count / sum(tab$count), 1)
  write.csv(tab, file = paste0("report/tables/Bhavani_freq_", v, ".csv"), row.names = FALSE)
}

# Contingency examples (Attendance x GPA_cat; Sex x Scholarship row%):
if ("GPA" %in% names(student_data)) {
  student_data$GPA_cat <- cut(as.numeric(student_data$GPA),
                              breaks = c(-Inf, 1.5, 2.5, Inf),
                              labels = c("Low", "Medium", "High"))
  ct1 <- table(student_data$Attendance.to.classes, student_data$GPA_cat, useNA = "ifany")
  ct1_df <- as.data.frame.matrix(ct1)
  ct1_df <- cbind(Attendance = rownames(ct1_df), ct1_df); rownames(ct1_df) <- NULL
  write.csv(ct1_df, file = "report/tables/Bhavani_ct_Attendance_GPAcat.csv", row.names = FALSE)
}

if (all(c("Sex","Scholarship.type") %in% names(student_data))) {
  ct2 <- prop.table(table(student_data$Sex, student_data$Scholarship.type), margin = 1) * 100
  ct2_df <- as.data.frame.matrix(round(ct2, 1)); ct2_df <- cbind(Sex = rownames(ct2_df), ct2_df); rownames(ct2_df) <- NULL
  write.csv(ct2_df, file = "report/tables/Bhavani_ct_Sex_Scholarship_rowpct.csv", row.names = FALSE)
}

# Correlation matrix (numeric subset)
num_for_cor <- na.omit(as.data.frame(lapply(student_data[numeric_cols], as.numeric)))
if (nrow(num_for_cor) >= 2) {
  cor_mat <- round(cor(num_for_cor), 3)
  write.csv(as.data.frame(cor_mat), file = "report/tables/Bhavani_correlation_matrix.csv", row.names = TRUE)
}

# Simple figures (same as Zaheer)
if ("GPA" %in% names(student_data)) {
  png(filename = "report/figures/Bhavani_hist_GPA.png", width = 800, height = 600)
  hist(as.numeric(student_data$GPA), main = "Distribution of GPA", xlab = "GPA (0-4)", ylab = "Frequency",
       breaks = 8, col = "lightblue", border = "white")
  dev.off()
}

if (all(c("Study_Hours","GPA") %in% names(student_data))) {
  png(filename = "report/figures/Bhavani_scatter_StudyHours_GPA.png", width = 800, height = 600)
  plot(as.numeric(student_data$Study_Hours), as.numeric(student_data$GPA),
       main = "Study Hours vs GPA", xlab = "Weekly Study Hours", ylab = "GPA", pch = 19)
  if (sum(!is.na(student_data$Study_Hours) & !is.na(student_data$GPA)) >= 2) {
    mod_tmp <- lm(as.numeric(GPA) ~ as.numeric(Study_Hours), data = student_data)
    abline(mod_tmp, col = "red", lwd = 2)
  }
  dev.off()
}

if (all(c("Attendance.to.classes","GPA") %in% names(student_data))) {
  png(filename = "report/figures/Bhavani_box_GPA_by_Attendance.png", width = 800, height = 600)
  boxplot(as.numeric(GPA) ~ Attendance.to.classes, data = student_data,
          main = "GPA by Attendance", xlab = "Attendance (coded)", ylab = "GPA", col = "lightgray")
  dev.off()
  
}



# -----------------
# STEP 4: Modelling, standardized coefs & partial R2 (Hafeez/Taslim style)
# -----------------
# Purpose: fit linear model, standardized coefficients, save model and tables.
# Ensure analysis variables are numeric (same as earlier)
predictors <- c("Study_Hours", "Attendance.to.classes", "Taking.notes.in.classes",
                "Listening.in.classes", "Preparation.to.midterm.exams.1")
all_vars <- c("GPA", predictors)
for (v in intersect(names(student_data), all_vars)) {
  if (is.factor(student_data[[v]])) {
    student_data[[v]] <- as.numeric(as.character(student_data[[v]]))
  }
}

# Fit main linear model (same formula used by Hafeez/Taslim)
lm_model <- lm(GPA ~ Study_Hours + Attendance.to.classes + Taking.notes.in.classes +
                 Listening.in.classes + Preparation.to.midterm.exams.1, data = student_data)

# Save model object
save(lm_model, file = "report/tables/Bhavani_lm_model.RData")

# Standardized coefficients: scale predictors and response
std_lm <- lm(scale(GPA) ~ scale(Study_Hours) + scale(Attendance.to.classes) +
               scale(Taking.notes.in.classes) + scale(Listening.in.classes) +
               scale(Preparation.to.midterm.exams.1), data = student_data)
std_coefs <- as.data.frame(coef(summary(std_lm)))
std_coefs$term <- rownames(std_coefs)
names(std_coefs)[1:4] <- c("estimate","std.error","t.value","p.value")
std_coefs <- std_coefs[ , c("term","estimate","std.error","t.value","p.value")]
write.csv(std_coefs, "report/tables/Bhavani_standardized_coefficients.csv", row.names = FALSE)

# Partial R2 approx (compare full R2 to model without each predictor)
full_r2 <- summary(lm_model)$r.squared
partial_R2 <- sapply(predictors, function(p) {
  others <- setdiff(predictors, p)
  f <- as.formula(paste("GPA ~", paste(others, collapse = " + ")))
  m <- lm(f, data = student_data)
  max(0, full_r2 - summary(m)$r.squared)
})
partial_R2_df <- data.frame(predictor = predictors, partial_R2 = round(partial_R2, 4))
write.csv(partial_R2_df, "report/tables/Bhavani_partial_R2.csv", row.names = FALSE)

# Save one-line summary and model R2
one_line <- paste0("Linear model: GPA ~ Study_Hours + Attendance + TakingNotes + Listening + ExamPrep; R2 = ",
                   round(full_r2, 3))
cat(one_line, file = "report/tables/Bhavani_one_line_summary.txt")


# -----------------
# STEP 5: Hypothesis testing outputs and diagnostics (Taslim-style)
# -----------------
# Purpose: collect p-values, decisions, residual checks, and save for appendix

# p-values and decisions
lm_sum <- summary(lm_model)
overall_p <- if (!is.null(lm_sum$fstatistic)) {
  pf(lm_sum$fstatistic[1], lm_sum$fstatistic[2], lm_sum$fstatistic[3], lower.tail = FALSE)
} else NA

coef_tab <- as.data.frame(coef(summary(lm_model)))
coef_tab$term <- rownames(coef_tab)
names(coef_tab)[1:4] <- c("estimate", "std.error", "t.value", "p.value")
coef_tab <- coef_tab[, c("term", "estimate", "std.error", "t.value", "p.value")]
alpha <- 0.05
overall_decision <- if (!is.na(overall_p) && overall_p < alpha) "Reject H0 (overall model significant)" else "Do not reject H0 (overall model not significant)"
coef_tab$decision <- ifelse(coef_tab$p.value < alpha, "Reject H0 (significant)", "Do not reject H0 (not significant)")
write.csv(coef_tab, "report/tables/Bhavani_predictor_pvalues.csv", row.names = FALSE)
write.csv(data.frame(overall_p = overall_p, overall_decision = overall_decision), "report/tables/Bhavani_overall_test_decision.csv", row.names = FALSE)

# Residuals & fitted saved
resid_df <- data.frame(fitted = fitted(lm_model), residuals = residuals(lm_model))
write.csv(resid_df, "report/tables/Bhavani_residuals_fitted.csv", row.names = FALSE)

# Shapiro-Wilk (normality) and save output text
shapiro_res <- tryCatch(shapiro.test(residuals(lm_model)), error = function(e) e)
capture.output(shapiro_res, file = "report/tables/Bhavani_shapiro_residuals.txt")

# Residuals vs fitted plot
png("report/figures/Bhavani_resid_vs_fitted.png", width = 800, height = 600)
plot(fitted(lm_model), residuals(lm_model), main = "Residuals vs Fitted (Bhavani)", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

# -----------------
# STEP 6: Merge scripts & tidy (final Analysis.R saved) + export git log guidance
# -----------------
# Purpose: create final single script (this file), ensure no duplicates and scripts combined.


# Save a copy of this full merged script workspace (useful for reproducibility)
save.image(file = "report/tables/Bhavani_full_workspace.RData")

# Guidance to export git log (run on command line, not inside R)
cat("
--- Git instructions (run in your local project root, not inside R) ---
# Export full git log to file for Appendix B:
git log --pretty=format:'%h %ad | %s | %an' --date=short > report/tables/GIT_LOG.txt

# To select top 3 commits manually: inspect GIT_LOG.txt and copy the three significant commits with
# Commit message + explanation (Bhavani will prepare Appendix B).
----------------------------------------------------------------------
")

# End of script
cat('Bhavani merged Analysis.R complete. Check report/tables and report/figures for outputs.\n')


