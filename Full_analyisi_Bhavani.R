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