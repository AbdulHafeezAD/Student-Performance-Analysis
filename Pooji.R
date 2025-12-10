# =========================================
# (Pooji) - Data Loading & Cleaning
# =========================================

# Load the dataset
student_data <- read.csv("StudentsPerformance_with_headers.csv", stringsAsFactors = FALSE)

# View structure
str(student_data)

# View first 6 rows
head(student_data)


# =========================================
# Step 2: Check Missing Values
# =========================================

# Count missing values in each column
colSums(is.na(student_data))

# Total missing values in the whole dataset
sum(is.na(student_data))



# =========================================
# Step 2: Check Duplicate Records
# =========================================

# Count duplicate rows
sum(duplicated(student_data))


# =========================================
# Step 3: Convert appropriate categorical columns to factors
# =========================================

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


# =========================================
# Step 4: Rename key columns for easy analysis
# =========================================

names(student_data)[names(student_data) ==
                      "Weekly.study.hours"] <- "Study_Hours"

names(student_data)[names(student_data) ==
                      "Cumulative.grade.point.average.in.the.last.semester...4.00."] <- "GPA"

names(student_data)[names(student_data) ==
                      "Expected.Cumulative.grade.point.average.in.the.graduation...4.00."] <- "Expected_GPA"

names(student_data)[names(student_data) ==
                      "GRADE"] <- "Final_Grade"


# =========================================
# Step 5: Save Final Clean Dataset for Team
# =========================================

write.csv(student_data, "Final_Clean_Student_Data.csv", row.names = FALSE)


