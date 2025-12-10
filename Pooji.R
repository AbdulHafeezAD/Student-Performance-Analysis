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


