# =============================================================================
# COMPOUND ANALYSIS - COMPONENT B PROJECT
# Author: Ashima Singh
# Description: Analysis of compound co-occurrence, frequency, and categorization
#              from opioid overdose surveillance data
# =============================================================================


# -----------------------------------------------------------------------------
# SECTION 1 - LOAD LIBRARIES
# -----------------------------------------------------------------------------

install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("openxlsx")
install.packages("purrr")

library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(purrr)


# -----------------------------------------------------------------------------
# SECTION 2 - IMPORT DATA
# -----------------------------------------------------------------------------

data_new <- read.csv("C:/Users/YourName/Desktop/yourfile.csv",
                     stringsAsFactors = FALSE)

# Check data
head(data_new)
colnames(data_new)
str(data_new)


# -----------------------------------------------------------------------------
# SECTION 3 - PIVOT TO LONG FORMAT
# -----------------------------------------------------------------------------

data_new_long <- data_new %>%
  pivot_longer(
    cols = starts_with("Compound"),
    names_to = "CompoundColumn",
    values_to = "CompoundValue"
  ) %>%
  filter(!is.na(CompoundValue) & CompoundValue != "")

# Check long format
head(data_new_long)
colnames(data_new_long)


# -----------------------------------------------------------------------------
# SECTION 4 - ADD MONTH YEAR COLUMN
# -----------------------------------------------------------------------------

data_new_long <- data_new_long %>%
  mutate(MonthYear = format(as.Date(Date, format = "%m/%d/%Y"), "%b-%Y"))

# Check unique compounds
unique(data_new_long$CompoundValue)


# -----------------------------------------------------------------------------
# SECTION 5 - ADD CATEGORY COLUMN
# -----------------------------------------------------------------------------
# Replace compound names below with your actual compound names
# Use unique(data_new_long$CompoundValue) to get exact names

data_new_long <- data_new_long %>%
  mutate(Category = case_when(
    CompoundValue %in% c("Heroin", "Fentanyl", "Oxycodone") ~ "Opioids",
    CompoundValue %in% c("THC", "CBD")                      ~ "Cannabinoids",
    CompoundValue %in% c("Cocaine", "Meth")                 ~ "Stimulants",
    CompoundValue %in% c("Xanax", "Valium")                 ~ "Benzodiazepines",
    TRUE                                                     ~ "Other"
  ))


# -----------------------------------------------------------------------------
# SECTION 6 - UNIQUE COMPOUNDS PER MONTH
# -----------------------------------------------------------------------------

monthly_unique <- data_new_long %>%
  group_by(MonthYear) %>%
  summarise(UniqueCompounds = n_distinct(CompoundValue)) %>%
  arrange(MonthYear)

View(monthly_unique)


# -----------------------------------------------------------------------------
# SECTION 7 - COMPOUND FREQUENCY BY MONTH
# -----------------------------------------------------------------------------

compound_total <- data_new_long %>%
  group_by(MonthYear, CompoundValue) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(MonthYear, desc(Frequency))

View(compound_total)


# -----------------------------------------------------------------------------
# SECTION 8 - COMPOUND FREQUENCY BY MONTH AND CATEGORY
# -----------------------------------------------------------------------------

# Frequency for each compound
compound_freq <- data_new_long %>%
  group_by(MonthYear, Category, CompoundValue) %>%
  summarise(Frequency = n(), .groups = "drop")

# Total frequency for each category
category_freq <- data_new_long %>%
  group_by(MonthYear, Category) %>%
  summarise(CategoryTotal = n(), .groups = "drop")

# Join both together
compound_category <- compound_freq %>%
  left_join(category_freq, by = c("MonthYear", "Category")) %>%
  arrange(MonthYear, Category, desc(Frequency))

View(compound_category)


# -----------------------------------------------------------------------------
# SECTION 9 - COMPOUND PAIRS ANALYSIS
# -----------------------------------------------------------------------------

# Step 1 - Get all compounds per sample
budget_pairs <- data_new_long %>%
  group_by(ParticipantID, Date) %>%
  summarise(Compounds = list(sort(unique(CompoundValue))), .groups = "drop") %>%
  filter(lengths(Compounds) >= 2)

# Step 2 - Generate all pairs
pair_combinations <- budget_pairs %>%
  mutate(pairs_col = map(Compounds, ~as.data.frame(t(combn(.x, 2))))) %>%
  unnest(pairs_col)

# Rename columns
colnames(pair_combinations)[colnames(pair_combinations) == "V1"] <- "Compound1"
colnames(pair_combinations)[colnames(pair_combinations) == "V2"] <- "Compound2"

# Step 3 - Count frequency of each pair by participant and month
pair_frequency <- pair_combinations %>%
  mutate(MonthYear = format(as.Date(Date, format = "%m/%d/%Y"), "%b-%Y")) %>%
  group_by(ParticipantID, MonthYear, Compound1, Compound2) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(ParticipantID, MonthYear, desc(Frequency))

View(pair_frequency)


# -----------------------------------------------------------------------------
# SECTION 10 - COMPOUND TRIPLETS ANALYSIS
# -----------------------------------------------------------------------------

# Step 1 - Get all compounds per sample
triplet_frequency <- data_new_long %>%
  group_by(ParticipantID, Date) %>%
  filter(n_distinct(CompoundValue) >= 3) %>%
  summarise(Compounds = list(sort(unique(CompoundValue))), .groups = "drop") %>%
  mutate(
    Compound1 = sapply(Compounds, function(x) x[1]),
    Compound2 = sapply(Compounds, function(x) x[2]),
    Compound3 = sapply(Compounds, function(x) x[3])
  ) %>%
  mutate(MonthYear = format(as.Date(Date, format = "%m/%d/%Y"), "%b-%Y")) %>%
  group_by(ParticipantID, MonthYear, Compound1, Compound2, Compound3) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(ParticipantID, MonthYear, desc(Frequency))

View(triplet_frequency)


# -----------------------------------------------------------------------------
# SECTION 11 - SAVE ALL RESULTS TO EXCEL
# -----------------------------------------------------------------------------

# Save each table as separate Excel file
write.xlsx(monthly_unique,
           file.path(Sys.getenv("USERPROFILE"), "Desktop", "monthly_unique.xlsx"))

write.xlsx(compound_total,
           file.path(Sys.getenv("USERPROFILE"), "Desktop", "compound_total.xlsx"))

write.xlsx(compound_category,
           file.path(Sys.getenv("USERPROFILE"), "Desktop", "compound_category.xlsx"))

write.xlsx(pair_frequency,
           file.path(Sys.getenv("USERPROFILE"), "Desktop", "pair_frequency.xlsx"))

write.xlsx(triplet_frequency,
           file.path(Sys.getenv("USERPROFILE"), "Desktop", "triplet_frequency.xlsx"))


# OR save ALL tables in one Excel file with separate sheets
wb <- createWorkbook()

addWorksheet(wb, "Monthly Unique")
writeData(wb, "Monthly Unique", monthly_unique)

addWorksheet(wb, "Compound Total")
writeData(wb, "Compound Total", compound_total)

addWorksheet(wb, "Compound Category")
writeData(wb, "Compound Category", compound_category)

addWorksheet(wb, "Pairs")
writeData(wb, "Pairs", pair_frequency)

addWorksheet(wb, "Triplets")
writeData(wb, "Triplets", triplet_frequency)

saveWorkbook(wb,
             file.path(Sys.getenv("USERPROFILE"), "Desktop", "complete_analysis.xlsx"),
             overwrite = TRUE)

# =============================================================================
# END OF SCRIPT
# =============================================================================
