# =============================================================================
# COMPOUND MISMATCH, PAIRS & TRIPLETS ANALYSIS - COMPONENT B PROJECT
# Author: Ashima Singh
# Description: Standalone analysis of:
#              1. Number of compounds detected per sample
#              2. Sold As (expected) vs Detected compound mismatch
#              3. Compound pairs - frequency + overall top pairs
#              4. Compound triplets - frequency + overall top triplets
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
# SECTION 5 - NUMBER OF COMPOUNDS DETECTED PER SAMPLE
# -----------------------------------------------------------------------------

compounds_per_sample <- data_new_long %>%
  group_by(ParticipantID, Date) %>%
  summarise(
    Compounds = list(sort(unique(CompoundValue))),
    NumDetected = n_distinct(CompoundValue),
    .groups = "drop"
  ) %>%
  mutate(
    DetectedCompounds = sapply(Compounds, paste, collapse = ", "),
    MonthYear = format(as.Date(Date, format = "%m/%d/%Y"), "%b-%Y")
  ) %>%
  select(ParticipantID, Date, MonthYear, DetectedCompounds, NumDetected)

View(compounds_per_sample)

# Summary - distribution of number of compounds detected, by month
num_detected_summary <- compounds_per_sample %>%
  group_by(MonthYear, NumDetected) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(MonthYear, NumDetected)

View(num_detected_summary)


# -----------------------------------------------------------------------------
# SECTION 6 - SOLD AS (EXPECTED) vs DETECTED MISMATCH ANALYSIS
# -----------------------------------------------------------------------------
# NOTE: Assumes a "SoldAs" column exists in data_new (the substance the
# participant reported buying/using). Adjust the column name below if yours
# is named differently (e.g. "Expected").

# Step 1 - Get detected compound list + SoldAs per sample
mismatch_check <- data_new_long %>%
  group_by(ParticipantID, Date) %>%
  summarise(
    SoldAs = first(SoldAs),
    Compounds = list(sort(unique(CompoundValue))),
    .groups = "drop"
  )

# Step 2 - Classify each sample as Match / No Match / Partial Match
mismatch_check <- mismatch_check %>%
  mutate(
    SoldAsDetected = map2_lgl(Compounds, SoldAs, ~ .y %in% .x),
    NumDetected = lengths(Compounds),
    MismatchStatus = case_when(
      is.na(SoldAs) | SoldAs == ""                       ~ "No Sold As Info",
      SoldAsDetected == TRUE  & NumDetected == 1          ~ "Match",
      SoldAsDetected == TRUE  & NumDetected > 1           ~ "Partial Match - Extras Present",
      SoldAsDetected == FALSE                             ~ "No Match - Sold As Not Detected",
      TRUE                                                ~ "Other"
    )
  )

# Step 3 - Unnest detected compounds into a readable comma-separated column
mismatch_check <- mismatch_check %>%
  mutate(DetectedCompounds = sapply(Compounds, paste, collapse = ", ")) %>%
  select(ParticipantID, Date, SoldAs, DetectedCompounds, NumDetected, MismatchStatus)

# Step 4 - Add MonthYear for consistency with other tables
mismatch_check <- mismatch_check %>%
  mutate(MonthYear = format(as.Date(Date, format = "%m/%d/%Y"), "%b-%Y"))

View(mismatch_check)

# Step 5 - Summary: mismatch counts by month
mismatch_summary <- mismatch_check %>%
  group_by(MonthYear, MismatchStatus) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(MonthYear, desc(Frequency))

View(mismatch_summary)


# -----------------------------------------------------------------------------
# SECTION 7 - COMPOUND PAIRS ANALYSIS
# -----------------------------------------------------------------------------

# Step 1 - Get all compounds per sample (only samples with 2+ compounds)
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

# Step 4 - TOP PAIRS OVERALL (across all participants and months)
top_pairs <- pair_combinations %>%
  group_by(Compound1, Compound2) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(desc(Frequency))

View(top_pairs)


# -----------------------------------------------------------------------------
# SECTION 8 - COMPOUND TRIPLETS ANALYSIS
# -----------------------------------------------------------------------------

# Step 1 - Get all compounds per sample (only samples with 3+ compounds)
budget_triplets <- data_new_long %>%
  group_by(ParticipantID, Date) %>%
  filter(n_distinct(CompoundValue) >= 3) %>%
  summarise(Compounds = list(sort(unique(CompoundValue))), .groups = "drop")

# Step 2 - Generate all triplets
triplet_combinations <- budget_triplets %>%
  mutate(triplets_col = map(Compounds, ~as.data.frame(t(combn(.x, 3))))) %>%
  unnest(triplets_col)

# Rename columns
colnames(triplet_combinations)[colnames(triplet_combinations) == "V1"] <- "Compound1"
colnames(triplet_combinations)[colnames(triplet_combinations) == "V2"] <- "Compound2"
colnames(triplet_combinations)[colnames(triplet_combinations) == "V3"] <- "Compound3"

# Step 3 - Count frequency of each triplet by participant and month
triplet_frequency <- triplet_combinations %>%
  mutate(MonthYear = format(as.Date(Date, format = "%m/%d/%Y"), "%b-%Y")) %>%
  group_by(ParticipantID, MonthYear, Compound1, Compound2, Compound3) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(ParticipantID, MonthYear, desc(Frequency))

View(triplet_frequency)

# Step 4 - TOP TRIPLETS OVERALL (across all participants and months)
top_triplets <- triplet_combinations %>%
  group_by(Compound1, Compound2, Compound3) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(desc(Frequency))

View(top_triplets)


# -----------------------------------------------------------------------------
# SECTION 9 - SAVE ALL RESULTS TO EXCEL
# -----------------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "Num Detected per Sample")
writeData(wb, "Num Detected per Sample", compounds_per_sample)

addWorksheet(wb, "Num Detected Summary")
writeData(wb, "Num Detected Summary", num_detected_summary)

addWorksheet(wb, "Mismatch Detail")
writeData(wb, "Mismatch Detail", mismatch_check)

addWorksheet(wb, "Mismatch Summary")
writeData(wb, "Mismatch Summary", mismatch_summary)

addWorksheet(wb, "Pairs")
writeData(wb, "Pairs", pair_frequency)

addWorksheet(wb, "Top Pairs")
writeData(wb, "Top Pairs", top_pairs)

addWorksheet(wb, "Triplets")
writeData(wb, "Triplets", triplet_frequency)

addWorksheet(wb, "Top Triplets")
writeData(wb, "Top Triplets", top_triplets)

saveWorkbook(wb,
             file.path(Sys.getenv("USERPROFILE"), "Desktop", "mismatch_pairs_triplets_analysis.xlsx"),
             overwrite = TRUE)

# =============================================================================
# END OF SCRIPT
# =============================================================================
