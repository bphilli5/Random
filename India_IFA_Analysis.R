library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(broom)


folder_path <- "filepath"

# Get list of CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Function to extract quarter and year from filename
extract_metadata <- function(filename) {
  # Extract the base name of the file (without path and without extension)
  base_name <- tools::file_path_sans_ext(basename(filename))
  
  # Extract quarter (assuming it's always in the format 'Q1', 'Q2', etc.)
  quarter <- stringr::str_extract(base_name, "Q[1-4]")
  
  # Extract year (assuming it's always a 4-digit number at the end)
  year <- stringr::str_extract(base_name, "\\d{4}$")
  
  return(list(quarter = quarter, year = year))
}

# Import all CSV files into a list, adding metadata columns
data_list <- lapply(csv_files, function(file) {
  df <- read.csv(file)
  metadata <- extract_metadata(file)
  df$quarter <- metadata$quarter
  df$year <- metadata$year
  df$filename <- basename(file)  # Add filename as a column for reference
  return(df)
})

# Combine all data frames into one
combined_df <- do.call(rbind, data_list)

# Reset row names
rownames(combined_df) <- NULL

combined_df <- combined_df %>%
  mutate(
    year = as.numeric(year),
    numeric_time = case_when(
      quarter == "Q1" ~ year + 0/4,
      quarter == "Q2" ~ year + 1/4,
      quarter == "Q3" ~ year + 2/4,
      quarter == "Q4" ~ year + 3/4,
      TRUE ~ NA_real_
    )
  ) %>% rename(
    "IFA Syrup" = names(combined_df)[2],
    "Pink IFA Tablets" = names(combined_df)[3],
    "Blue IFA Tablets" = names(combined_df)[4]
  ) %>%
  select(-5, -6) %>% 
  mutate(numeric_time = numeric_time - min(numeric_time, na.rm = TRUE) + 1) %>% 
  select(Location, `IFA Syrup`, `Pink IFA Tablets`, `Blue IFA Tablets`, year, quarter, numeric_time)

# Assuming your data is in a dataframe called 'df'

summary_table <- combined_df %>%
  group_by(Location) %>%
  summarize(
    IFA_Syrup_Start = `IFA Syrup`[numeric_time=1],
    Pink_IFA_Tablets_Start = `Pink IFA Tablets`[numeric_time=1],
    Blue_IFA_Tablets_Start = `Blue IFA Tablets`[numeric_time=1],
    IFA_Syrup_End = `IFA Syrup`[numeric_time=5],
    Pink_IFA_Tablets_End = `Pink IFA Tablets`[numeric_time=5],
    Blue_IFA_Tablets_End = `Blue IFA Tablets`[numeric_time=5]
  )

# Calculate trend lines (slopes)
trend_lines <- combined_df %>%
  pivot_longer(cols = c(`IFA Syrup`, `Pink IFA Tablets`, `Blue IFA Tablets`), 
               names_to = "Supplement_Type", 
               values_to = "Value") %>%
  group_by(Location, Supplement_Type) %>%
  do(tidy(lm(Value ~ numeric_time, data = .))) %>%
  filter(term == "numeric_time") %>%
  select(Location, Supplement_Type, slope = estimate) %>%
  pivot_wider(names_from = Supplement_Type, 
              values_from = slope, 
              names_prefix = "Trend_")

# Join the summary table with trend lines
final_summary <- summary_table %>%
  left_join(trend_lines, by = "Location") %>% 
  mutate(across(starts_with("Trend_"), ~round(., 2))) %>% 
  select(
    Location,
    IFA_Syrup_Start,
    IFA_Syrup_End,
    `Trend_IFA Syrup`,
    Pink_IFA_Tablets_Start,
    Pink_IFA_Tablets_End,
    `Trend_Pink IFA Tablets`,
    Blue_IFA_Tablets_Start,
    Blue_IFA_Tablets_End,
    `Trend_Blue IFA Tablets`
  )

# Save combined_df to a CSV file
write.csv(combined_df, file = "combined_data.csv", row.names = FALSE)

# Save final_summary to a CSV file
write.csv(final_summary, file = "final_summary.csv", row.names = FALSE)
