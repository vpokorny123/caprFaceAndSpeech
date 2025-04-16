library(dplyr)
main_dir <- 
  '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/'
load(paste0(main_dir,'CAPR SWS and Mooney/RData/merged_data_sparse.RData'))
source("~/Desktop/R_functions/funcs.R")
df <- merged_df

#set grouping variable name
grouping_var = 'phenotype_final'

# create demographic variable list: first value is name of the variable, second 
#value is the "pretty" name to be printed, third value sets the 
#variable as nominal (TRUE) or not (FALSE)
#fourth optional allows you to specify median instead of mean (e.g., for income)
demo_vars <- list(
  c('demo_age_in_years', 'Age',FALSE),
  c('demo_biological_sex','Sex',TRUE),
  c('demo_race','Race',TRUE),
  c('demo_hispanic','Hispanic',TRUE),
  c('demo_school_type','Education',TRUE),
  c('demo_household_income','Household Income',FALSE,'median'))

demo_df <- vjp_build_demographics_table(df, demo_vars, grouping_var)

write.csv(demo_df, paste0(main_dir,'CAPR SWS and Mooney/csvs/table_raw.csv'))
