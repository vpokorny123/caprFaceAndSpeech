# Load packages and functions
source("api/dataMerge.R")
local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/'
variables_to_merge<-c('mooney_vjp_clean','sws_clean',
                      'demo_clean','sips_p_clean')
load(paste0(local_dir,'CAPR SWS and Mooney/RData/pulled_data.rda'))

#how many folks have NAs for phenotype
problem_subs_sips_p<- sips_p_clean$src_subject_id[is.na(sips_p_clean$phenotype_four_groups)]

problem_subs<-sws_clean$src_subject_id[!sws_clean$src_subject_id %in% 
                                          sips_p_clean$src_subject_id]

#create list of all dataframes to merge and make sure all dataframes are good to be 
# merged
dfs_to_merge = NULL
for (name in variables_to_merge) {
  df<-base::get(name)
  #I think merge does better when src_subject_id is not a factor
  df$src_subject_id<-as.numeric(as.character(df$src_subject_id))
  
  #there are discrepancies in subjectkey, sex, phenotype,etc. between dataframes
  #long term solution: fix these discrepancies (this would be a good job
  #for an RA or undergrad). short term solution: drop these columns for all,
  #but one
  if (name != 'sips_p_clean'){
    df <- dplyr::select(df, c(-sex,
                              -phenotype,
                              -subjectkey,
                              -site,
                              -interview_age,
                              -interview_date))
  }
  dfs_to_merge[[name]]<-df
}
#finally we can merge
merged_df<-dataMerge(dfs_to_merge, by = c("src_subject_id", "visit"))
merged_df<-merged_df %>%
  dplyr::relocate(
    src_subject_id,visit,phenotype_four_groups, phenotype)

#look at phenotype confusion
#this checks how well phenotype lines up with sips_chr_status

problem_phenos<-merged_df[!is.na(merged_df$phenotype) & 
                            merged_df$phenotype== 'chr' &
                            merged_df$sips_chr_status== 'notchr',]
problem_subs<- merged_df$src_subject_id[is.na(merged_df$phenotype_four_groups)]

#some folks don't have a visit variable?
subs_with_no_visit <- merged_df$src_subject_id[is.na(merged_df$visit)]

#drop them
merged_df <- merged_df[!is.na(merged_df$visit),]

# Let's check for duplicate folks at baseline -- duplicates can result from merging
# when a subject has different values across dataframes being merged
merged_df_baseline   <- merged_df[merged_df$visit=='bl',]
merged_df_baseline   <- merged_df_baseline[order(merged_df_baseline$src_subject_id),]
duplicated_idx       <- which(duplicated(merged_df_baseline$src_subject_id))
merged_df_duplicates <- merged_df_baseline[c(duplicated_idx),]
print(paste0(nrow(merged_df_duplicates)," duplicated subjects that need to be dealt with"))

#order the visit factor
merged_df$visit <- factor(merged_df$visit, levels = c('bl','12m','24m'))

#we only need baseline values
merged_df<- merged_df[merged_df$visit=='bl',]

#-------------------------------------------------------------------------------
#save out
save(merged_df,
     file = paste0(local_dir,'CAPR SWS and Mooney/RData/merged_data.RData'))
