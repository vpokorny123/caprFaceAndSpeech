local_dir <- 
  '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/'
load(paste0(local_dir,'CAPR SWS and Mooney/RData/merged_data_sparse.RData'))
source("~/Desktop/R_functions/funcs.R")

chr_only = 0
if (chr_only ==1){
  merged_df<-merged_df[merged_df$phenotype_four_groups=='chr',]
}

#-------------------------------------------------------------------------------

attach(merged_df)
#is there a block by group interaction
long_df <- tidyr::pivot_longer(merged_df, 
                               cols =c(total_yes_responses_pre, 
                                       total_yes_responses_post),
                               names_to ='pre_vs_post')
long_df$pre_vs_post<-as.factor(long_df$pre_vs_post)
long_df$src_subject_id<- as.factor(long_df$src_subject_id)

#check for interaction of pre vs post
res<-afex::aov_ez(between = 'phenotype_final',
             within = 'pre_vs_post',
             dv = 'value',
             id = 'src_subject_id',
             data = long_df)
pub_ready_stats(res)

#what about a symptom by block interaction
res<-afex::aov_ez(within = 'pre_vs_post',
                  dv = 'value',
                  covariate = 'SIPS Total Positive',
                  id = 'src_subject_id',
                  factorize = FALSE,
                  data = long_df)

pub_ready_stats(res)

library(lme4)
library(lmerTest)
pub_ready_stats(lmer(value ~ `SIPS Total Positive`*pre_vs_post + (1 | src_subject_id),
     data = long_df, REML = TRUE))

