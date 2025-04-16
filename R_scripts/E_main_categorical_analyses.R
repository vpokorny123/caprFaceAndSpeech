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

pub_ready_stats(anova(lm(`Composite Score` ~ phenotype_four_groups)))

pub_ready_stats(lm(`Composite Score` ~ phenotype_four_groups + `SIPS Total Positive`))

anova_test(`Composite Score` ~ phenotype_four_groups + `SIPS Total Positive`,data = merged_df)

pub_ready_stats(lm(`Composite Score` ~ phenotype_four_groups,data = merged_df))
pub_ready_stats(t.test(`Composite Score` ~ phenotype_four_groups, 
                       data = merged_df,
                       subset = phenotype_four_groups == 'chr' |
                         phenotype_four_groups == 'hc',
                       var.equal = TRUE))

pub_ready_stats(t.test(`Composite Score` ~ phenotype_four_groups, 
                       data = merged_df,
                       subset = phenotype_four_groups == 'chr' |
                         phenotype_four_groups == 'hsc_other',
                       var.equal = TRUE))

pub_ready_stats(t.test(`Composite Score` ~ phenotype_four_groups, 
                       data = merged_df,
                       subset = phenotype_four_groups == 'chr' |
                         phenotype_four_groups == 'hsc_sub',
                       var.equal = TRUE))


pub_ready_stats(anova(lm(`Speech Reports` ~ phenotype_four_groups)))
pub_ready_stats(anova(lm(inverted_faces_reported ~ phenotype_four_groups)))


pub_ready_stats(t.test(`Speech Reports` ~ phenotype_four_groups, data = merged_df,
       subset = phenotype_four_groups == 'chr' | phenotype_four_groups == 'hc',
       var.equal = TRUE))
pub_ready_stats(t.test(inverted_faces_reported ~ phenotype_four_groups, data = merged_df,
                       subset = phenotype_four_groups == 'chr' |
                         phenotype_four_groups == 'hc',
                       var.equal = TRUE))

pub_ready_stats(t.test(`Composite Score` ~ phenotype_four_groups, data = merged_df,
                       subset = phenotype_four_groups == 'chr' |
                         phenotype_four_groups == 'hc',
                       var.equal = TRUE))


#plot group differences on main variables
plot_groups(merged_df, 'phenotype_final', 'total_yes_responses_all',
            xlab = NULL, ylab = "Speech Reports",condition = NULL)
plot_groups(merged_df, 'phenotype_final', 'inverted_faces_reported', 
            xlab = NULL, ylab = "Face Reports",condition = NULL)

#plot group differences on pre only
plot_groups(merged_df, 'phenotype_final', 'total_yes_responses_pre',
            xlab = NULL, ylab = "Face Reports", condition = NULL)
pub_ready_stats(anova(lm(total_yes_responses_pre ~ phenotype_final)))

plot_groups(merged_df, 'phenotype_final', 'total_yes_responses_post', xlab = NULL,
            ylab = "Face Reports", condition = NULL)
pub_ready_stats(anova(lm(total_yes_responses_post ~ phenotype_final)))

long_df <- tidyr::pivot_longer(merged_df, 
                               cols =c(total_yes_responses_pre, 
                                       total_yes_responses_post),
                               names_to ='pre_vs_post')

#check for interaction of pre vs post
afex::aov_ez(between = 'phenotype_final',
             within = 'pre_vs_post',
             dv = 'value',
             id = 'src_subject_id',
             data = long_df)

long_df <- tidyr::pivot_longer(merged_df, 
                               cols =c(total_yes_responses_intell, 
                                       total_yes_responses_unintell),
                               names_to ='condition')

#check for interaction of intelligible vs unintelligible
afex::aov_ez(between = 'phenotype_final',
             within = 'condition',
             dv = 'value',
             id = 'src_subject_id',
             data = long_df)
plot_groups(long_df, 'phenotype_final', 'value', xlab = NULL,
            ylab = "Face Reports", condition = 'condition')

#check block by condition
afex::aov_ez(between = 'phenotype_final',
             within = 'condition',
             dv = 'value',
             id = 'src_subject_id',
             data = long_df)

