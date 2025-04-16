local_dir <- 
  '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/'
load(paste0(local_dir,'CAPR SWS and Mooney/RData/merged_data_sparse.RData'))

chr_only = 0
if (chr_only ==1){
  merged_df<-merged_df[merged_df$phenotype_four_groups=='chr',]
}

#-------------------------------------------------------------------------------

attach(merged_df)
#so first we show that c and inverted faces rates are correlated
scatterplot(`Inverted Face Reports`,`Speech Reports`)


scatterplot(`SIPS Total Positive` , `Speech Reports`)
scatterplot(`SIPS Total Positive`, `Inverted Face Reports`)
cor.test(`SIPS Total Positive`, `Inverted Face Reports`, method = 'spearman', exact = FALSE)
cor.test(`SIPS Total Positive`, `Speech Reports`, method = 'spearman', exact = FALSE)

#now let's do composite measure
X <- merged_df[c('total_yes_responses_all','inverted_faces_reported')]
`Composite Multisensory Priors Score`<-unifactorScores(X)
scatterplot(`SIPS Total Positive`,`Composite Multisensory Priors Score`)

#now simple sum score
composite_score_sum = (`Speech Reports` + `Inverted Face Reports`) /2 # have to flip sign
scatterplot(`SIPS Total Positive`, composite_score_sum)

#what about latent score for positive symptoms as well
# really we should go full blown SEM with lavaan for this



#group difference in yeses sine wave speech?
pub_ready_stats(anova(lm(`Sine Wave Speech Reports` ~ phenotype_four_groups_bl)))
pub_ready_stats(anova(lm(inverted_faces_reported_bl ~ phenotype_four_groups_bl)))

plot_groups(wide_df, 'phenotype_final_bl', 'total_yes_responses_all_bl', xlab = NULL, ylab = "Speech Reports",
         condition = NULL)

plot_groups(wide_df, 'phenotype_final_bl', 'inverted_faces_reported_bl', xlab = NULL, ylab = "Face Reports",
            condition = NULL)

plot_groups(wide_df, 'phenotype_final_bl', 'total_yes_responses_pre_bl', xlab = NULL, ylab = "Face Reports",
            condition = NULL)
pub_ready_stats(anova(lm(total_yes_responses_pre_bl ~ phenotype_four_groups_bl)))

plot_groups(wide_df, 'phenotype_final_bl', 'total_yes_responses_post_bl', xlab = NULL, ylab = "Face Reports",
            condition = NULL)
pub_ready_stats(anova(lm(total_yes_responses_post_bl ~ phenotype_four_groups_bl)))

long_df <- tidyr::pivot_longer(wide_df, 
                               cols =c(total_yes_responses_pre_bl, 
                                       total_yes_responses_post_bl),
                               names_to ='condition')

afex::aov_ez(between = 'phenotype_final_bl',
             within = 'condition',
             dv = 'value',
             id = 'src_subject_id',
             data = long_df)

plot_groups(wide_df, 'phenotype_final_bl', 'unint_sum_pre_bl', xlab = NULL, ylab = "Face Reports",
            condition = NULL)
pub_ready_stats(anova(lm(total_yes_responses_pre_bl ~ int_sum_post_bl + phenotype_final_bl )))
pub_ready_stats(anova(lm(unint_sum_pre_bl ~  phenotype_final_bl )))
pub_ready_stats(anova(lm(int_sum_post_bl ~  phenotype_final_bl )))
pub_ready_stats(anova(lm(int_sum_post_bl ~  phenotype_final_bl )))

cor.test(int_sum_pre_bl,unint_sum_pre_bl)
cor.test(int_sum_post_bl,unint_sum_pre_bl)
cor.test(unint_sum_post_bl,unint_sum_pre_bl)
head(unint_sum_pre_bl)
head(int_sum_post_bl)
head(unint_sum_post_bl)

