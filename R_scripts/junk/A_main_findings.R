source("api/dataRequest.R")
source("api/dataMerge.R")
source("api/getRedcap.R")
local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/'
load(paste0(local_dir,'CAPR SWS and Mooney/RData/merged_data_sparse.RData'))
merged_df$phenotype_final<- factor(merged_df$phenotype_four_groups,
                                   levels = c('chr','hsc_sub','hsc_other','hc'))
merged_df$phenotype_final<- dplyr::recode(merged_df$phenotype_final,
                                          chr = 'CHR',
                                          hsc_sub = 'PLE',
                                          hsc_other = 'CLN',
                                          hc = 'CON'
                                          )

chr_only = 0
if (chr_only ==1){
  merged_df<-merged_df[merged_df$phenotype_four_groups=='chr',]
}

#-------------------------------------------------------------------------------

wide_df<-tidyr::pivot_wider(merged_df, names_from = 'visit', 
                            values_from = -c('src_subject_id','visit'))
attach(wide_df)
`Sine Wave Speech C Recognition` <- -1*c_all_bl
`Mooney Faces Inverted Reports` <- inverted_faces_reported_bl
`Sine Wave Speech Reports`  <- total_yes_responses_all_bl


scatterplot(`Sine Wave Speech C Recognition`,`Speech Reports`)
#so first we show that c and inverted faces rates are correlated
scatterplot(`Mooney Faces Inverted Reports`,`Sine Wave Speech Reports`)
#I wonder if we can predict sips_p symptoms from each individually

scatterplot(`SIPS Total Positive` , `Sine Wave Speech Reports`)
scatterplot(`SIPS Total Positive`, `Mooney Faces Inverted Reports`)
cor.test(`SIPS Total Positive`, `Mooney Faces Inverted Reports`, method = 'spearman', exact = FALSE)

#now let's do composite measure
X <- wide_df[c('total_yes_responses_all_bl','inverted_faces_reported_bl')]
`Composite Multisensory Priors Score`<-unifactorScores(X)
`SIPS Total Positive` <- wide_df$sips_p_total_bl
scatterplot(`SIPS Total Positive`,`Composite Multisensory Priors Score`)


#now simple sum score
composite_score_sum = (wide_df$inverted_faces_reported_bl + 
                         (-1 *wide_df$c_all_bl))/2 # have to flip sign
cor.test(wide_df$sips_p_total_bl, composite_score_sum)
scatterplot(wide_df$sips_p_total_bl, composite_score_sum)

#what about latent score for positive symptoms as well
X <- wide_df[c(paste0('p',seq(5),'_sev_bl'))]
composite_score_symps<-unifactorScores(X)
cor.test(composite_score_symps, `Composite Cross-Modal Score`)
scatterplot(composite_score_symps, `Composite Cross-Modal Score`)


#group difference in yeses sine wave speech?
pub_ready_stats(anova(lm(`Sine Wave Speech Reports` ~ phenotype_four_groups_bl)))
pub_ready_stats(anova(lm(inverted_faces_reported_bl ~ phenotype_four_groups_bl)))

#what about negative and disorganized symptoms?
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

