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
#so first we look at distribution of proportion
block1_ratio<-merged_df$int_sum_pre/ merged_df$unint_sum_pre
vjp_hist(merged_df$int_sum_pre/ merged_df$unint_sum_pre)

#no group differences in this proportion
pub_ready_stats(anova(lm(block1_ratio ~ phenotype_four_groups)))
pub_ready_stats(t.test(`Composite Score` ~ phenotype_four_groups, 
                       data = merged_df,
                       subset = phenotype_four_groups == 'chr' |
                         phenotype_four_groups == 'hc',
                       var.equal = TRUE))

# no correlation with positive symptoms
scatterplot(block1_ratio, `SIPS Total Positive`)

#not associated with task performance?
scatterplot(block1_ratio, `Speech Reports`)

#association between task performance and positive symptoms was 
#unaffected when controlling for this proportion
pub_ready_stats(lm(`SIPS Total Positive` ~ block1_ratio + `Speech Reports`))




