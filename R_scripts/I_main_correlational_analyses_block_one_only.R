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
#so first we show that c and inverted faces rates are correlated
scatterplot(`Inverted Face Reports`,`Speech Reports Unprimed`)

#then show relationship between positive symptoms and tasks
scatterplot(`SIPS Total Positive` , `Speech Reports Unprimed`)
scatterplot(`SIPS Total Positive`, `Inverted Face Reports`)

# then show correlations hold for Spearman
cor.test(`SIPS Total Positive`, `Inverted Face Reports`, method = 'spearman', exact = FALSE)
cor.test(`SIPS Total Positive`, `Speech Reports`, method = 'spearman', exact = FALSE)

#look at composite measures
scatterplot(`SIPS Total Positive`,`Composite Score Unprimed`)

scatterplot(`SIPS Total Positive`, `Composite Score Sum Unprimed`)

pub_ready_stats(lm(`SIPS Total Positive` ~ `Speech Reports Unprimed` + 
     `Inverted Gender and Age Percent Correct`,
   data = merged_df))

scatterplot(`Unusual Thought Content/Delusional Ideas`,
            `Composite Score Unprimed`, jitter_x = 1)
scatterplot(`Suspiciousness/Persecutory Ideas`,
            `Composite Score Unprimed`,jitter_x = 1)
scatterplot(`Grandiose Ideas`,
            `Composite Score Unprimed`,jitter_x = 1)
scatterplot(`Perceptual Abnormalities/Hallucinations`,
            `Composite Score Unprimed`, jitter_x = 1)
scatterplot(`Disorganized Communication`,
            `Composite Score Unprimed`, jitter_x = 1)

pub_ready_stats(lm(`Composite Score Unprimed` ~ 
             `Perceptual Abnormalities/Hallucinations` + 
             `Unusual Thought Content/Delusional Ideas` + 
             `Suspiciousness/Persecutory Ideas` + 
             `Grandiose Ideas` + 
             `Disorganized Communication`))




