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
scatterplot(`Inverted Face Reports`,`Speech Reports`)

scatterplot(`Inverted Face Reports`,total_yes_responses_pre)

#then show relationship between positive symptoms and tasks
scatterplot(`SIPS Total Positive` , `Speech Reports`)
scatterplot(`SIPS Total Positive`, `Inverted Face Reports`)

# then show correlation holds when not assuming 
cor.test(`SIPS Total Positive`, `Inverted Face Reports`, method = 'spearman', exact = FALSE)
cor.test(`SIPS Total Positive`, `Speech Reports`, method = 'spearman', exact = FALSE)

#look at composite measures
scatterplot(`SIPS Total Positive`,`Composite Score`)

scatterplot(`SIPS Total Positive`, `Composite Score Sum`)


#now let's do response bias analyses
scatterplot(`Inverted Gender and Age Correct`, `Inverted Face Reports`)

#percentage removes built-in dependency number face reports and number of corrects
scatterplot(`Inverted Gender and Age Percent Correct`, `Inverted Face Reports`)

#nice so association remains highly significant after controlling for possible
#response bias
pub_ready_stats(lm(`SIPS Total Positive` ~ `Inverted Face Reports` + 
     `Inverted Gender and Age Percent Correct`,
   data = merged_df))

pub_ready_stats(lm(`SIPS Total Positive` ~ `Speech Reports` + 
                     `Inverted Gender and Age Percent Correct`,
                   data = merged_df))

#reviewer wants to know if SIPS Total Positive correlates with Gender and Age percent
scatterplot(`Inverted Gender and Age Percent Correct`, `SIPS Total Positive`)


scatterplot(`Unusual Thought Content/Delusional Ideas`,
            `Composite Score`, jitter_x = 1)
scatterplot(`Suspiciousness/Persecutory Ideas`,
            `Composite Score`,jitter_x = 1)
scatterplot(`Grandiose Ideas`,
            `Composite Score`,jitter_x = 1)
scatterplot(`Perceptual Abnormalities/Hallucinations`,
            `Composite Score`, jitter_x = 1)
scatterplot(`Disorganized Communication`,
            `Composite Score`, jitter_x = 1)

reg_table<-pub_ready_reg_table(lm(`Composite Score` ~ 
             `Perceptual Abnormalities/Hallucinations` + 
             `Unusual Thought Content/Delusional Ideas` + 
             `Suspiciousness/Persecutory Ideas` + 
             `Grandiose Ideas` + 
             `Disorganized Communication`))
write.csv(reg_table, paste0(main_dir,'CAPR SWS and Mooney/csvs/table_2_raw.csv'))

#steve asked whether: there is a correlation between the number of MFT responses
#on which correct age/sex category responses were given and number of trials 
#where speech was reported on the speech perception test.

scatterplot(`Inverted Gender and Age Correct`, `Speech Reports`)
scatterplot(`Inverted Gender and Age Correct`, `Inverted Face Reports`)

