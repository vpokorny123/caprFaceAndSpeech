library(svglite)

local_dir <- 
  '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/'
load(paste0(local_dir,'CAPR SWS and Mooney/RData/merged_data_sparse.RData'))
source("~/Desktop/R_functions/funcs.R")
width = 4
height = 5
chr_only = 0
if (chr_only ==1){
  merged_df<-merged_df[merged_df$phenotype_four_groups=='chr',]
}

#-------------------------------------------------------------------------------

attach(merged_df)
#so first we show that c and inverted faces rates are correlated
svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/facesXSpeech.svg'), width = width, height = height)
scatterplot(`Inverted Face Reports`,`Speech Reports`)
dev.off()

svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/postiveXSpeech.svg'), width = width, height = height)
scatterplot(`SIPS Total Positive` , `Speech Reports`)
dev.off()

svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/postiveXFaces.svg'), width = width, height = height)
scatterplot(`SIPS Total Positive`, `Inverted Face Reports`)
dev.off()

#now let's do composite measure
X <- merged_df[c('total_yes_responses_all','inverted_faces_reported')]
`Composite Score`<-unifactorScores(X)
svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/positiveXComposite.svg'), width = width, height = height)
scatterplot(`SIPS Total Positive`,`Composite Score`)
dev.off()


width = 4
height = 5

#now let's do plots where we break down positive into each dimension
svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/p1XComposite.svg'),
        width = width, height = height)
scatterplot(`Unusual Thought Content/Delusional Ideas`,
            `Composite Score`, jitter_x = 1)
dev.off()

svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/p2XComposite.svg'),
        width = width, height = height)
scatterplot(`Suspiciousness/Persecutory Ideas`,
            `Composite Score`, jitter_x = 1)
dev.off()

svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/p3XComposite.svg'),
        width = width, height = height)
scatterplot(`Grandiose Ideas`,
            `Composite Score`, jitter_x = 1)
dev.off()

svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/p4XComposite.svg'),
        width = width, height = height)
scatterplot(`Perceptual Abnormalities/Hallucinations`,
            `Composite Score`, jitter_x = 1)
dev.off()

svglite(paste0(local_dir,'CAPR SWS and Mooney/svgs/p5XComposite.svg'),
        width = width, height = height)
scatterplot(`Disorganized Communication`,
            `Composite Score`, jitter_x = 1)
dev.off()


