source("api/dataRequest.R")
local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/'
vars_of_interest <- c('mooney_vjp',
                      'sws',
                      'demo',
                      'sips_p')

for (j in vars_of_interest){
  dataRequest(j)
}

save(list = paste0(vars_of_interest,'_clean'),
     file = paste0(local_dir,'CAPR SWS and Mooney/RData/pulled_data.rda'))
