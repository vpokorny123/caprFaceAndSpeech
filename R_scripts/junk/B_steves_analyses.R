local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/'
load(paste0(local_dir,'CAPR SWS and Mooney/RData/merged_data_sparse.RData'))
wide_df<-tidyr::pivot_wider(merged_df, names_from = 'visit', 
                            values_from = -c('src_subject_id','visit'))
#steve said this: I would like to know in advance whether the % accuracy for age/gender in the MFT inverted condition is as related to (or more related to) positive symptoms and SWS score compared to the cruder index of number of faces reported on the MFT.

#so first we will correlate % accuracy with positive symptoms
#gender correct
pub_ready_stats(cor.test(wide_df$inverted_gender_correct_bl, wide_df$sips_p_total_bl))
scatterplot(wide_df$inverted_gender_correct_bl, wide_df$sips_p_total_bl)

#age correct
pub_ready_stats(cor.test(wide_df$inverted_age_correct_bl, wide_df$sips_p_total_bl))
scatterplot(wide_df$inverted_age_correct_bl, wide_df$sips_p_total_bl)

#let's combine into an "accuracy" variable
wide_df$inverted_age_and_gender_acc <- wide_df$inverted_age_correct_bl + wide_df$inverted_age_correct_bl
pub_ready_stats(cor.test(wide_df$inverted_age_and_gender_acc , wide_df$sips_p_total_bl))
scatterplot(wide_df$inverted_age_correct_bl + wide_df$inverted_age_correct_bl, wide_df$sips_p_total_bl)

pub_ready_stats(cor.test(wide_df$inverted_age_and_gender_acc, wide_df$c_all_bl))
pub_ready_stats(cor.test(wide_df$inverted_age_correct_bl, wide_df$c_all_bl))
pub_ready_stats(cor.test(wide_df$inverted_gender_correct_bl, wide_df$c_all_bl))
scatterplot(wide_df$inverted_age_and_gender_acc, wide_df$c_all_bl) 

# then steve said:
#Maybe a more important question is, in a hierarchical regression analysis, would adding the % correct on age/gender score from the MFT in a second step account for variance in positive symptoms over and above that accounted for by number of faces reported in the inverted condition alone (i.e., in the first step)

pub_ready_stats(lm(sips_p_total_bl ~ inverted_age_and_gender_acc +
                             inverted_faces_reported_bl,wide_df))

summary(lm(sips_p_total_bl ~ inverted_age_correct_bl + 
                             inverted_faces_reported_bl))
pub_ready_stats(cor.test(wide_df$inverted_age_and_gender_acc,
                         wide_df$inverted_faces_reported_bl))

scatterplot(wide_df$inverted_age_and_gender_acc,wide_df$inverted_faces_reported_bl)


hist(wide_df$inverted_age_and_gender_acc)
hist(wide_df$inverted_faces_reported_bl)

