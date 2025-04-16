library(dplyr)
library(rstatix)
library(tidyr)
pub_ready_reg_table <- function(x){
  res_sum      <- summary(x)
  coefficients <- round(res_sum$coefficients,3)
  ps <- unname(coefficients[,'Pr(>|t|)'])
  ps<-ifelse(ps == 0, "<.001",as.character(ps))
  betas<-unname(coefficients[,'Estimate'])
  betas<-ifelse(betas== 0, "<.001",as.character(betas))
  coefficients[,'Pr(>|t|)'] <- ps
  coefficients[,'Estimate'] <- betas
  coef_names<-rownames(coefficients)
  coef_name<-gsub('`','',coef_names)
  rownames(coefficients)<-coef_name
  library(sensemakr)
  #browser()
  r2_coefs<-round(partial_r2(x),3)
  r2_coefs<-ifelse(r2_coefs== 0, "<.001",as.character(r2_coefs))
  reg_table<-cbind(coefficients,r2_coefs)
  #let's add full model R squared and adjusted R squared
  rsquared<-round(res_sum$r.squared,3)
  rsquared_adj<-round(res_sum$adj.r.squared,3)
  fstat<-res_sum$fstatistic['value']
  df1<-res_sum$fstatistic['numdf']
  df2<-res_sum$fstatistic['dendf']
  rsquared_pval<- round(pf(fstat, df1, df2, lower.tail = FALSE),3)
  ifelse(rsquared_pval==0, rsquared_pval<-"<.001",rsquared_pval<-paste0('=',rsquared_pval))
  ` ` <- c(paste0('Full Model R^2 = ',rsquared, '; ',
                  'Adjusted R^2 = ', rsquared_adj,'; ',
                  'F(',df1,',',df2,')=',round(fstat,2),
                  ', p',rsquared_pval),"","","","")
  pub_ready_reg_table<-rbind(reg_table, ` `)
  return(pub_ready_reg_table)
}

vjp_rectangle_corrplot <- function(row_variables, column_variables){
  #NOTE if you're having trouble running this. It may be because you have
  #a single variables in row_variables. this function CAN handle this, but
  #the input needs to be df[rows_variables], NOT df[,row_variables]. The
  #latter way removes the name from the vector which causes problems later
  #on
  library(corrplot)
  cor_res <- psych::corr.test(x = row_variables, y =column_variables, 
                              use = 'pairwise', adjust = 'fdr')
  cor_mat <- cor_res$r
  p_mat <- cor_res$p
  #corrplot gets mad if none of the p-values are significant AND we are doing   
  #a rectangle corr plot. so let's avoid this with an if statement
  if (any(p_mat<.05)){
    corrplot(cor_mat, p.mat = p_mat,
             method = c('circle'),
             tl.cex = .8, number.cex = .65,
             sig.level = .05, diag = TRUE,
             insig = 'label_sig', pch.cex = 1.3,
             pch.col = 'black', tl.col = 'black',
             col = rev(COL2('RdBu', 100))) 
  } else {
    corrplot(cor_mat,
             method = c('circle'),
             tl.cex = .6, number.cex = .65,
             sig.level = .05, diag = TRUE,
             insig = 'label_sig', pch.cex = 1,
             pch.col = 'black',tl.col = 'black',
             col = rev(COL2('RdBu', 100))) 
  }
  
}

vjp_corrplot <- function(df,p){
  library(corrplot)
  library(psych)
  #cor_mat <- cor(df, use = "pairwise.complete.obs")
  cor_res <- psych::corr.test(df, adjust = 'fdr')
  cor_mat <- cor_res$r
  p_mat <- cor_res$p
  corrplot(cor_mat, p.mat = p_mat,
           method = c('circle'),type = "upper",
           tl.cex = .6, number.cex = .65,
           sig.level = .05, diag = FALSE,
           insig = 'label_sig', pch.cex = 1,
           pch.col = 'black',
           col = rev(COL2('RdBu', 100)))
  
}
#checking 
build_lm_formula <-function(x, interactions = FALSE){
  for (j in seq(x)){
    variable = x[j]
    if (j == 1){
      lm_formula = paste0('`',variable,'` ~ `')
    } else if (j == 2) {
      lm_formula = paste0(lm_formula, variable, '`')
    }else if (j>2 & interactions == TRUE){
      lm_formula = paste0(lm_formula,' * `',variable, '`')
    }else if (j>2 & interactions ==FALSE){
      lm_formula = paste0(lm_formula,' + `',variable, '`')
    }
  }
  return(formula(lm_formula))
}
impute_lm <- function(y,x, data){
  model_variables <- c(y, x)
  
  #just constructing the lm_formula
  for (j in seq(model_variables)){
    variable = model_variables[j]
    if (j == 1){
      lm_formula = paste0(variable,' ~ ')
    } else if (j == 2) {
      lm_formula = paste0(lm_formula,variable)
    }else {
      lm_formula = paste0(lm_formula,' + ',variable)
    }
  }
  # for (j in seq(model_variables)){
  #   variable = model_variables[j]
  #   if (j == 1){
  #     lm_formula = paste0('scale(',variable,') ~ ')
  #   } else if (j == 2) {
  #     lm_formula = paste0(lm_formula,'scale(',variable,')')
  #   }else {
  #     lm_formula = paste0(lm_formula,' + scale(',variable,')')
  #   }
  # }
  #lm_formula<-build_lm_formula(model_variables)
  
  #now we create an impute object
  imputed_data <- data %>% 
    select(all_of(model_variables)) %>%
    mice(pred = quickpred(.), seed = 123, m = 10) 
  
  #now we can fit 5 linear regressions and pool across them 
  fit<-with(imputed_data, lm(formula = formula(lm_formula)))
  results<-summary(pool(fit))
  rsquared_adj<-round(pool.r.squared(fit, adjusted = TRUE)[1,1],3)
  rsquared<-round(pool.r.squared(fit, adjusted = FALSE)[1,1],3)
  round_ps<-round(results$p.value,3)
  #browser()
  cleaner_results<-cbind(results, round_ps)
  final_results <- list(cleaner_results, 
                        rsquared_adj, 
                        rsquared)
  names(final_results) <- c("main_results","rsquared_adj","rsquared")
  return(final_results)
  
}


vjp_hist <- function(x, label = NULL){
  hist(x, main = NULL, breaks = 'Scott')
  mu<-round(mean(x, na.rm = TRUE),3)
  sigma<-round(sd(x, na.rm = TRUE),3)
  mini<- round(min(x, na.rm = TRUE),3)
  maxi<- round(max(x, na.rm = TRUE),3)
  mtext(paste0('m = ', mu,', sd = ',sigma,', min = ',mini,', max = ',maxi))
  if (!is.null(label)){
    title(label)
  }
}

run.cocor.dep.groups.overlap<- function(j,k,h){
  library(cocor)
  r.jk = cor.test(j,k)$estimate
  r.jh = cor.test(j,h)$estimate
  r.hk = cor.test(h,k)$estimate
  #check n is equal across vectors
  if (length(unique(sapply(list(j,k,h),length))) == 1){
    n = length(j)
  }
  
  cocor.dep.groups.overlap(r.jk,r.jh,r.hk, n)
  
}
unifactorScores <- function(X){
  library(fungible)
  complete_cases <- complete.cases(X)
  na_idx <- which(!complete.cases(X))
  X_complete = X[complete_cases,]
  #browser()
  fac_res<-faMain(X = X_complete, numFactors = 1)
  scores<-faScores(X =X_complete, faMainObject = fac_res, Method = 'Thurstone')
  # FSI can be computed from fac res as S'RS where S is the structure (i.e. pattern or loadings %*% phi)
  FSI <- fac_res$facIndeterminacy
  #put NAs back in
  if (sum(complete_cases) != nrow(X)){
    final_scores <- insertValuesAtIdx(scores$fscores,rep(NA,sum(!complete_cases)),na_idx)
    final_scores <- list(final_scores, fac_res)
  } else {
    final_scores <- list(scores$fscores, fac_res)
  } 
  names(final_scores) <- c('scores','fa.res')
  return(final_scores)
}

insertValuesAtIdx <- function(vec,values,idx)
{
  res<-vector(mode=mode(vec),length = length(vec)+length(idx))
  res[-idx]<-vec
  res[idx]<-values
  return(res)
}

#this takes multiple X variables and a single y vector, converts the raw X variables
#to a single vector of factor scores and then correlates fscores with y.
fscore_cor <- function(X, y){ 
  complete_cases <- complete.cases(X)
  X_complete = X[complete_cases]
  fac_res<-faMain(X = X_complete, numFactors = 1)
  scores<-faScores(X =X_complete, faMainObject = fac_res, Method = 'Thurstone')
  result<-cor.test(scores$fscores,y[complete_cases])
  return(result)
}
scatterplot<- function(x,y,xlab=NULL, ylab= NULL, tail = NULL, jitter_x = NULL){
  if (is.null(xlab) & is.null(ylab)){
    xlab = deparse(substitute(x))
    ylab = deparse(substitute(y))
  }
  
  if (!is.null(tail)){
    res = cor.test(x, y, alternative = tail)
  } else {
    res = cor.test(x, y)
  }
  model<-lm(y ~ x)
  #browser()
  if (!is.null(jitter_x)){
    plot(jitter(x, factor = jitter_x), y, xlab = xlab, ylab = ylab, pch = 20)
  }
  else {
    plot(x, y, xlab = xlab, ylab = ylab, pch = 20)
  }
  abline(model,lwd = 2)
  mtext(pub_ready_stats(res))
  print(pub_ready_stats(res))
  
}

plot_groups <- function(df,group_var, dv , xlab = NULL, ylab = NULL,
                        condition = NULL,save_name = NULL){
  if (!is.null(condition)){
    # browser()
    summary_stats <- df %>% 
      group_by(!!as.name(group_var),!!as.name(condition)) %>%
      summarise(means = mean(!!as.name(dv),na.rm = TRUE),
                sd = sd(!!as.name(dv),na.rm = TRUE),
                n = n(),
                se = sd / sqrt(n),
                conf_int = se*2 )
    ggplot(data = summary_stats, aes(x = condition, 
                                     y = means, 
                                     group = !!as.name(group_var), 
                                     color = !!as.name(group_var))) +
      geom_line(position = position_dodge(0.1)) + 
      geom_pointrange(aes(ymin = means-conf_int, 
                          ymax = means+conf_int), position = position_dodge(0.1))+
      theme_bw() 
  }else{
    summary_stats <- df %>% 
      group_by(!!as.name(group_var)) %>%
      summarise(means = mean(!!as.name(dv),na.rm = TRUE),
                sd = sd(!!as.name(dv),na.rm = TRUE),
                n = n(),
                se = sd / sqrt(n),
                conf_int = se*2 )
    stats<-pub_ready_stats(aov(base::get(dv) ~ base::get(group_var), df))
    ggplot(data = df, aes(x = !!as.name(group_var), 
                          y = !!as.name(dv),
                          color = !!as.name(group_var))) +
      geom_point(position = position_jitter(width = 0.1),alpha = .5) + 
      #geom_line(position = position_dodge(0.1)) + 
      geom_pointrange(data = summary_stats, aes(y = means, ymin = means-conf_int, 
                                                ymax = means+conf_int,), color = 'black',
                      position = position_nudge(x = .2))+
      theme_bw() +
      ylab(ylab) + xlab(xlab) + 
      annotate('text',  x=Inf, y = Inf, label = stats, vjust=1.6, hjust=1.05)
    
    #let's get stats too
    
    
    
  }
  if (!is.null(save_name)){
    ggsave(save_name,width = 4,height = 4,dpi = 500)
  }
}

mediating_func <- function(x,m,y,control.value, treat.value, sims, data){
  data<-base::subset(data, select = c(x, m, y))
  #browser()
  data<-data[complete.cases(data),]
  mediator_model = lm(data[[m]] ~ data[[x]])
  summary(mediator_model)
  outcome_model = lm(data[[y]] ~ data[[m]] + data[[x]])
  summary(outcome_model)
  if (missing(sims)){
    sims = 5000
  }
  print(paste0('starting mediation with ', sims, ' simulations. If taking too long',
               ' set sims to something lower'))
  if (missing(control.value)) {
    result<-mediate(mediator_model, outcome_model, 
                    treat = 'data[[x]]',
                    mediator = 'data[[m]]',
                    sims = sims)
  } else {
    result<-mediate(mediator_model, outcome_model, 
                    treat = 'data[[x]]',
                    mediator = 'data[[m]]',
                    control.value = control.value,
                    treat.value = treat.value,
                    sims = sims)
  }
  return(result) 
}


pub_ready_stats<-function(x, one_tail = FALSE) {
  #this is for afex aov_ez
  #correction should either be none, hf or gg
  output = NULL
  if (class(x)[1]=="afex_aov") {
    anov_table<-x[["anova_table"]]
    pest <- afex::nice(x, 'pes')
    rownames(pest)<- pest$Effect
    for (j in rownames(anov_table)) {
      fstat = round(anov_table[j,"F"], digits = 2)
      pval = round(anov_table[j,"Pr(>F)"], digits = 3)
      ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
      df1 = round(anov_table[j,"num Df"],digits =2)
      df2 = round(anov_table[j,"den Df"],digits =2)
      p_eta <- pest[j,'pes']
      pub_ready = paste0('F(',df1,',',df2,')=',fstat,', p',pval,', ',
                         greekLetters::greeks("eta^2"), '=', p_eta)
      pub_ready = unname(cbind(j,pub_ready))
      output = unname(rbind(output,pub_ready))
    }
  }
  if (all(class(x)==c("psych","fa"))){
    #x$STATISTIC is what we want to report unless something funky's goin on
    chi<-round(x$STATISTIC,digits = 2)
    ifelse(round(x$PVAL,digits = 3)==0, pval<-"<.001",pval<-paste0('=',round(x$PVAL,digits = 3)))
    chi_rep<- paste0(greekLetters::greeks("chi^2"),'(',x$dof,')=',chi,', p',pval)
    x$CFI<-((x$null.chisq-x$null.dof)-
              (x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof)
    other_fits<-paste0(chi_rep,', TLI=',round(x$TLI,digits=2),
                       ', CFI=', round(x$CFI,digits=2),
                       ', RMSEA=', round(x$RMSEA,digits=2)[1])
    output = other_fits
  }
  if ("method" %in% names(x)){
    if (x$method == "Pearson's product-moment correlation") {
      #browser()
      library(MBESS)
      r = round(x$estimate,digits = 2)
      df = x$parameter
      pval = round(x$p.value,3)
      ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
      cis <- paste0(round(x$conf.int[1],2),',',round(x$conf.int[2],2))
      output<-paste0("r(",df,")=",r,", p",pval,', 95% CI [',cis,']')}
    
    if (grepl('t-test',x$method )) {
      t = unname(round(x$statistic,digits = 2))
      df = unname(round(x$parameter,digits = 2))
      pval = round(x$p.value,3)
      ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
      if (x$method == " Two Sample t-test"){
        d = round(t/ sqrt((df+2)),2)
        output<-paste0("t(",df,")=",t,", p",pval,", Cohen's d=",d)
      } else {
        output<-paste0("t(",df,")=",t,", p",pval)
      }
    }
    if (x$method == "Pearson's Chi-squared test"){
      pval = round(x$p.value,3)
      ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
      output<-paste0('X^2(',round(x$parameter,2),')=',round(x$statistic,2),
                     ', p', pval)
      
    }
    if (x$method == "Kruskal-Wallis rank sum test"){
      pval = round(x$p.value,3)
      ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
      output<-paste0('X^2(',round(x$parameter,2),')=',round(x$statistic,2),
                     ', p', pval)
    }
  }
  if (all(grepl("mediate",class(x)))) {
    library(greekLetters)
    digits = 2
    acme_b<-round(x$d.avg,digits = digits)
    ade_b = round(x$z.avg,digits = digits)
    while (acme_b == 0){
      digits = digits+1
      acme_b = round(x$d.avg,digits = digits)
    }
    while (ade_b == 0){
      digits = digits+1
      ade_b = round(x$z.avg,digits = digits)
    }
    
    acme_pval <- round(x$d.avg.p,digits = 3)
    ade_pval  <- round(x$z.avg.p,digits = 3)
    
    ifelse(acme_pval==0, 
           acme_pval<-"<.001",
           acme_pval<-paste0('=',acme_pval))
    ifelse(ade_pval==0, 
           ade_pval<-"<.001",
           ade_pval<-paste0('=',ade_pval))
    #browser()
    acme_ci<-round(x$d.avg.ci ,digits = digits)
    ade_ci<-round(x$z.avg.ci ,digits = digits)
    while (acme_ci[1] == 0 & acme_ci[2] == 0){
      digits = digits+1
      acme_ci<-round(x$d0.ci,digits = digits)
    }
    while (ade_ci[1] == 0 & ade_ci[2] == 0){
      digits = digits+1
      ade_ci<-round(x$z.avg.ci,digits = digits)
    }
    
    acme<-paste0("ACME ", greeks("beta"),"=",acme_b,", 95% CI [",acme_ci[1],", ",
                 acme_ci[2],"], p",acme_pval)
    ade <- paste0("ADE ", greeks("beta"),"=",ade_b,
                  ", 95% CI [",
                  ade_ci[1],", ",ade_ci[2],"], p",ade_pval)
    acme_output<-cbind('mediation',acme)
    ade_output <-cbind('direct effect',ade)
    model_m<-x$model.m
    x_to_m <- pub_ready_stats(model_m)
    x_to_m_output<- cbind('x to m',x_to_m[2,2])
    model_y<-x$model.y['model']
    model_data<-model_y$model
    m_to_y <- pub_ready_stats(lm(`data[[y]]` ~ `data[[m]]`, model_data))
    m_to_y_output <- cbind('m to y',m_to_y[2,2])
    
    output<-rbind(acme_output,ade_output, x_to_m_output,m_to_y_output)
  }
  if (length(class(x))==2){
    if (all(class(x) %in% c('aov','lm'))){
      res_sum<-summary(x)[[1]]
      pval = round(res_sum$`Pr(>F)`[1],3)
      ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
      output <- paste0('F(',res_sum$Df[1],',',res_sum$Df[2],')=',
                       round(res_sum$`F value`[1],2), 
                       ', p',pval)
    }
  }
  if (all(class(x) %in% c('anova','data.frame'))){
    res_sum<-x
    pval = round(res_sum$`Pr(>F)`[1],3)
    ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
    output <- paste0('F(',res_sum$Df[1],',',res_sum$Df[2],')=',
                     round(res_sum$`F value`[1],2), 
                     ', p',pval)
  }
  if (length(class(x))==1){
    if (class(x)=='lm'){
      #browser
      res_sum<-summary(x)$coefficients
      df<-summary(x)$df[2]
      if (nrow(res_sum)==2){
        r2_coefs = cor(unlist(x$model[1]),as.numeric(unlist(x$model[2])))^2
        r2_coefs = c(NA,r2_coefs)
        r2_text <- ', r^2 = '
      }
      if (nrow(res_sum)>2){
        library(sensemakr)
        r2_coefs<-partial_r2(x)
        r2_text <- ', partial r^2 = '
      }
      idx = 1
      for (j in rownames(res_sum)) {
        tstat = round(res_sum[j,"t value"], digits = 2)
        pval = round(res_sum[j,"Pr(>|t|)"], digits = 3)
        ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
        b = round(res_sum[j,"Estimate"],3)
        r2_coef = round(r2_coefs[idx],3)
        pub_ready = paste0('b=',b,', t(',df,')=',tstat,', p',pval,
                           r2_text,r2_coef)
        pub_ready = unname(cbind(j,pub_ready))
        output = unname(rbind(output,pub_ready))
        idx = idx+1
      }
    }
  }
  #browser()
  if (class(x)[1] == "lmerModLmerTest" ){
    res_sum<-summary(x)$coefficients
    df<- res_sum
    idx = 1
    for (j in rownames(res_sum)) {
      tstat = round(res_sum[j,"t value"], digits = 2)
      pval = round(res_sum[j,"Pr(>|t|)"], digits = 3)
      df = round(res_sum[j,"df"], digits = 3)
      ifelse(pval==0, pval<-"<.001",pval<-paste0('=',pval))
      b = round(res_sum[j,"Estimate"],3)
      #r2_coef = round(r2_coefs[idx],3)
      pub_ready = paste0('b=',b,', t(',df,')=',tstat,', p',pval)
      #r2_text,r2_coef)
      pub_ready = unname(cbind(j,pub_ready))
      output = unname(rbind(output,pub_ready))
      idx = idx+1
    }
  }
  if (class(x)[1] == "QuadTypeIndependenceTest" ){
    chi_stat = round(x@statistic@teststatistic,2)
    df = x@statistic@df 
    p_val = round(pchisq(chi_stat, df,lower.tail = FALSE),3)
    p_val = ifelse(p_val==0, '<.001',paste0('=',p_val))
    output <- paste0('X^2(',df,')=',chi_stat,', p', p_val)
  }
  
  return(output)
}

vjp_demographics_row <- function(df,main_var, grouping_var, pretty_name, 
                                 nominal, central_tendency){
  #define what type of measure of central tendency we want. Right now we are only
  #able to do mean (default) and median (by specifying in argument)
  #browser()
  if (is.na(central_tendency)){
    central_tendency = 'mean'
  }
  
  if (nominal == FALSE){
    if (central_tendency =='mean'){
      #browser()
      summary_df <- df %>% 
        group_by(!!as.name(grouping_var)) %>%
        get_summary_stats(!!as.name(main_var), type = "mean_sd" )
      summary_df = paste0(round(summary_df$mean,2) , ' (',round(summary_df$sd,2),')')
      res<-aov(lm(paste0(main_var, ' ~ ' ,grouping_var), data =df))
      res_sum<- summary(res)[[1]]
      if (res_sum$`Pr(>F)`[1] <.05) {
        post_hoc_res<-TukeyHSD(res)
        post_hoc_res<-data.frame(post_hoc_res[[grouping_var]])
        groups_compared<- rownames(post_hoc_res)
        post_hoc_p_vals<- post_hoc_res$p.adj
        sig_contrasts<-paste0(groups_compared[post_hoc_p_vals<.05],collapse = ', ')
        if (length(sig_contrasts) == 0) {sig_contrasts = ''}
      } else {
        sig_contrasts = '' 
      }
    }
    if (central_tendency =='median'){
      
      summary_df <- df %>% 
        group_by(!!as.name(grouping_var)) %>%
        get_summary_stats(!!as.name(main_var), type = "median" )
      summary_df = round(summary_df$median,2)
      res<-coin::median_test(formula = formula(paste0(main_var, ' ~ ' ,grouping_var)),
                             data =df)
      chi_stat<-res@statistic@teststatistic
      deg_free<-res@statistic@df
      p_val<-pchisq(chi_stat,deg_free, lower.tail = FALSE)
      
      if (p_val <.05) {
        groups_of_interest <- unique(df[[grouping_var]])
        group_pairs<-t(combn(groups_of_interest,2))
        df$Group <- df[grouping_var]
        sig_contrasts = NULL
        for (jj in seq(nrow(group_pairs))){
          group_one = as.character(group_pairs[jj,1])
          group_two = as.character(group_pairs[jj,2])
          df_subset = df[(df[grouping_var] ==group_one | df[grouping_var] ==group_two), ]
          post_hoc_res<- coin::median_test(
            formula = formula(paste0(main_var,' ~ ',grouping_var)), 
            data =df_subset)
          z_stat<-post_hoc_res@statistic@teststatistic
          p_val_lower<-pnorm(z_stat, lower.tail = TRUE)
          p_val_upper<-pnorm(z_stat, lower.tail = FALSE)
          p_val<-round(ifelse(p_val_upper<p_val_lower,p_val_upper,p_val_lower),3)
          #browser()
          if(p_val<.05){
            sig_contrasts = paste0(sig_contrasts,group_one,'-',group_two,'; ')
          }
        }
        if (length(sig_contrasts) == 0) {sig_contrasts = ''}
      } else {
        sig_contrasts = '' 
      }  
    }
    paste_stats <- pub_ready_stats(res)
    #browser()
    demo_row = cbind(pretty_name,format(t(summary_df),scientific = F),unname(paste_stats),unname(sig_contrasts))
  } else if (nominal == TRUE){
    main_factor<-as.factor(df[[main_var]])
    grouping_factor<-as.factor(df[[grouping_var]])
    counts <- table(main_factor,grouping_factor)
    group_marginals <- colSums(counts)
    res<- chisq.test(counts)
    paste_stats  = pub_ready_stats(res)
    summary_df <- sweep(counts, 2, group_marginals, `/`)
    summary_df[]<- paste0(round(summary_df*100,1),'%')
    padding<-rbind(rep('',nrow(summary_df)))
    row_names<- rownames(summary_df)
    summary_df<-cbind(unname(row_names),summary_df)
    #browser()
    if (res$p.value <.05) {
      post_hoc_res <- chisq.posthoc.test::chisq.posthoc.test(counts)
      post_hoc_res <- post_hoc_res[post_hoc_res$Value=='p values',]
      post_hoc_res_long<-pivot_longer(post_hoc_res, 
                                      cols = c('CHR','PLE','CLN','CON'),
                                      names_to = 'Group',values_to = 'pvals')
      post_hoc_res_long$pvals<-round(post_hoc_res_long$pvals,3)
      sig_vals <- data.frame(post_hoc_res_long[post_hoc_res_long$pvals<.05,])
      sig_vals_vector <- NULL
      for (j in seq(nrow(sig_vals))){
        sig_row<-sig_vals[j,]
        sig_string <-paste0(c(sig_row$Dimension,' ', sig_row$Group,'; '), collapse = '')
        sig_vals_vector <- paste0(sig_vals_vector, sig_string)
      }
      if (length(sig_vals_vector) == 0) {sig_vals_vector = ''}
    } else {
      sig_vals_vector = '' 
    }
    demo_table <- cbind(unname(summary_df),t(padding),t(padding))
    empty_row <- c(pretty_name,rep("",ncol(summary_df)-1),unname(paste_stats),sig_vals_vector)
    demo_row <- rbind(unname(empty_row), demo_table)
    
  }
  return(demo_row)
}

vjp_build_demographics_table <- function(df, demo_vars, grouping_var){
  demo_table = NULL
  for (j in demo_vars){
    
    if (length(j)==3){
      j = c(j,NULL)
    }
    #browser()
    row<-vjp_demographics_row(df,j[1],grouping_var,j[2],j[3],j[4])
    demo_table <- rbind(demo_table,unname(row))
  }
  
  #create colnames
  demo_df <- as.data.frame(demo_table)
  ns <- df %>% dplyr::count(!!as.name(grouping_var))
  group_names<-levels(df[[grouping_var]])
  colnames(demo_df)[-1] <- cbind(t(paste0(group_names,' (n=',ns$n,')')),
                                 "Statistics","Post Hocs")
  return(demo_df)
}