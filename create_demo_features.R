library(tidyverse)
library(DT)
library(htmltools)
library(shiny)
library(shinyWidgets)
library(data.table)
library(stringr)
library(pROC)

dat.log = readRDS("raw_demo_data_logged.rds")
dat.train = readRDS("raw_demo_data_training.rds")
feature_info = readRDS("demo_feature_info.rds")
most_recent_date = as.Date("02/07/2023", "%m/%d/%Y")

##### FEATURES ################################################################################################################################
stdif <- function(x, y){
  if(inherits(x, "POSIXct") || inherits(x, "Date")) x <- as.numeric(x)
  if(inherits(y, "POSIXct") || inherits(y, "Date")) y <- as.numeric(y)
  if(!is.numeric(x) && !is.logical(x) && !is.numeric(y) && !is.logical(y)) return(NA_real_)
  mx <- mean(x, na.rm = TRUE)
  my <- mean(y, na.rm = TRUE)
  vx <- if(is.logical(x)) mx*(1 - mx) else var(x, na.rm = TRUE)
  vy <- if(is.logical(y)) my*(1 - my) else var(y, na.rm = TRUE)
  
  if(is.na(mx) || is.na(my)) return(NA)
  if(mx == my) return(0)
  
  if(is.na(vx) || is.na(vy)) return(NA)
  if(vx == 0 || vy == 0) return(Inf)
  
  (mx - my)/sqrt(0.5*(vx + vy))
}

dist.test = function(x,y){
  
  if(all(is.na(x)) || all(is.na(y))) return(NA)
  
  #if class is numeric run ks-test
  if(class(x) == "numeric"){
    out = ks.test(x,y)
  } 
  #else run chi-square test
  else{
    #create contingency table 
    x = table(x)
    y = table(y)
    
    fill_missing_level = function(d){
      for(i in c("FALSE","TRUE")){
        if(!(i %in% names(d))){
          d[i] = 0
        }
      }
      d
    }
    
    #if one element never appears in the vector, add to table with count = 0
    x = fill_missing_level(x)
    y = fill_missing_level(y)
    
    out = chisq.test(rbind(x,y))
  }
  out$p.value
}

make_stats_features = function(data, train){
  
  #filter nms by feature set used in model
  features = sapply(dat.log, class)
  features = features[features %in% c("integer", "numeric", "logical")]
  nms <- intersect(names(features), names(train))
  
  results = tibble(
    feature = nms,
    
    std.difference = map2_dbl(data[nms], train[nms], stdif),
    dist.test = map2_dbl(data[nms], train[nms], dist.test),
    
    training_mean = map_dbl(train[nms], ~mean(.x, na.rm = TRUE)),
    training_sd = map_dbl(train[nms], ~sd(.x, na.rm = TRUE)),
    training_sd_lower = map_dbl(train[nms], ~mean(.x, na.rm = TRUE) + sd(.x, na.rm = TRUE)),
    training_sd_upper = map_dbl(train[nms], ~mean(.x, na.rm = TRUE) - sd(.x, na.rm = TRUE)),
    
    training_median = map_dbl(train[nms], ~median(.x, na.rm = TRUE)),
    training_lower_quartile = map_dbl(train[nms], ~quantile(.x, probs = .25, na.rm = TRUE)),
    training_upper_quartile = map_dbl(train[nms], ~quantile(.x, probs = .75, na.rm = TRUE)),
    
    new_missing = map_dbl(data[nms], ~mean(is.na(.x))),
    
    new_mean = map_dbl(data[nms], ~mean(.x, na.rm = TRUE)),
    new_sd = map_dbl(data[nms], ~sd(.x, na.rm = TRUE)),
    new_sd_lower = map_dbl(data[nms], ~mean(.x, na.rm = TRUE) + sd(.x, na.rm = TRUE)),
    new_sd_upper = map_dbl(data[nms], ~mean(.x, na.rm = TRUE) - sd(.x, na.rm = TRUE)),
    
    new_median = map_dbl(data[nms], ~median(.x, na.rm = TRUE)),
    new_lower_quartile = map_dbl(data[nms], ~quantile(.x, probs = .25, na.rm = TRUE)),
    new_upper_quartile = map_dbl(data[nms], ~quantile(.x, probs = .75, na.rm = TRUE))
    
  )
  
  p = results$dist.test
  p_fdr = p.adjust(p, "fdr")
  
  results$p = p
  results$p_fdr = p_fdr
  
  results = results %>% mutate(
    std.difference = round(std.difference, 2),
    training_mean = round(training_mean, 2),
    training_sd = round(training_sd, 2),
    new_missing = round(new_missing, 2),
    new_mean = round(new_mean, 2),
    new_sd = round(new_sd, 2),
    p = round(p, 3),
    p_fdr = round(p_fdr, 3)
  )
  return(results)
}

# filter to last 2 weeks
dat.recent = dat.log %>% filter(date > (most_recent_date - 14))
dat.stats_features = dat.recent %>% group_by(date) %>% group_modify(~make_stats_features(.x, train = dat.train))
dat.stats_features = as.data.frame(bind_rows(dat.stats_features))
dat.stats_features = merge(dat.stats_features, feature_info, by = "feature", all.x = TRUE)

#handle outcome variable
dat.stats_features[dat.stats_features$feature == "probOfPalCare", "feature_type"] = "numeric"

#Observations
dat.daily_num_obs = dat.recent %>% group_by(date) %>% summarize(n = n())

##### PERFORMANCE ################################################################################################################################
make_stats_performance = function(data){
  if(dim(data) > 1){
    AUC = as.double(auc(multiclass.roc(response = data$prediction, predictor = data$probOfPalCare)))
    data.frame(auc = AUC)
  }
  else{
    data.frame(auc = NA)
  }
}

dat.risk_scores = dat.recent %>% select(date, probOfPalCare)

##### SAVE OUT DATA
dfs = c("dat.recent", "dat.stats_features", "dat.daily_num_obs", "dat.risk_scores")
save(list = dfs, file = "demo_features.RData")
