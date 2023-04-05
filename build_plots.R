library(tidyverse)
library(dfphase1)
library(reshape2)
library(gridExtra)

load("demo_features.RData")
most_recent_date = as.Date("02/07/2023", "%m/%d/%Y")

dat.log = readRDS("raw_demo_data_logged.rds")
dat.train = readRDS("raw_demo_data_training.rds")
feature_info = readRDS("demo_feature_info.rds")

#bind production data to the training data
all_data <- bind_rows(dat.log, dat.train)
all_data <- all_data %>% mutate(week = cut(date, "week"))
all_data = all_data %>% mutate_if(is.logical ,as.numeric)

#Weekly
numeric_mean <- all_data %>%
  group_by(week) %>%  #groupby the week 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% #weekly means
  ungroup() %>% 
  filter(week != "2018-04-30") %>% #omit a particular week
  mutate(week = as.Date(week), 
         trainprod = ifelse(week <= as.Date("2020-01-01"), "train", "prod")) %>% #set as week
  select(-starts_with("Lift")) #remove columns

#Missing values count
missing_values <-  all_data %>% select(-any_of(c("date", "admit.time"))) %>%
  group_by(week) %>% 
  mutate_all(funs(ifelse(is.na(.), TRUE, FALSE))) %>% #find NA values
  group_by(week) %>% #groupby week
  summarise_if(is.logical, mean, na.rm = TRUE) %>% #remove the NA values
  ungroup()%>%
  filter(week != "2018-04-30") %>% 
  mutate(week = as.Date(week), 
         trainprod = ifelse(week <= as.Date("2020-01-01"), "train", "prod")) %>%
  select(-starts_with("Lift"), -starts_with("Rank"))

#Numeric Means
p <- list()
column_loop <- feature_info$feature

for(i in 1:length(column_loop)){
  
  #create vector that indexes missing values
  NAvec <- as.numeric(which(is.na(as.numeric(unlist(numeric_mean[,column_loop[i]])))))
  #create a blank NA matrix that will be used to plot the data
  new_mat <- matrix(nrow = length(unlist(numeric_mean[,column_loop[i]])), 
                    ncol = 4)
  #replace necessary NA values with true $stat values from the rsp function
  #if there are no missing values, we will create a dummy vector of length 1 that removes the nth+1 row of the matrix. 
  if (length(NAvec) ==0){NAvec <- nrow(new_mat)+1}
  
  new_mat[-NAvec,1:2] <- as.matrix(rsp(as.numeric(unlist(na.omit(numeric_mean[,column_loop[i]]))), plot=FALSE)$stat)
  new_mat[-NAvec,3:4] <- as.matrix(rsp(as.numeric(unlist(na.omit(numeric_mean[,column_loop[i]]))), plot= FALSE)$fit)
  
  #name the columns for clarity
  colnames(new_mat) <- c("level_stat", "scale_stat", "level_fit", "scale_fit")
  new_mat <- data.frame(new_mat)
  new_dat <- cbind(numeric_mean[,c("week","trainprod")],new_mat)
  
  #LEVEL:
  leveldat <- new_dat[,c("week","trainprod","level_stat","level_fit")]
  ldf <- melt(leveldat,id.vars=c("week", "trainprod"))
  backcol_level <- ifelse(rsp(as.numeric(unlist(na.omit(numeric_mean[,column_loop[i]]))), plot= FALSE)$p["level"] <0.05,
                          'pink','palegreen')
  
  #SCALE
  scaledat <- new_dat[,c("week","trainprod","scale_stat","scale_fit")]
  sdf <- melt(scaledat,id.vars=c("week", "trainprod"))
  backcol_scale <- ifelse(rsp(as.numeric(unlist(na.omit(numeric_mean[,column_loop[i]]))), plot= FALSE)$p["scale"] <0.05,
                          'pink','palegreen')
  #LEVEL
  p_level <-   ggplot(ldf, aes(x=week, y=value, col=trainprod, size = variable)) +
    geom_line(aes(linetype = variable))+
    scale_size_manual(breaks=c("level_stat","level_fit"), values=c(0.3,0.7)) +
    scale_color_manual(values=c("red", "black"))+ 
    scale_x_date(breaks = c(as.Date("2020-01-01"),  
                            as.Date("2021-01-01"), 
                            most_recent_date))+
    theme(panel.background = element_rect(fill = backcol_level ))+
    ggtitle(paste0("LEVEL: ",column_loop[i]))
  #SCALE
  p_scale <-   ggplot(sdf, aes(x=week, y=value, col=trainprod, size = variable)) +
    geom_line(aes(linetype = variable))+
    scale_size_manual(breaks=c("scale_stat","scale_fit"), values=c(0.3,0.7)) +
    scale_color_manual(values=c("red", "black"))+ 
    scale_x_date(breaks = c(as.Date("2020-01-01"),  
                            as.Date("2021-01-01"), 
                            most_recent_date))+
    theme(panel.background = element_rect(fill = backcol_scale ))+
    ggtitle(paste0("SCALE: ",column_loop[i]))
  
  p[[length(p)+1]] <- list(name = column_loop[i], plot = ggpubr::ggarrange(p_level, p_scale))
}

saveRDS(p, "model_drift_numeric_mean.rds")


q <- list()
column_loop_miss <- colnames(missing_values %>% select(-any_of(c("week", "date", "trainprod"))))

for(i in 1:length(column_loop_miss)){
  #create a blank NA matrix that will be used to plot the data
  new_mat <- matrix(nrow = length(unlist(missing_values[,column_loop_miss[i]])),
                    ncol = 4)
  new_mat[,1:2] <- as.matrix(rsp(as.numeric(unlist(na.omit(missing_values[,column_loop_miss[i]]))), plot=FALSE)$stat)
  new_mat[,3:4] <- as.matrix(rsp(as.numeric(unlist(na.omit(missing_values[,column_loop_miss[i]]))), plot= FALSE)$fit)

  #name the columns for clarity
  colnames(new_mat) <- c("level_stat", "scale_stat", "level_fit", "scale_fit")
  new_mat <- data.frame(new_mat)
  new_dat <- cbind(missing_values[,c("week","trainprod")],new_mat)

  #LEVEL:
  leveldat <- new_dat[,c("week","trainprod","level_stat","level_fit")]
  ldf <- melt(leveldat,id.vars=c("week", "trainprod"))
  backcol_level <- ifelse(rsp(as.numeric(unlist(na.omit(missing_values[,column_loop_miss[i]]))), plot= FALSE)$p["level"] <0.05,
                          'pink','palegreen')

  #SCALE
  scaledat <- new_dat[,c("week","trainprod","scale_stat","scale_fit")]
  sdf <- melt(scaledat,id.vars=c("week", "trainprod"))
  backcol_scale <- ifelse(rsp(as.numeric(unlist(na.omit(missing_values[,column_loop_miss[i]]))), plot= FALSE)$p["scale"] <0.05,
                          'pink','palegreen')
  #LEVEL
  q_level <-   ggplot(ldf, aes(x=week, y=value, col=trainprod, size = variable)) +
    geom_line(aes(linetype = variable))+
    scale_size_manual(breaks=c("level_stat","level_fit"), values=c(0.3,0.7)) +
    scale_color_manual(values=c("red", "black"))+
    scale_x_date(breaks = c(as.Date("2020-01-01"),
                            as.Date("2021-01-01"),
                            most_recent_date))+
    theme(panel.background = element_rect(fill = backcol_level ))+
    ggtitle(paste0("LEVEL: ",column_loop_miss[i]))+
    ylab("prop. missing")
  
  #SCALE
  q_scale <-   ggplot(sdf, aes(x=week, y=value, col=trainprod, size = variable)) +
    geom_line(aes(linetype = variable))+
    scale_size_manual(breaks=c("scale_stat","scale_fit"), values=c(0.3,0.7)) +
    scale_color_manual(values=c("red", "black"))+
    scale_x_date(breaks = c(as.Date("2020-01-01"),
                            as.Date("2021-01-01"),
                            most_recent_date))+
    theme(panel.background = element_rect(fill = backcol_scale ))+
    ggtitle(paste0("SCALE: ",column_loop_miss[i]))+
    ylab("prop. missing")
  
  q[[length(q)+1]] <- list(name = column_loop[i], plot = ggpubr::ggarrange(q_level, q_scale))
}

saveRDS(q, "model_drift_miss.rds")