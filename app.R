library(tidyverse)
library(ggpubr)
library(DT)
library(htmltools)
library(shiny)
library(shinyWidgets)
library(data.table)
library(stringr)
library(pROC)
library(scales)

# load data sets
load("demo_features.RData")
dat.train = readRDS("raw_demo_data_training.rds")

# load historic plots
model_drift_numeric_mean = readRDS("model_drift_numeric_mean.rds")
model_drift_miss = readRDS("model_drift_miss.rds")

most_recent_date = max(dat.stats_features$date)

##### PLOTTING FUNCTIONS ######################################################################################################################
univariate_plot = function(feature, data, title = "", xlab = "date", xlim_ll = NA, xlim_ul = NA, point_size = 3){
  
  p = ggplot(data, aes_string(x = xlab)) + 
    geom_line(aes_string(y = feature)) + geom_point(aes_string(y=feature), size = point_size) + 
    labs(x = xlab, y = feature, title = title, color = "Legend")
  
  if(!is.na(xlim_ll) | !is.na(xlim_ul)){
    p = p + ylim(xlim_ll, xlim_ul)
  }
  p
}

historic_plot_bar = function(feature, recent_data, training_data){
  
  dates_at_zero = unique(recent_data$date)
  
  recent_data = recent_data %>%
    mutate(feature = factor(!!as.symbol(feature))) %>%
    group_by(date, feature) %>%
    count() %>%
    group_by(date) %>%
    mutate(percentage = n/sum(n)) %>% filter(feature == TRUE) %>%
    as.data.frame()
  
  dates_at_zero = setdiff(as.character(dates_at_zero), unique(as.character(recent_data$date)))
  if(length(dates_at_zero) > 0){
    dates_at_zero = data.frame(date = dates_at_zero, feature = TRUE, n = 0, percentage = 0)
    recent_data = rbind(recent_data, dates_at_zero)
  }
  
  training_data = training_data %>%
    mutate(feature = factor(!!as.symbol(feature))) %>% 
    group_by(feature) %>% 
    summarise(count = n()) %>% 
    mutate(percentage = count / sum(count)) %>% filter(feature == TRUE)
  
  training_data = as.data.frame(training_data)
  
  a = ggplot(training_data, aes(x = feature, y = percentage, fill = feature)) + 
    geom_bar(stat = 'identity', show.legend = FALSE) + 
    ggtitle("Train") + labs(x = "") + ylim(0, 1)
  
  b = ggplot(recent_data, aes(x = date, y = percentage, fill = feature)) +
    geom_bar(position = 'dodge', stat = 'identity') + 
    ggtitle("Recent") + labs(y = "") + ylim(0, 1)
  
  ggarrange(a, b, ncol = 2, nrow = 1, widths = c(.1,.9))
}


historic_plot_box = function(feature, recent_data, training_data){
  
  #get ends of whiskers to use to ylim
  a = recent_data[,eval(feature)]
  b = training_data[,eval(feature)]
  
  y_lim_ub = max(quantile(a, .75, na.rm = TRUE) + 1.55 * abs(quantile(a, .75, na.rm = TRUE) - quantile(a, .25, na.rm = TRUE)),
                 quantile(b, .75, na.rm = TRUE) + 1.55 * abs(quantile(b, .75, na.rm = TRUE) - quantile(b, .25, na.rm = TRUE)), na.rm = TRUE)
    
  y_lim_lb = max(quantile(a, .75, na.rm = TRUE) - 1.55 * abs(quantile(a, .75, na.rm = TRUE) - quantile(a, .25, na.rm = TRUE)),
                 quantile(b, .75, na.rm = TRUE) - 1.55 * abs(quantile(b, .75, na.rm = TRUE) - quantile(b, .25, na.rm = TRUE)), na.rm = TRUE)
  
  a = ggplot(training_data) + 
    geom_boxplot(aes_string(y = feature), outlier.shape = NA) + 
    ggtitle("Train") + labs(x = "", y = "") + ylim(y_lim_lb, y_lim_ub)
  
  b =   ggplot(dat.recent, aes(x = as.factor(date))) +
    geom_boxplot(aes_string(y = feature), outlier.shape = NA) + 
    ggtitle("Recent") + labs(y = "", x = "date") + ylim(y_lim_lb, y_lim_ub)
  
  ggarrange(a, b, ncol = 2, nrow = 1, widths = c(.1,.9))
}

historic_plot_mean = function(feature, data){
  
  alpha = 0.75
  
  ggplot(data, aes(x=date)) + 
    geom_line(aes(y=new_mean, color="black")) + geom_point(aes(y=new_mean, color="black")) +
    geom_line(aes(y=new_sd_lower), color="black", alpha = alpha, linetype = "dashed") +
    geom_line(aes(y=new_sd_upper), color="black", alpha = alpha, linetype = "dashed") +
    
    geom_line(aes(y=training_mean, color="green")) +
    geom_line(aes(y=training_sd_lower), color="orange", linetype = "dashed") +
    geom_line(aes(y=training_sd_upper), color="orange", linetype = "dashed") +
    
    labs(x = "date", y = "mean ±1 SD", color = "Legend", title = "Comparison of means (±1 SD)") +
    scale_color_manual(name = 'Data', guide = 'legend', values =c('green'='green','black'='black'), labels = c('new','training')) +
    theme(legend.position="bottom", legend.box = "horizontal")
}


historic_plot_median = function(feature, data){

  alpha = 0.75
  
  ggplot(data, aes(x=date)) +
    geom_line(aes(y=new_median, color="black")) + geom_point(aes(y=new_median, color="black")) +
    geom_line(aes(y=new_lower_quartile), color="black", alpha = alpha, linetype = "dashed") +
    geom_line(aes(y=new_upper_quartile), color="black", alpha = alpha, linetype = "dashed") +

    geom_line(aes(y=training_median, color="green", alpha = alpha)) + geom_point(aes(y=training_median, color="green")) +
    geom_line(aes(y=training_lower_quartile), color="orange", alpha = alpha, linetype = "dashed") +
    geom_line(aes(y=training_upper_quartile), color="orange", alpha = alpha, linetype = "dashed") +

    labs(x = "date", y = "median (25th/75th percentiles)", color = "Legend", title = "Comparison of medians (25th/75th percentiles)") +
    scale_color_manual(name = 'Data', guide = 'legend', values =c('green'='green','black'='black'), labels = c('new','training')) +
    theme(legend.position="bottom", legend.box = "horizontal")
}


risk_score_plot = function(data, title = "", xlab = "date", xlim_ll = NA, xlim_ul = NA, point_size = 3){

  p = ggplot(data, aes_string(x = xlab)) + 
    geom_line(aes(y = median)) + geom_point(aes(y=median), size = point_size) + 
    geom_errorbar(aes(ymin = lower_q, ymax = upper_q), width=.2, position=position_dodge(0.05)) +
    labs(x = xlab, y = "median risk score (Q1, Q3)", title = title, color = "Legend")
    
  if(!is.na(xlim_ll) | !is.na(xlim_ul)){
    p = p + ylim(xlim_ll, xlim_ul)
  }
  
  p
}


##### UI ##############################################################################################################################################
ui <- navbarPage(
  title = div(
    "Control Tower Monitoring"
  ),
  windowTitle = "Control Tower Monitoring",
  tabPanel(
    "Risk Scores",
    fluidRow(
      column(
        12,
        DTOutput("outcomeStats"),
        plotOutput("riskScore_plot_log"),
        plotOutput("riskScore_plot")
      )
    )
  ),
  tabPanel(
    "Features",
    fluidRow(
      column(
        12,
        tags$h3(paste("Statistics for most recent observations:", most_recent_date)),
        DTOutput("mostRecentStats"),
        paste("*P-values are generated from two different statistical tests depending on the feature type.",
              "For numeric features, a Kolmogorov-Smirnov test is used, and for binary features, a chi-square test is used.", sep = " ")
      )
    ),
    fluidRow(
      column(
        12,
        tags$h3("Historic plots for selected feature"),
        plotOutput("std_diff_plot"),
        plotOutput("historic_distribution_plot")
      )
    )
  ),
  tabPanel(
    "Number of Daily Observations",
    fluidRow(
      column(
        12,
        plotOutput("daily_num_obs_plot")
      )
    )
  ),
  tabPanel("Historical Covariates", uiOutput("historical_cov_plots")),
  tabPanel("Historical NAs", uiOutput("historical_na_plots"))
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ##### FEATURES
  output$outcomeStats = renderDT({ 
    dat.stats_features %>% slice_max(date) %>% select(feature, feature_type, feature_importance, std.difference, new_mean, training_mean, new_sd, training_sd, p, p_fdr) %>% filter(feature == "probOfPalCare")
  }, selection = 'single')
  
  output$mostRecentStats = renderDT({ 
    dat.stats_features %>% filter(feature != "probOfPalCare") %>% slice_max(date) %>% select(feature, feature_type, feature_importance, std.difference, new_mean, training_mean, new_sd, training_sd, p, p_fdr)
  }, selection = 'single')
  
  
  observeEvent(input$mostRecentStats_rows_selected, {
    
    if(!is.null(input$mostRecentStats_rows_selected)){
      
      tmp_data = dat.stats_features %>% slice_max(date) %>% select(feature, feature_importance, feature_type, std.difference, new_mean, training_mean)
      selected_feature = tmp_data[input$mostRecentStats_rows_selected, ]$feature
      selected_feature_type = tmp_data[input$mostRecentStats_rows_selected, ]$feature_type[1]
      
      if(selected_feature_type == "binary"){
        output$historic_distribution_plot = renderPlot({ historic_plot_bar(feature = selected_feature, recent_data = dat.recent, training_data = dat.train) })
        #output$historic_distribution_plot = renderPlot({historic_plot_mean(feature = selected_feature, data = dat.stats_features[dat.stats_features$feature == selected_feature,])})
      }
      else{
        output$historic_distribution_plot = renderPlot({ historic_plot_box(feature = selected_feature, recent_data = dat.recent, training_data = dat.train) })
        #output$historic_distribution_plot = renderPlot({historic_plot_median(feature = selected_feature, data = dat.stats_features[dat.stats_features$feature == selected_feature,])})
      }
      
      output$std_diff_plot = renderPlot({ univariate_plot(feature = "std.difference", data = dat.stats_features[dat.stats_features$feature == selected_feature, ], title = "Standard Difference") })
    }
  })
  
  ##### OBSERVATIONS  
  output$daily_num_obs_plot = renderPlot({ univariate_plot(feature = "n", data = dat.daily_num_obs, title = "Number of Daily Observations", xlim_ll = 0) })
  
  
  ##### PERFORMANCE
  output$riskScore_plot = renderPlot({ 
    tmp = dat.risk_scores
    tmp$date = as.factor(tmp$date)
    ggplot(tmp, aes(x=date, y=probOfPalCare)) + geom_violin(trim=FALSE) + geom_boxplot(color="#0854bc", fill="lightblue", alpha=0.2, width=0.1) + labs(y = "probOfPalCare")
    
  })
  
  output$riskScore_plot_log = renderPlot({ 
    tmp = dat.risk_scores
    tmp$probOfPalCare = log(tmp$probOfPalCare)
    tmp$date = as.factor(tmp$date)
    ggplot(tmp, aes(x=date, y=probOfPalCare)) + geom_violin(trim=FALSE) + geom_boxplot(color="#0854bc", fill="lightblue", alpha=0.2, width=0.1) + labs(y = "probOfPalCare (log scale)")
    
  })
  
  # ##### HISTORICAL NAs
  listItems_mdm <- paste0("mdm", seq_along(model_drift_miss))
  lapply(seq_along(model_drift_miss), function(i){
    output[[listItems_mdm[i]]] <- renderPlot({model_drift_miss[[i]]$plot})
  })
  output$historical_na_plots <- renderUI({
    lapply(listItems_mdm, plotOutput)
  })

  ##### HISTORICAL COVARIATES
  listItems_mdnm <- paste0("mdnm", seq_along(model_drift_numeric_mean))
  lapply(seq_along(model_drift_numeric_mean), function(i){
    output[[listItems_mdnm[i]]] <- renderPlot({model_drift_numeric_mean[[i]]$plot})
  })
  output$historical_cov_plots <- renderUI({
    lapply(listItems_mdnm, plotOutput)
  })
}



# Run the application 
options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
shinyApp(ui = ui, server = server)