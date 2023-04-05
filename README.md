The following is a set of instructions for launching the Control Tower Model Monitoring R Shiny app. It consists of transforming the raw training and incoming data into a clean dataset for the app, as well as creating and saving plots for the app on the backend to speed up viewing them in the app.

We include a set of training data (raw_demo_data_training.rds) to mimic data utilized to train the model, and logging data (raw_demo_data_logging.rds) to mimic incoming data while the model is deployed. These data have been randomly drawn from synthetic distributions and do not hold the same properties as actual EHR data.

Instructions
1)	Change your R working directory to the downloaded “control_tower_demo” folder.
2)	Run “create_demo_features.R”
3)	Run “build_plots.R”
4)	Assuming you are using Rstudio, open “app.R” and click the “Run App” button. 
