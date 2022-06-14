##### Example: MetaPipe::full_pipeline()

### This script is an example for the full_pipeline() function in the MetaPipe package.
### The full_pipeline() function performs all steps of the MetaPipe.
### It will create a list output and export a folder with the same structure as the list that is created in your current working directory.
### If you run the whole script, it first builds an input for the function and then applies that function.

## installing and loading MetaPipe
library(devtools)
install_github("JensFuenderich/MetaPipe/MetaPipe")
library(MetaPipe)

## Building an input for the function

# create vectors with names
project_names <- c("Multi_Lab_1", "Multi_Lab_2") # two projects
replication_names <- c("Rep_A", "Rep_B", "Rep_C", "Rep_D") # two replications per project
lab_names <- c("Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E",
               "Lab_A", "Lab_B", "Lab_C", "Lab_D", "Lab_E",
               "Lab_F", "Lab_G", "Lab_H", "Lab_I", "Lab_J",
               "Lab_F", "Lab_G", "Lab_H", "Lab_I", "Lab_J") # k = 5 per replication

# create df with all example data
set.seed(1973)
example_data_df <- data.frame(Project = rep(project_names, each = 100),
                              Replication = rep(replication_names, each = 50),
                              Lab = rep(lab_names, each = 10), # n = 10 (5 in control, 5 in treatment group)
                              DV = round(rnorm(n = 2e2, mean = 0, sd = 5), 0),
                              Treatment = rep(c(1,0), times = 100))

# split the data per replication to prepare for use in MetaPipe::full_pipeline()
example_data_list <- split(example_data_df,
                           example_data_df$Replication)

## applying the input to the MetaPipe function

# run full_pipeline
example_MetaPipe_output <- MetaPipe::full_pipeline(data = example_data_list,
                                                   Project = "Project", # column name needs no change
                                                   Replication = "Replication",
                                                   Lab = "Lab",
                                                   DV = "DV",
                                                   Group = "Treatment", # column name needs changing
                                                   output_path = file.path(paste0(getwd(), "/")), # creates a folder in the current working directory
                                                   folder_name = "MetaPipe_OSF_Example" # the name of that folder
)

## The output folder "MetaPipe_OSF_Example" now contains the table "MetaPipe_data.csv", which is fit for uploading it to the MetaPipe App.
