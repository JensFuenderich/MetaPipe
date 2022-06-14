##### Example: MetaPipe::merge_lab_summaries()

### This script is an example for the merge_lab_summaries() function in the MetaPipe package.
### The merge_lab_summaries() function performs the first step of the MetaPipe. Afterwards the meta_analyses() function may be applied to the data output.
### It will create a list output and export a folder with the same structure as the list that is created in your current working directory.
### If you run the whole script, it first builds an input for the function and then applies that function.

## load/install packages
library(readr)
library(glue)

## installing and loading MetaPipe
library(devtools)
install_github("JensFuenderich/MetaPipe/MetaPipe")
library(MetaPipe)

## Building an input for the function

# import the according table template

# make sure to download the according table template
# the github link the template: https://github.com/JensFuenderich/MetaPipe-OSF-Data/blob/main/Table%20Templates/Lab%20Summaries/Project_Replication_lab_summaries.csv
folder <- "~/Some_Folder/" # make sure to indicate the correct path to the "Project_Replication_lab_summaries.csv"
Project_Replication_lab_summaries <- read_csv(glue::glue("{folder}Project_Replication_lab_summaries.csv"))[,-1]

# set seed for drawing data
set.seed(1973)

data_example <- as.data.frame(matrix(data = rnorm(n = 200 * (ncol(Project_Replication_lab_summaries)-3), mean = 5, sd = 0.5), nrow = 200, ncol = ncol(Project_Replication_lab_summaries)-3))
names(data_example) <- names(Project_Replication_lab_summaries)[4:length(names(Project_Replication_lab_summaries))]

Multi_Lab_1_Rep_A_lab_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0, sd = 0.5)
Multi_Lab_1_Rep_B_lab_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0, sd = 1)
Multi_Lab_2_Rep_C_lab_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0.5, sd = 0.5)
Multi_Lab_2_Rep_D_lab_summaries <- data_example + rnorm(n = ncol(data_example)*nrow(data_example), mean = 0.5, sd = 1)

# create identification columns: Project, Replication and Lab names
Multi_Lab_1 <- rep("Multi_Lab_1", times = nrow(data_example))
Multi_Lab_2 <- rep("Multi_Lab_2", times = nrow(data_example))
Multi_Lab_1_Rep_A <- rep("Rep_A", times = nrow(data_example))
Multi_Lab_1_Rep_B <- rep("Rep_B", times = nrow(data_example))
Multi_Lab_2_Rep_C <- rep("Rep_C", times = nrow(data_example))
Multi_Lab_2_Rep_D <- rep("Rep_D", times = nrow(data_example))
Multi_Lab_1_Labs <- rep(c("Lab_A", "Lab_B", "Lab_C", "Lab_D"), each = nrow(data_example)/4)
Multi_Lab_2_Labs <- rep(c("Lab_E", "Lab_F", "Lab_G", "Lab_H"), each = nrow(data_example)/4)

# combine identification columns and data and rename according to table template
Multi_Lab_1_Rep_A_lab_summaries <- cbind(Multi_Lab_1, Multi_Lab_1_Rep_A, Multi_Lab_1_Labs, Multi_Lab_1_Rep_A_lab_summaries)
names(Multi_Lab_1_Rep_A_lab_summaries) <- names(Project_Replication_lab_summaries)
Multi_Lab_1_Rep_B_lab_summaries <- cbind(Multi_Lab_1, Multi_Lab_1_Rep_B, Multi_Lab_1_Labs, Multi_Lab_1_Rep_B_lab_summaries)
names(Multi_Lab_1_Rep_B_lab_summaries) <- names(Project_Replication_lab_summaries)
Multi_Lab_2_Rep_C_lab_summaries <- cbind(Multi_Lab_1, Multi_Lab_2_Rep_C, Multi_Lab_2_Labs, Multi_Lab_2_Rep_C_lab_summaries)
names(Multi_Lab_2_Rep_C_lab_summaries) <- names(Project_Replication_lab_summaries)
Multi_Lab_2_Rep_D_lab_summaries <- cbind(Multi_Lab_1, Multi_Lab_2_Rep_D, Multi_Lab_2_Labs, Multi_Lab_2_Rep_D_lab_summaries)
names(Multi_Lab_2_Rep_D_lab_summaries) <- names(Project_Replication_lab_summaries)


# create list of lab summaries
list_of_lab_summaries <- list(Multi_Lab_1_Rep_A_lab_summaries, Multi_Lab_1_Rep_B_lab_summaries, Multi_Lab_2_Rep_C_lab_summaries, Multi_Lab_2_Rep_D_lab_summaries)
names(list_of_lab_summaries) <- c("Multi_Lab_1_Rep_A_lab_summaries", "Multi_Lab_1_Rep_B_lab_summaries", "Multi_Lab_2_Rep_C_lab_summaries", "Multi_Lab_2_Rep_D_lab_summaries")

## applying the input to the MetaPipe function

# run merge_lab_summaries
example_MetaPipe_output <- MetaPipe::merge_lab_summaries(data = list_of_lab_summaries,
                                                         output_folder = file.path(paste0(getwd(), "/")) # chooses the current working directory as folder for exports
)

## The data output of the function may be used as input for the MetaPipe::meta_analyses() function.



