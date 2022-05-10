#' Full Pipeline Function
#'
#'
#' @import glue
#' @import dplyr
#' @import stringr
#' @export
#'
full_pipeline <- function(Data, Project = NULL, Replication = NULL, Lab = NULL, DV = NULL, Group = NULL, IV = NULL, output_folder, folder_name = NULL, suppress_list_output = FALSE){


  if (missing(output_folder)) {
    print("You chose not to export the output of the pipeline.")
  } else {

    ## create the output folder
    if (is.null(folder_name)) { # create folder with the default name "MetaPipe Output"
      # glue names
      MetaPipe_folder <- glue::glue("{output_folder}MetaPipe Output")
    } else { # create folder with custom name
      # glue names
      MetaPipe_folder <- glue::glue("{output_folder}{folder_name}")
    }
    # create directory
    dir.create(MetaPipe_folder)
    # create folder for individual participant data
    dir.create(glue::glue("{MetaPipe_folder}/Individual Participant Data"))
    # create folder for lab summaries
    dir.create(glue::glue("{MetaPipe_folder}/Lab Summaries"))
    # create folder for merged lab summaries
    dir.create(glue::glue("{MetaPipe_folder}/Merged Lab Summaries"))
    # create folder for meta analyses
    dir.create(glue::glue("{MetaPipe_folder}/Meta Analyses"))
    # create folder for meta analyses
    dir.create(glue::glue("{MetaPipe_folder}/Meta Pipe"))

  }

  ## create output list
  output_list <- vector(mode = "list", length = 4)
  names(output_list) <- c("Individual Participant Data", "Lab Summaries", "Merged Lab Summaries", "Meta Analyses")

  ## 1. Step of Pipeline: create individual participant data output with MetaPipe names and codebook

  ## renaming all list object columns according to function input
  # creating a function to rename the columns
  renamer <- function(x){
    Data[[x]] %>%
      rename(Project = {{ Project }},
             Replication = {{ Replication }},
             Lab = {{ Lab }},
             DV = {{ DV }},
             Group = {{ Group }},
             IV = {{ IV }})
  }
  # applying the function
  Data_List <- lapply(1:length(Data), renamer)

  # renaming the list according to original data list
  names(Data_List) <- names(Data)

  # create a function for exporting ipd as csv per replication
  export_ipd_fun <- function(x){
    project_name <- unique(x$Project)
    replication_name <- unique(x$Replication)
    write.csv(x, file = glue::glue("{MetaPipe_folder}/Individual Participant Data/{project_name}_{replication_name}_individual_participant_data.csv"))
  }
  # apply function
  if (missing(output_folder)) {} else { lapply(Data_List, export_ipd_fun) }

  ## create codebook for individual participant data
  codebook_ipd <- c()
  # # export codebook for individual participant data
  # write.csv(codebook_ipd, glue::glue("{MetaPipe_folder}/Individual Participant Data/codebook for individual participant data.csv"))

  # add to the output list for step 1 of the pipeline
  output_list$`Individual Participant Data` <- list(Data_List, codebook_ipd)
  # rename list items
  names(output_list$`Individual Participant Data`) <- c("individual participant data", "codebook_ipd")

  ## 2. Step of Pipeline: create lab summaries
  if (missing(output_folder)) {
    output_list$`Lab Summaries` <- MetaPipe::create_lab_summaries(data = Data,
                                                                  Project = {{Project}},
                                                                  Replication = {{Replication}},
                                                                  Lab = {{Lab}},
                                                                  DV = {{DV}},
                                                                  Group = {{Group}},
                                                                  IV = {{IV}},
                                                                  suppress_list_output = FALSE)
  } else {
    output_list$`Lab Summaries` <- MetaPipe::create_lab_summaries(data = Data,
                                                                  Project = {{Project}},
                                                                  Replication = {{Replication}},
                                                                  Lab = {{Lab}},
                                                                  DV = {{DV}},
                                                                  Group = {{Group}},
                                                                  IV = {{IV}},
                                                                  output_folder = glue::glue("{MetaPipe_folder}/Lab Summaries/"),
                                                                  suppress_list_output = FALSE)
  }

  ## 3. Step of Pipeline: merge lab summaries
  if (missing(output_folder)) {
    output_list$`Merged Lab Summaries` <- MetaPipe::merge_lab_summaries(data = output_list$`Lab Summaries`$lab_summaries,
                                                                        suppress_list_output = FALSE)
  } else {
    output_list$`Merged Lab Summaries` <- MetaPipe::merge_lab_summaries(data = output_list$`Lab Summaries`$lab_summaries,
                                                                        output_folder = glue::glue("{MetaPipe_folder}/Merged Lab Summaries/"),
                                                                        suppress_list_output = FALSE)
  }

  ## 4. Step of Pipeline: perform meta analyses

  output_list$`Meta Analyses` <- MetaPipe::meta_analyses(output_list$`Merged Lab Summaries`$merged_lab_summaries)
  # export data
  if (missing(output_folder)) {} else {
  write.csv(output_list$`Meta Analyses`, glue::glue("{MetaPipe_folder}/Meta Analyses/meta analyses.csv"))
  }

  ## 5. Step of Pipeline: create a data frame for the MetaPipe App

  # reorder data frames
  merged_lab_summaries <- dplyr::arrange(output_list$`Merged Lab Summaries`$merged_lab_summaries, Replication)
  meta_analyses <- dplyr::arrange(output_list$`Meta Analyses`, Replication)

  # number of labs per replication (= "How many labs are in each replication?")
  k_per_Replication <- merged_lab_summaries %>%
    dplyr::count(.,Replication) %>%
    dplyr::pull(.,n)

  # duplication vector (indicates how often replication level column needs to be repeated to match the lab level structure)
  duplications <- rep(1:nrow(meta_analyses), k_per_Replication)

  # expand df
  expanded_MA <- meta_analyses[duplications,]

  # reorder both data frames (so they match) and combine them to create the MetaPipe App data format
  MetaPipe_Data <- cbind(merged_lab_summaries, expanded_MA)

  # add "Lab__Empirical__" to all lab related columns and "MA__" to all meta-analysis columns
  # Lab
  # columns from "T_N" to "SE_SMD"
  first_lab_col <- which(names(MetaPipe_Data) == "T_N")
  last_lab_col <- which(names(MetaPipe_Data) == "SE_SMD")
  names(MetaPipe_Data)[first_lab_col:last_lab_col] <- stringr::str_c("Lab__Empirical__", names(MetaPipe_Data[,first_lab_col:last_lab_col]))

  # MA
  first_lab_MA <- last_lab_col + 1
  last_lab_MA <- ncol(MetaPipe_Data)
  names(MetaPipe_Data)[first_lab_MA:last_lab_MA] <- stringr::str_c("MA__", names(MetaPipe_Data[,first_lab_MA:last_lab_MA]))

  # delete duplicate columns
  MetaPipe_Data$MA__Project <- NULL
  MetaPipe_Data$MA__Replication <- NULL

  # add to output list
  MetaPipe_Data_list <- list(MetaPipe_Data)
  names(MetaPipe_Data_list) <- "MetaPipe_Data"
  output_list <- append(output_list, MetaPipe_Data_list)

  # export data
  if (missing(output_folder)) {} else {
    write.csv(output_list$MetaPipe_Data, glue::glue("{MetaPipe_folder}/Meta Pipe/MetaPipe_Data.csv"))
  }

  if (suppress_list_output == TRUE) {
    print("You chose not to return results in R. If you specified an output folder, check that folder for the output of the pipeline.")
  } else {
    return(output_list)
  }
}

### merge_lab_summaries --> codebook produces mostly NA
