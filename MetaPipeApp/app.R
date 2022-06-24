#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
## install pacman package if you haven't already
# install.packages("pacman")
library(pacman)

# runs library()/install.packages() where necessary for the listed packages (those necessary for this script)
pacman::p_load("shiny",
               "readr",
               "here",
               "DT",
               "ggplot2",
               "shinyWidgets",
               "shinythemes",
               "devtools",
               "tidyverse",
               "stringr",
               "glue",
               "magrittr")

# devtools::install_github("JensFuenderich/MetaPipe", subdir = "MetaPipe", force = TRUE)
# library(MetaPipe)

### general imports
MA_data <- readr::read_csv(here::here("Data","Input","MetaPipeApp Data Empty.csv"))[,-1]
#IPD_MA_data / MR_data <- readr::read_csv(here::here("Data","Input",""))[,-1]
codebook <- readr::read_csv(here::here("Data","Input","MetaPipeApp codebook.csv"))
codebook_text_vec <- readr::read_file(here::here("Data","Input","MetaPipeApp codebook_text.txt"))

### helpers
# create empty object which will be overwritten by working in the "Reactive Data Table" tab
Reactive_Output_Data <- c()

# create a list for checkboxes, etc (in "Reactive Data Table" tab)
Variables_List <- list(
    AnalysisResults = list("Lab Results" = "Lab",
                           "Model Estimates" = "Model_Estimate",
                           "Tau2" = "__Tau2_",
                           "SE of Tau2" = "SE_Tau2",
                           "Tau" = "Tau_",
                           "I2" = "I2_",
                           "H2" = "H2_",
                           "QE" = "QE_",
                           "QEp" = "QEp_",
                           " " = "exclude"
    ),
    Statistics = list("Control Mean" = "C_M",
                      "Treatment Mean" = "T_M",
                      "Control SD" = "C_SD",
                      "Treatment SD" = "T_SD",
                      "pooled SD" = "pooled_SD",
                      "MD" = "_MD",
                      "SMD" = "_SMD",
                      " " = "exclude"
    ),
    Sample_Size = list("N" = "_N",
                       "K" = "_K",
                       " " = "exclude"
    )
)

### UI

ui <- shiny::navbarPage(

    # make it pretty
    theme = shinythemes::shinytheme("flatly"),

    "MetaPipe Shiny Application",

    shiny::tabPanel(
        "Upload Data",

        sidebarPanel(
            shiny::selectInput(inputId = "select_upload",
                               label = "Which level of data do you want to upload?",
                               choices = c("Individual Participant Data" = "IPD",
                                           "Lab Summaries" =  "LabSum",
                                           "Merged Lab Summaries" = "MergedLabSum",
                                           "MetaPipe (Meta-Analysis & Lab Summaries)" = "MAD"),
                               selected = "MAD"
                               ),
            actionButton("confirm_upload", "confirm upload of selected data")
        ),

        mainPanel(

            ## panel for upload of IPD
            conditionalPanel(condition = "input.select_upload == 'IPD'",
                             h4("Individual Participant Data"),
                             h5("1. Select one ore multiple .csv files with the data you want to analyze."),
                             fileInput("IPD", "choose .csv file with individual participant data",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             h5("2. The MetaPipe needs to know which columns of the data should be used. Select "),
                             shiny::selectInput(inputId = "project_col",
                                                label = "Project:",
                                                choices = ""),
                             shiny::selectInput(inputId = "replication_col",
                                                label = "Replication:",
                                                choices = ""),
                             shiny::selectInput(inputId = "lab_col",
                                                label = "Lab:",
                                                choices = ""),
                             shiny::selectInput(inputId = "DV_col",
                                                label = "DV:",
                                                choices = ""),
                             shiny::selectInput(inputId = "group_col",
                                                label = "Group:",
                                                choices = ""),
                             # textInput("project_col", "Project:"),
                             # textInput("replication_col", "Replication:"),
                             # textInput("lab_col", "Lab:"),
                             # textInput("DV_col", "DV:"),
                             # textInput("group_col", "Group:"),
                             textInput("output_folder_set", "output_folder:"),
                             actionButton("confirm_columns", "confirm to analyze data now (this may take a moment)")
                             ),

            ## panel for upload of lab summaries
            conditionalPanel(condition = "input.select_upload == 'LabSum'",
                             h4("Lab Level Data from multiple .csv"),
                             fileInput("LabSum", "choose multiple .csv files with lab level data",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             h5("Make sure the files to be uploaded take the form of the following template:"),
                             h5("https://github.com/JensFuenderich/MetaPipe/blob/main/Supplementary%20Material/Table%20Templates/Lab%20Summaries/Project_Replication_lab_summaries.csv"),
                             h5("A way to ensure this is to analyze your data with the MetaPipe::create_lab_summaries() function"),
                             actionButton("confirm_LabSum", "confirm to analyze data now (this may take a moment)")
            ),


            ## panel for upload of merged lab summaries

            conditionalPanel(condition = "input.select_upload == 'MergedLabSum'",
                             h4("Lab Level Data merged in a single .csv"),
                             fileInput("MergedLabSum", "choose a single .csv file with merged lab level data",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             h5("Make sure the file to be uploaded takes the form of the following template:"),
                             h5("https://github.com/JensFuenderich/MetaPipe/blob/main/Supplementary%20Material/Table%20Templates/Merged%20Lab%20Summaries/merged_lab_summeries.csv"),
                             h5("A way to ensure this is to analyze your data with the MetaPipe::create_lab_summaries() function and afterwards apply the MetaPipe::merge_lab_summaries() function."),
                             actionButton("confirm_MergedLabSum", "confirm to analyze data now (this may take a moment)")
                             ),

            ## panel for upload of data from meta-analysis
            conditionalPanel(condition = "input.select_upload == 'MAD'",
                             h4("Meta-Analysis Data"),
                             fileInput("MAD", "choose .csv file with meta-analysis data",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             h5("Make sure the data to be uploaded was analyzed with the MetaPipe::meta_analyis function.")
            )
        )
    ),

    ## tab for Meta-Analysis Data Selection

    shiny::tabPanel(
        "Data Selection: Meta-Analysis",

        sidebarLayout(

            sidebarPanel(
                h3("Data Set Selection"),
                shinyWidgets::materialSwitch(inputId = "Level",
                                             label = "Reduce to meta-analytical Data?",
                                             status = "success"),
                selectInput(inputId = "Project",
                            label = "Project",
                            choices = ""
                ),
                selectInput(inputId = "Replication",
                            label = "Replication",
                            choices = ""
                ),
                selectInput(inputId = "Lab",
                            label = "Lab",
                            choices = c("all", unique(MA_data$Lab))
                ),
                shinyWidgets::prettyCheckboxGroup(inputId = "Statistics",
                                                  label = h3("Choose statistics of interest"),
                                                  choices = Variables_List$Statistics,
                                                  selected = "exclude",
                                                  animation = "pulse",
                                                  shape = "curve"
                                                  ),
                tags$head(
                  tags$style(HTML("input[name=Statistics][value='exclude'] { display: none }"))
                ),
                # h3("Exclude Further Information"),
                shinyWidgets::materialSwitch(inputId = "Stat_SE",
                                             label = "Exclude Standard Error of Lab Level Statistic?",
                                             status = "success"),
                # shinyWidgets::materialSwitch(inputId = "Stat_ln",
                #                              label = "Exclude the Logarithms of Standard Deviations?",
                #                              status = "success"),
                shinyWidgets::prettyCheckboxGroup(inputId = "AnalysisResults",
                                                  label = h3("Display Analysis results"),
                                                  choices = Variables_List$AnalysisResults,
                                                  selected = "exclude",
                                                  animation = "pulse",
                                                  shape = "curve"
                ),
                tags$head(
                  tags$style(HTML("input[name=AnalysisResults][value='exclude'] { display: none }"))
                ),
                shinyWidgets::prettyCheckboxGroup(inputId = "SampleSize",
                                                  label = h3("Sample Size Information"),
                                                  choices = Variables_List$Sample_Size,
                                                  selected = "exclude",
                                                  animation = "pulse",
                                                  shape = "curve"
                ),
                tags$head(
                  tags$style(HTML("input[name=SampleSize][value='exclude'] { display: none }"))
                ),
                h3("Exclude Non-Effects?"),
                shiny::sliderInput(inputId = "exclude_effects",
                                   label = "Exlcude replications with a model estimate for |g| lower than...",
                                   min = 0,
                                   max = 3,
                                   value = 0,
                                   step = 0.1)

            ),
            mainPanel(
                DT::DTOutput('table'),
                downloadButton("downloadData", "Download")
            )
        )
    ),

    ## tab for Histograms

    tabPanel("Histograms",
             sidebarLayout(
                 sidebarPanel(
                     actionButton(inputId = "upload_hist",
                                  label = "Upload Data"),
                     shiny::varSelectInput(inputId = "hist_data1",
                                           label = "choose a statistic for the histogram",
                                           data = Reactive_Output_Data),
                     checkboxInput(inputId = "hist_include_variable2",
                                   label = "Include a second Variable"),
                     shiny::varSelectInput(inputId = "hist_data2",
                                           label = "choose a statistic for the histogram",
                                           data = Reactive_Output_Data),
                     checkboxInput(inputId = "hist_include_variable3",
                                   label = "Include a third Variable"),
                     shiny::varSelectInput(inputId = "hist_data3",
                                           label = "choose a statistic for the histogram",
                                           data = Reactive_Output_Data)
                 ),
                 mainPanel(
                     h4("Histogram for selected statistics"),
                     plotOutput(outputId = "histogram"),
                     downloadLink("download_hist", "Download Histogram")
                 )
             )
    ),

    ## tab for Violin Plots

    tabPanel("Violin Plots",
             sidebarLayout(
                 sidebarPanel(
                     shiny::actionButton(inputId = "upload_violin",
                                         label = "Upload Data"),
                     radioButtons(inputId = "include_violins",
                                  h3("Number of Violins"),
                                  choices = list("1" = 1,
                                                 "2" = 2,
                                                 "3" = 3,
                                                 "4" = 4,
                                                 "5" = 5,
                                                 "6" = 6),
                                  selected = 1
                     ),
                     shiny::varSelectInput(inputId = "violin_1",
                                           label = "choose a statistic for violin 1",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "violin_2",
                                           label = "choose a statistic for violin 2",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "violin_3",
                                           label = "choose a statistic for violin 3",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "violin_4",
                                           label = "choose a statistic for violin 4",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "violin_5",
                                           label = "choose a statistic for violin 5",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "violin_6",
                                           label = "choose a statistic for violin 6",
                                           data = Reactive_Output_Data),
                     shiny::checkboxInput(inputId = "violin_include_point_size",
                                          label = "Point Size"),
                     shiny::varSelectInput(inputId = "violin_point_size",
                                           label = "choose a statistic for the point size",
                                           data = Reactive_Output_Data)


                 ),
                 mainPanel(
                     h4("Vioin Plot for selected statistics"),
                     plotOutput(outputId = "violin_plot"),
                     downloadLink("download_violin", "Download Violin Plot")
                 )
             )
    ),

    ## tab for Scatter Plots

    tabPanel("Scatter Plots",
             sidebarLayout(
                 sidebarPanel(
                     actionButton(inputId = "upload_scatter", label = "Upload Data"),
                     shiny::varSelectInput(inputId = "x_plot", label = "choose a statistic for x", data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "y_plot", label = "choose a statistic for y", data = Reactive_Output_Data),
                     checkboxInput(inputId = "include_point_size", label = "Point Size"),
                     shiny::varSelectInput(inputId = "size_plot", label = "choose a statistic for the point size", data = Reactive_Output_Data),
                     checkboxInput(inputId = "include_point_color", label = "Point Color"),
                     shiny::varSelectInput(inputId = "color_plot", label = "choose a statistic for the point color", data = Reactive_Output_Data),
                     checkboxInput(inputId = "include_custom_lims", label = "Use Custom Axis Limits (essentially zooming in or out) and update correlation"),
                     numericInput(inputId = "x_min_plot", label = "Minimum of X-Axis", value = 0),
                     numericInput(inputId = "x_max_plot", label = "Maximum of X-Axis", value = 100),
                     numericInput(inputId = "y_min_plot", label = "Minimum of Y-Axis", value = 0),
                     numericInput(inputId = "y_max_plot", label = "Maximum of Y-Axis", value = 100)
                 ),
                 mainPanel(
                     h4("Scatter Plot for selected statistics"),
                     plotOutput(outputId = "scatter_plot"),
                     downloadLink("download_scatter", "Download Scatter Plot")
                 )
             )
    ),

    ## tab for forest Plots

    tabPanel("Forest Plots",
             shiny::sidebarLayout(
                 sidebarPanel(
                     actionButton(inputId = "upload_forest",
                                  label = "Upload Data"),
                     shiny::varSelectInput(inputId = "forest_data1",
                                           label = "choose a statistic of interest",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "forest_data2",
                                           label = "choose the according standard error",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "forest_data3",
                                           label = "choose information on aggregation (likely the lab)",
                                           data = Reactive_Output_Data)
                 ),
                 shiny::mainPanel(
                     h4("Forest Plot for selected statistics"),
                     shiny::plotOutput(outputId = "forest_plot"),
                     downloadLink("download_forest", "Download Forest Plot")
                 )
             )
    ),

    ## tab for Codebook Display

    tabPanel("Codebook Display",
             shiny::sidebarLayout(
                 sidebarPanel(
                     h3("How to use this codebook:"),
                     p(codebook_text_vec)#,
                     #verbatimTextOutput("codebook_text",  placeholder = FALSE)
                 ),
                 shiny::mainPanel(
                     h4("Tabular Codebook"),
                     DT::DTOutput("codebook")
                 )
             )
    )
)

###
### SERVER

server <- function(input, output, session){

    ### IPD Input

    # object for columns selection (IPD upload)
    IPD_raw_data_input_columns <- reactive({

        if (length(input$IPD) > 0) {

            # extract upload info from UI input
            upload_info <- input$IPD

            # import all selected .csv data
            IPD_list <- lapply(upload_info$datapath,readr::read_csv)

            # store input
            saveRDS(IPD_list, here::here("Data", "Input", "UI Upload", "IPD_raw_upload.rds"))

            # store all columns names in "IPD_raw_data_input_columns" for columns selection in UI
            unlist(unique(lapply(IPD_list, names)))

        } else {

        }

    })


    observe({
        updateSelectInput(session, "project_col",
                          choices = IPD_raw_data_input_columns())
    })

    observe({
        updateSelectInput(session, "replication_col",
                          choices = IPD_raw_data_input_columns())
    })
    observe({
        updateSelectInput(session, "lab_col",
                          choices = IPD_raw_data_input_columns())
    })
    observe({
        updateSelectInput(session, "DV_col",
                          choices = IPD_raw_data_input_columns())
    })
    observe({
        updateSelectInput(session, "group_col",
                          choices = IPD_raw_data_input_columns())
    })

    # run the pipeline, as soon as the columns selection ist confirmed
    IPD_data_input <- eventReactive( input$confirm_columns, {

        IPD_list <- readRDS(here::here("Data", "Input", "UI Upload", "IPD_raw_upload.rds"))

        IPD_analzed <- MetaPipe::full_pipeline(data = IPD_list,
                                               Project = input$project_col,
                                               Replication = input$replication_col,
                                               Lab = input$lab_col,
                                               DV = input$DV_col,
                                               Group = input$group_col,
                                               output_path = here::here("Data", "Output",),
                                               folder_name = "Meta_Pipe_Output")

        IPD_analzed$`5_Meta_Pipe`$MetaPipe_Data

    })


    ### LabSum Input

    LabSum_raw_data_input <- reactive({

      if (length(input$LabSum) > 0) {

        # extract upload info from UI input
        upload_info <- input$LabSum

        # import all selected .csv data
        LabSum_list <- lapply(upload_info$datapath,readr::read_csv)

        # store input
        saveRDS(LabSum_list, here::here("Data", "Input", "UI Upload", "LabSum_raw_upload.rds"))

      } else {

      }

    })

    # run the pipeline, as soon as the input is confirmed

    LabSum_data_input <- eventReactive( input$confirm_LabSum, {


      LabSum_list <- readRDS(here::here("Data", "Input", "UI Upload", "LabSum_raw_upload.rds"))

      # merge the lab summaries
      LabSum_merged <- MetaPipe::merge_lab_summaries(data = LabSum_list,
                                                     output_folder = here::here("Data", "Output", "Meta_Pipe_Output", "Merged Lab Summaries"))

      # run meta analyses
      LabSum_analyzed <- MetaPipe::meta_analyses(data = LabSum_merged$merged_lab_summaries,
                                                 output_folder = here::here("Data", "Output","Meta_Pipe_Output", "Meta Analyses"))

      ## combine lab and meta analysis data

      # reorder data frames
      merged_lab_summaries <- dplyr::arrange(LabSum_merged$merged_lab_summaries, Replication)
      meta_analyses <- dplyr::arrange(LabSum_analyzed$meta_analyses, Replication)

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

      # delete duplicate/redundant columns
      MetaPipe_Data$MA__Project <- NULL
      MetaPipe_Data$MA__Replication <- NULL
      base::rownames(MetaPipe_Data) <- NULL


      ### Create codebook

      # create empty df
      abbr_library <- data.frame(Abbreviation = logical(0),
                                 Full_Name = logical(0))

      # pair abbreviations with verbal descriptions
      abbr_library <- as.data.frame(base::rbind(c("_T_", "__treatment group_"),
                                                c("_C_", "__control group_"),
                                                c("_N", "_number of participants"),
                                                c("_K", "_number of labs"),
                                                c("_MD", "_mean difference"),
                                                c("_Model_Estimate_", "_model estimate for_"),
                                                c("_M", "_mean"),
                                                c("_SD", "_standard deviation"),
                                                c("__SE_", "__standard error of the_"),
                                                c("_SMD", "_standardized mean difference"),
                                                c("MA__", "meta analysis level:__"),
                                                c("__pooled_", "__pooled_"),
                                                c("Lab__", "lab level:__"), # redundant but maybe necessary for code (if pooled works but (for example) "Estimate" does not, I'll know)
                                                c("__I2_", "__I2 for_"),
                                                c("__H2_", "__H2 for_"),
                                                c("__QE_", "__QE for_"),
                                                c("__QEp_", "__QEp for_")
      ))

      # rename columns of df
      names(abbr_library) <- c("Abbreviation", "Full Name")

      # extract names from merged df
      description_vector <- names(MetaPipe_Data)

      # sorry for this, did not want to loop
      # check if there's enough pipes in that orchestra
      #nrow(abbr_library) (the result of this should be equivalent to the max indexing in the following chunk)


      description_vector %<>% # pipe from magrittr
        gsub(abbr_library$Abbreviation[1], abbr_library$`Full Name`[1], .) %>%
        gsub(abbr_library$Abbreviation[2], abbr_library$`Full Name`[2], .) %>%
        gsub(abbr_library$Abbreviation[3], abbr_library$`Full Name`[3], .) %>%
        gsub(abbr_library$Abbreviation[4], abbr_library$`Full Name`[4], .) %>%
        gsub(abbr_library$Abbreviation[5], abbr_library$`Full Name`[5], .) %>%
        gsub(abbr_library$Abbreviation[6], abbr_library$`Full Name`[6], .) %>%
        gsub(abbr_library$Abbreviation[7], abbr_library$`Full Name`[7], .) %>%
        gsub(abbr_library$Abbreviation[8], abbr_library$`Full Name`[8], .) %>%
        gsub(abbr_library$Abbreviation[9], abbr_library$`Full Name`[9], .) %>%
        gsub(abbr_library$Abbreviation[10], abbr_library$`Full Name`[10], .) %>%
        gsub(abbr_library$Abbreviation[11], abbr_library$`Full Name`[11], .) %>%
        gsub(abbr_library$Abbreviation[12], abbr_library$`Full Name`[12], .) %>%
        gsub(abbr_library$Abbreviation[13], abbr_library$`Full Name`[13], .) %>%
        gsub(abbr_library$Abbreviation[14], abbr_library$`Full Name`[14], .) %>%
        gsub(abbr_library$Abbreviation[15], abbr_library$`Full Name`[15], .) %>%
        gsub(abbr_library$Abbreviation[16], abbr_library$`Full Name`[16], .) %>%
        gsub(abbr_library$Abbreviation[17], abbr_library$`Full Name`[17], .)

      description_vector <- stringr::str_replace_all(description_vector, "__Empirical__", "_")

      description_vector <- stringr::str_replace_all(description_vector, "___", "_")
      description_vector <- stringr::str_replace_all(description_vector, "__", "_")
      description_vector <- stringr::str_replace_all(description_vector, "_", " ")

      codebook_for_meta_pipe <- data.frame(Variable_Name = names(MetaPipe_Data), Variable_Description = description_vector)

      # export codebook
      utils::write.csv(codebook_for_meta_pipe,
                       file = "Data/Output/Meta Pipe/codebook_for_meta_pipe_data.csv",
                       row.names = FALSE)
      # export data
      utils::write.csv(MetaPipe_Data,
                       file = "Data/Output/Meta Pipe/MetaPipe_Data.csv",
                       row.names = FALSE)
      MetaPipe_Data

    })


    ### MergedLabSum Input

    MergedLabSum_raw_data_input <- reactive({

      if (length(input$LabSum) > 0) {

        # extract upload info from UI input
        upload_info <- input$MAD

        # import selected .csv data
        MergedLabSum_csv <- readr::read_csv(file = upload_info$datapath)

        # store input
        saveRDS(MergedLabSum_csv, here::here("Data", "Input", "UI Upload", "MergedLabSum_raw_upload.rds"))

      } else {

      }

    })

    # run the pipeline, as soon as the input is confirmed

    MergedLabSum_data_input <- eventReactive( input$confirm_MergedLabSum, {

      MergedLabSum <- readRDS(here::here("Data", "Input", "UI Upload", "MergedLabSum_raw_upload.rds"))

      # run meta analyses
      LabSum_analyzed <- MetaPipe::meta_analyses(data = MergedLabSum,
                                                 output_folder = here::here("Data", "Output","Meta_Pipe_Output", "Meta Analyses"))

      ## combine lab and meta analysis data

      # reorder data frames
      merged_lab_summaries <- dplyr::arrange(MergedLabSum, Replication)
      meta_analyses <- dplyr::arrange(LabSum_analyzed$meta_analyses, Replication)

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

      # delete duplicate/redundant columns
      MetaPipe_Data$MA__Project <- NULL
      MetaPipe_Data$MA__Replication <- NULL
      base::rownames(MetaPipe_Data) <- NULL


      ### Create codebook

      # create empty df
      abbr_library <- data.frame(Abbreviation = logical(0),
                                 Full_Name = logical(0))

      # pair abbreviations with verbal descriptions
      abbr_library <- as.data.frame(base::rbind(c("_T_", "__treatment group_"),
                                                c("_C_", "__control group_"),
                                                c("_N", "_number of participants"),
                                                c("_K", "_number of labs"),
                                                c("_MD", "_mean difference"),
                                                c("_Model_Estimate_", "_model estimate for_"),
                                                c("_M", "_mean"),
                                                c("_SD", "_standard deviation"),
                                                c("__SE_", "__standard error of the_"),
                                                c("_SMD", "_standardized mean difference"),
                                                c("MA__", "meta analysis level:__"),
                                                c("__pooled_", "__pooled_"),
                                                c("Lab__", "lab level:__"), # redundant but maybe necessary for code (if pooled works but (for example) "Estimate" does not, I'll know)
                                                c("__I2_", "__I2 for_"),
                                                c("__H2_", "__H2 for_"),
                                                c("__QE_", "__QE for_"),
                                                c("__QEp_", "__QEp for_")
      ))

      # rename columns of df
      names(abbr_library) <- c("Abbreviation", "Full Name")

      # extract names from merged df
      description_vector <- names(MetaPipe_Data)

      # sorry for this, did not want to loop
      # check if there's enough pipes in that orchestra
      #nrow(abbr_library) (the result of this should be equivalent to the max indexing in the following chunk)


      description_vector %<>% # pipe from magrittr
        gsub(abbr_library$Abbreviation[1], abbr_library$`Full Name`[1], .) %>%
        gsub(abbr_library$Abbreviation[2], abbr_library$`Full Name`[2], .) %>%
        gsub(abbr_library$Abbreviation[3], abbr_library$`Full Name`[3], .) %>%
        gsub(abbr_library$Abbreviation[4], abbr_library$`Full Name`[4], .) %>%
        gsub(abbr_library$Abbreviation[5], abbr_library$`Full Name`[5], .) %>%
        gsub(abbr_library$Abbreviation[6], abbr_library$`Full Name`[6], .) %>%
        gsub(abbr_library$Abbreviation[7], abbr_library$`Full Name`[7], .) %>%
        gsub(abbr_library$Abbreviation[8], abbr_library$`Full Name`[8], .) %>%
        gsub(abbr_library$Abbreviation[9], abbr_library$`Full Name`[9], .) %>%
        gsub(abbr_library$Abbreviation[10], abbr_library$`Full Name`[10], .) %>%
        gsub(abbr_library$Abbreviation[11], abbr_library$`Full Name`[11], .) %>%
        gsub(abbr_library$Abbreviation[12], abbr_library$`Full Name`[12], .) %>%
        gsub(abbr_library$Abbreviation[13], abbr_library$`Full Name`[13], .) %>%
        gsub(abbr_library$Abbreviation[14], abbr_library$`Full Name`[14], .) %>%
        gsub(abbr_library$Abbreviation[15], abbr_library$`Full Name`[15], .) %>%
        gsub(abbr_library$Abbreviation[16], abbr_library$`Full Name`[16], .) %>%
        gsub(abbr_library$Abbreviation[17], abbr_library$`Full Name`[17], .)

      description_vector <- stringr::str_replace_all(description_vector, "__Empirical__", "_")

      description_vector <- stringr::str_replace_all(description_vector, "___", "_")
      description_vector <- stringr::str_replace_all(description_vector, "__", "_")
      description_vector <- stringr::str_replace_all(description_vector, "_", " ")

      codebook_for_meta_pipe <- data.frame(Variable_Name = names(MetaPipe_Data), Variable_Description = description_vector)

      # export codebook
      utils::write.csv(codebook_for_meta_pipe,
                       file = "Data/Output/Meta Pipe/codebook_for_meta_pipe_data.csv",
                       row.names = FALSE)
      # export data
      utils::write.csv(MetaPipe_Data,
                       file = "Data/Output/Meta Pipe/MetaPipe_Data.csv",
                       row.names = FALSE)
      MetaPipe_Data

    })



    ### MAD Input

    MAD_data_input <- reactive({
        upload_info <- input$MAD
        readr::read_csv(file = upload_info$datapath)
    })

    MA_data <- eventReactive( input$confirm_upload, {
        if (input$select_upload == "MAD") {
          MAD_data_input()
        } else if (input$select_upload == "MergedLabSum") {
          MergedLabSum_data_input()
        } else if (input$select_upload == "LabSum") {
          LabSum_data_input()
        } else if (input$select_upload == "IPD") {
          IPD_data_input()
        } else {
            c()
        }
    })


    ### Data Selection: Meta-Analysis

    ## selectInput dependencies

    project_choices <- reactive({
        MA_data <- MA_data()
        c("all", unique(MA_data$Project))
    })
    replication_choices <- reactive({
        MA_data <- MA_data()
        unique(MA_data$Replication)
    })
    lab_choices <- reactive({
        MA_data <- MA_data()
        c("all", unique(MA_data$Lab))
    })

    observe({
        updateSelectInput(session, "Project",
                          choices = project_choices())
        })
    observe({
        updateSelectInput(session, "Replication",
                          choices = if (input$Project == "all") { # return all replications
                              c("all", replication_choices())
                          } else { # only return replications from the selected project
                              MA_data <- MA_data()
                              c("all", unique(MA_data[MA_data$Project == input$Project,]$Replication))
                          }
                              )
        })
    observe({
        updateSelectInput(session, "Lab",
                          choices = if (input$Project == "all") {
                              MA_data <- MA_data()
                              c("all",unique(MA_data[MA_data$Project == input$Project,]$Lab))
                          } else {
                              MA_data <- MA_data()
                              c("all", unique(MA_data[MA_data$Project == input$Project,]$Lab))
                          }
                          )
    })


    ## create the data table as reactive object according to the selection in the data table tab
    data <- reactive( {

        MA_data <- MA_data()

        # decide if lab level data is included
        if (input$Level == TRUE) {
            df <- unique( MA_data %>% dplyr::select(!dplyr::contains("Lab")) )
        } else {
            df <- MA_data
        }

        # select project of interest
        if (input$Project != "all") {
            df <- df[df$Project == input$Project,]
        }

        # select replication of interest
        if (input$Replication != "all") {
            df <- df[df$Replication == input$Replication,]
        }

        # select lab of interest
        if (input$Lab != "all") {
            df <- df[df$Lab == input$Lab,]
        }

        # exclude non effects
        # if (input$exclude_0 == TRUE) {
        #     df <- subset(df, abs(df$MA__Model_Estimate__SMD) > 0.1 )
        # }

        df <- subset(df, abs(df$MA__Model_Estimate__SMD) > input$exclude_effects)

        # display the df with selection according to SampleSize, Statistics and AnalysisResults
        if (input$Level == TRUE) { # this chunk runs if lab level data is NOT included
            df <- df %>%
                dplyr::select(Project,
                              Replication,
                              dplyr::contains(input$Statistics),
                              dplyr::contains(input$SampleSize)) %>%
                dplyr::select(Project,
                              Replication,
                              dplyr::contains(input$AnalysisResults),
                              dplyr::contains(input$SampleSize))
        } else if (input$Level == FALSE & input$Stat_SE == FALSE) { # this chunk runs if lab level data is included and SE included
            df <- df %>%
                dplyr::select(Project,
                              Replication,
                              Lab,
                              dplyr::contains(input$Statistics),
                              dplyr::contains(input$SampleSize)) %>%
                dplyr::select(Project,
                              Replication,
                              Lab,
                              dplyr::contains(input$AnalysisResults),
                              dplyr::contains(input$SampleSize))

        } else if (input$Level == FALSE & input$Stat_SE == TRUE) { # this chunk runs if lab level data is included, but SE excluded
            df <- df %>%
                dplyr::select(Project,
                              Replication,
                              Lab,
                              dplyr::contains(input$Statistics),
                              dplyr::contains(input$SampleSize)) %>%
                dplyr::select(Project,
                              Replication,
                              Lab,
                              dplyr::contains(input$AnalysisResults),
                              dplyr::contains(input$SampleSize)) %>%
                dplyr::select(!dplyr::contains("__SE"))
        }

        # exclude ln values
        if (input$Stat_ln != FALSE) {
            df <- df %>%
                dplyr::select(!dplyr::contains("ln_"))
        }
        df <- unique(df)
        write.csv(df,
                  file = here::here("Data", "Output", "Reactive Output.csv"),
                  row.names = FALSE)
        df

    })

    ## download button

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("MetaPipe Data Selection-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(data(),
                      file,
                      row.names = FALSE)
        }
    )

    ## update data for plots

    # histogram
    observeEvent(input$upload_hist, {
        Reactive_Output_Data <- readr::read_csv(here::here("Data", "Output", "Reactive Output.csv"))[,-1]

        updateVarSelectInput(session, "hist_data1",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "hist_data2",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "hist_data3",
                             data = Reactive_Output_Data)

    })

    # violin plot
    observeEvent(input$upload_violin, {
        Reactive_Output_Data <- readr::read_csv(here::here("Data", "Output", "Reactive Output.csv"))[,-1]

        updateVarSelectInput(session, "violin_1",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "violin_2",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "violin_3",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "violin_4",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "violin_5",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "violin_6",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "violin_point_size",
                             data = Reactive_Output_Data)

    })

    # scatter plot
    observeEvent(input$upload_scatter, {
        Reactive_Output_Data <- readr::read_csv(here::here("Data", "Output", "Reactive Output.csv"))[,-1]

        # Can also set the label and select items
        updateVarSelectInput(session, "x_plot",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "y_plot",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "size_plot",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "color_plot",
                             data = Reactive_Output_Data)
    })

    # forest plot
    observeEvent(input$upload_forest, {
        Reactive_Output_Data <- readr::read_csv(here::here("Data", "Output", "Reactive Output.csv"))[,-1]

        updateVarSelectInput(session, "forest_data1",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "forest_data2",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "forest_data3",
                             data = Reactive_Output_Data)

    })


    ## create output

    # tab 1: Reactive Data Table
    output$table = DT::renderDT(
        data(), options = list(lengthChange = FALSE)
    )

    # tab 2: Histogram

    hist_plot <- shiny::reactive({

      # select data from first input for the histogram
      data1 <- unlist(data() %>% dplyr::select(input$hist_data1))
      hist_data1 <- data.frame(Data = data1,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == input$hist_data1)$Variable_Description,
                                 times = length(data1)))
      # select data from second input for the histogram
      data2 <- unlist(data() %>% dplyr::select(input$hist_data2))
      hist_data2 <- data.frame(Data = data2,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == input$hist_data2)$Variable_Description,
                                 times = length(data2)))
      # select data from third input for the histogram
      data3 <- unlist(data() %>% dplyr::select(input$hist_data3))
      hist_data3 <- data.frame(Data = data3,
                               Statistic = rep(
                                 subset(codebook, codebook$Variable_Name == input$hist_data3)$Variable_Description,
                                 times = length(data3)))

      if (input$hist_include_variable2 == FALSE & input$hist_include_variable3 == FALSE) {
        hist_data <- hist_data1
      } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == FALSE) {
        hist_data <- rbind(hist_data1, hist_data2)
      } else if (input$hist_include_variable2 == FALSE & input$hist_include_variable3 == TRUE) {
        hist_data <- rbind(hist_data1, hist_data3)
      } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == TRUE) {
        hist_data <- rbind(hist_data1, hist_data2, hist_data3)
      }

      hist_plot_output <- ggplot2::ggplot(hist_data, aes(x = Data, fill = Statistic, color = Statistic)) +
        geom_histogram(position="identity",alpha = 0.7) +
        theme_light()

      hist_plot_output

    })


    output$histogram <- renderPlot({
      hist_plot()
    })


    ## download button

    output$download_hist <- downloadHandler(
      filename = function() {
        paste("MetaPipe Histogram-", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file=file, width = 15, height = 7)
        plot(hist_plot())
        dev.off()
      }
    )


    # tab 3: Violin Plot


    violin_plot <- shiny::reactive({

      # select data from first input for the histogram
      data1 <- unlist(data() %>% dplyr::select(input$violin_1))
      violin_1 <- data.frame(Data = data1,
                             Statistic = rep(
                               subset(codebook, codebook$Variable_Name == as.character(input$violin_1))$Variable_Description,
                               times = length(data1)))
      # select data from first input for the histogram
      data2 <- unlist(data() %>% dplyr::select(input$violin_2))
      violin_2 <- data.frame(Data = data2,
                             Statistic = rep(
                               subset(codebook, codebook$Variable_Name == input$violin_2)$Variable_Description,
                               times = length(data2)))

      # select data from first input for the histogram
      data3 <- unlist(data() %>% dplyr::select(input$violin_3))
      violin_3 <- data.frame(Data = data3,
                             Statistic = rep(
                               subset(codebook, codebook$Variable_Name == input$violin_3)$Variable_Description,
                               times = length(data3)))

      # select data from first input for the histogram
      data4 <- unlist(data() %>% dplyr::select(input$violin_4))
      violin_4 <- data.frame(Data = data4,
                             Statistic = rep(
                               subset(codebook, codebook$Variable_Name == input$violin_4)$Variable_Description,
                               times = length(data4)))

      # select data from first input for the histogram
      data5 <- unlist(data() %>% dplyr::select(input$violin_5))
      violin_5 <- data.frame(Data = data5,
                             Statistic = rep(
                               subset(codebook, codebook$Variable_Name == input$violin_5)$Variable_Description,
                               times = length(data5)))

      # select data from first input for the histogram
      data6 <- unlist(data() %>% dplyr::select(input$violin_6))
      violin_6 <- data.frame(Data = data6,
                             Statistic = rep(
                               subset(codebook, codebook$Variable_Name == input$violin_6)$Variable_Description,
                               times = length(data6)))


      vio_data_full <- if (input$include_violins == 1) {
        violin_data <- violin_1
      } else if (input$include_violins == 2) {
        violin_data <- rbind(violin_1, violin_2)
      } else if (input$include_violins == 3) {
        violin_data <- rbind(violin_1, violin_2, violin_3)
      } else if (input$include_violins == 4) {
        violin_data <- rbind(violin_1, violin_2, violin_3, violin_4)
      } else if (input$include_violins == 5) {
        violin_data <- rbind(violin_1, violin_2, violin_3, violin_4, violin_5)
      } else if (input$include_violins == 6) {
        violin_data <- rbind(violin_1, violin_2, violin_3, violin_4, violin_5, violin_6)
      }

      if (input$violin_include_point_size == FALSE) {
        violin_data$dot_size <- rep(1, nrow(violin_data) )
      } else if (input$violin_include_point_size == TRUE ) {
        violin_data$dot_size <- rep(unlist(data() %>% dplyr::select(input$violin_point_size)), input$include_violins)
      }


      # delete redundant level information
      violin_data$common_level <- rep(str_split(violin_data$Statistic, ":")[[1]][1], nrow(violin_data))
      violin_data$Statistic <- str_replace(violin_data$Statistic, "meta analysis level: ", "")
      violin_data$Statistic <- str_replace(violin_data$Statistic, "lab level: ", "")
      # delete redundant statistic information
      violin_data$common_statistic <- rep(str_split(violin_data$Statistic, " for")[[1]][1], nrow(violin_data))
      violin_data$Statistic <- str_replace(violin_data$Statistic, "model estimate for ", "")
      violin_data$Statistic <- str_replace(violin_data$Statistic, "Tau2 for ", "")
      violin_data$Statistic <- str_replace(violin_data$Statistic, "Tau for ", "")
      violin_data$Statistic <- str_replace(violin_data$Statistic, "I2 for ", "")
      violin_data$Statistic <- str_replace(violin_data$Statistic, "H2 for ", "")
      violin_data$Statistic <- str_replace(violin_data$Statistic, "QE for ", "")
      violin_data$Statistic <- str_replace(violin_data$Statistic, "QEp for ", "")

      # tell ggplot to take the order of the vector and stop it from making it alphabetical
      violin_data$Statistic <- factor(violin_data$Statistic, levels = unique(violin_data$Statistic))

      p <- ggplot(violin_data, aes(x=Statistic, y=Data)) +
        geom_violin(trim=TRUE) #+ scale_y_discrete(expand = c(3,5))

      violin_plot_output <- p + geom_boxplot(width = 0.1) +
        geom_jitter(width = 0.3, aes(color = Statistic, size = dot_size), alpha = 0.7) +
        guides( color = "none") +
        theme_light() +
        theme(text = element_text(size=15)) +
        theme(axis.title.x = element_blank()) +
        scale_x_discrete(guide = guide_axis(n.dodge=3)) +
        labs(title = unique(violin_data$common_level),
             subtitle = if (unique(violin_data$common_level) == "meta analysis level") {unique(violin_data$common_statistic)} else {}) +
        scale_size_continuous( if (input$violin_include_point_size == TRUE) { str_replace(str_replace(subset(codebook, codebook$Variable_Name == input$violin_point_size)$Variable_Description, "meta analysis level: ", ""), "lab level: ", "") }else{} )

      violin_plot_output

    })

    output$violin_plot <- renderPlot({
      violin_plot()
    })


    ## download button

    output$download_violin <- downloadHandler(
      filename = function() {
        paste("MetaPipe Violin Plot-", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file=file, width = 15, height = 7)
        plot(violin_plot())
        dev.off()
      }
    )


    # tab 4: Scatter Plot

    scatter_plot <- reactive({

      # Plot Data
      X <- unlist(data() %>% dplyr::select(input$x_plot))
      Y <- unlist(data() %>% dplyr::select(input$y_plot))
      Point_Size <- as.numeric(unlist(data() %>% dplyr::select(input$size_plot)))
      Point_Color <- as.numeric(unlist(data() %>% dplyr::select(input$color_plot)))
      plot_data_full <- as.data.frame(cbind(X,Y, Point_Size, Point_Color))
      names(plot_data_full)[1] <- "X"
      names(plot_data_full)[2] <- "Y"
      names(plot_data_full)[3] <- "Point_Size"
      names(plot_data_full)[4] <- "Point_Color"

      if (input$include_custom_lims == TRUE) {
        plot_data <- dplyr::filter(plot_data_full, X >= input$x_min_plot, X <= input$x_max_plot, Y >= input$y_min_plot, Y <= input$y_max_plot)
      }else{
        plot_data <- plot_data_full
      }


      plot_data$cor_x_y <- rep(cor(plot_data$X, plot_data$Y), nrow(plot_data))

      # Plotting

      scatter_plot_output <- ggplot(plot_data, aes(x = X, y = Y)) +
        geom_point(aes(colour = if (input$include_point_color == TRUE) {Point_Color}else{},
                       size = if (input$include_point_size == TRUE) {Point_Size}else{} )) +
        theme_light() +
        ggtitle(glue::glue("Correlation between currently depicted X and Y data: {round(unique(plot_data$cor_x_y), digits = 2)}")) +
        xlab(subset(codebook, codebook$Variable_Name == input$x_plot)$Variable_Description) +
        ylab(subset(codebook, codebook$Variable_Name == input$y_plot)$Variable_Description) +
        scale_colour_continuous( if (input$include_point_color == TRUE) {subset(codebook, codebook$Variable_Name == input$color_plot)$Variable_Description}else{} ) +
        scale_size_continuous( if (input$include_point_size == TRUE) {subset(codebook, codebook$Variable_Name == input$size_plot)$Variable_Description}else{} )

      scatter_plot_output

    })

    output$scatter_plot <- renderPlot({
      scatter_plot()
    })


    ## download button

    output$download_scatter <- downloadHandler(
      filename = function() {
        paste("MetaPipe Scatter Plot-", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file=file, width = 15, height = 7.5)
        plot(scatter_plot())
        dev.off()
      }
    )


    # tab 5: Forest Plot

    forest_plot <- reactive({

      #X <- unlist(data() %>% dplyr::select(input$x_plot))

      plot_data <- data.frame(Est = as.numeric(unlist(data() %>% dplyr::select(input$forest_data1))),
                              SE = as.numeric(unlist(data() %>% dplyr::select(input$forest_data2))),
                              Unit = unlist(data() %>% dplyr::select(input$forest_data3)))


      metaviz::viz_forest(x = plot_data[,c("Est", "SE")],
                          study_labels = plot_data[,c("Unit")],
                          summary_label = "Summary effect",
                          xlab = "Standard Deviation")

    })

    output$forest_plot <- renderPlot({
      forest_plot()
    })

    ## download button

    output$download_forest <- downloadHandler(
      filename = function() {
        paste("MetaPipe Forest Plot-", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file=file, width = 10, height = 12)
        plot(forest_plot())
        dev.off()
      }
    )

    # tab 6: Codebook Display
    output$codebook = DT::renderDT(
        codebook, options = list(lengthChange = FALSE)
    )

    # tab 6: Codebook Text

    output$codebook_text <- shiny::renderText({
        codebook_text_vec
    })


}

shiny::shinyApp(ui = ui, server = server)

