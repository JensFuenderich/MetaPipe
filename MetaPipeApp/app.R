#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


##### Aktuelles To Do:

## full_pipeline output so verändern, dass der die Informationen enthält die die App braucht (also die aus den Laboren! )




###### TO DOs
## DONE:
### 1. Ordner mit leerer "Final Data Frame" Datei und gefüllter codebook Datei erstellen
### 2. versuchen die Dateien in server Part der app einzulesen (möglicherweise auch in ui, oder beides) <- ist außerhalb dessen, müsste aber funktionieren: https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/ (siehe "Execution")
### 3. Upload Seite erstellen: IPD und Aggregated Data Upload erstmal nur als leere Felder, MA Data Upload funktionsfähig schreiben
### 4. Upload mit "Meta Analysis Data Selection" verknüpfen
### 5. Test der bisherigen Funktionsweise

## OPEN:
### 7. Meta Regression überall rausnehmen
### 8. MetaPipe Befehle finalisieren und Dokumentation fertigstellen
### 9. MetaPipe über github/gitlab verfügbar machen
### 10. MetaPipe Befehle in die App einarbeiten (in Upload Data)
### 11. Funktionalität von "Upload Data" finalisieren
### 12. Plot-Funktionen finalisieren
#### Veröffentlichungsweg (Packet: CRAN, App: ?) klären (und Zusammenhang mit Publikation)

library(MetaPipe)

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
               "shinythemes")

### general imports
MA_data <- readr::read_csv(here::here("Data","Input","MetaPipeApp Data Empty.csv"))[,-1]
#IPD_MA_data / MR_data <- readr::read_csv(here::here("Data","Input",""))[,-1]
codebook <- readr::read_csv(here::here("Data","Input","MetaPipeApp codebook.csv"))[,-1]
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
                           "exclude" = "exclude"
    ),
    Statistics = list("Control Mean" = "C_M",
                      "Treatment Mean" = "T_M",
                      "Control SD" = "C_SD",
                      "Treatment SD" = "T_SD",
                      "pooled SD" = "pooled_SD",
                      "MD" = "_MD",
                      "SMD" = "_SMD",
                      "exclude" = "exclude"
    ),
    Sample_Size = list("N" = "_N",
                       "K" = "_K",
                       "exclude" = "exclude"
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
                               label = "Which type of data do you want to upload?",
                               choices = c("Individual Participant Data" = "IPD",
                                           "Aggregated Data" = "AggrD",
                                           "Meta-Analysis Data" = "MAD"),
                               selected = "MAD"
                               ),
            actionButton("confirm_upload", "confirm upload of selected data")
        ),

        mainPanel(

            ## panel for upload of IPD
            conditionalPanel(condition = "input.select_upload == 'IPD'",
                             h4("Individual Participant Data"),
                             fileInput("IPD", "choose .csv file with individual participant data",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             h5("rename columns of the data frame(s)"),
                             textInput("project_col", "Project:"),
                             textInput("replication_col", "Replication:"),
                             textInput("lab_col", "Lab:"),
                             textInput("DV_col", "DV:"),
                             textInput("group_col", "Group:"),
                             textInput("output_folder_set", "output_folder:")
                             ),

            ## panel for upload of aggregated data
            conditionalPanel(condition = "input.select_upload == 'AggrD'",
                             h4("Aggregated Data"),
                             fileInput("AggrD", "choose .csv file with individual participant data",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))
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
                checkboxGroupInput(inputId = "Statistics",
                                   label = h3("Choose statistics of interest"),
                                   choices = Variables_List$Statistics,
                                   selected = "exclude"
                ),
                h3("Exclude Further Information"),
                shinyWidgets::materialSwitch(inputId = "Stat_SE",
                                             label = "Exclude Standard Error of Lab Level Statistic?",
                                             status = "success"),
                shinyWidgets::materialSwitch(inputId = "Stat_ln",
                                             label = "Exclude the Logarithms of Standard Deviations?",
                                             status = "success"),
                checkboxGroupInput(inputId = "AnalysisResults",
                                   label = h3("Display Analysis results"),
                                   choices = Variables_List$AnalysisResults,
                                   selected = "exclude"
                ),
                checkboxGroupInput(inputId = "SampleSize",
                                   label = h3("Sample Size Information"),
                                   choices = Variables_List$Sample_Size,
                                   selected = "exclude"
                ),
                h3("Exclude Non-Effects?"),
                shinyWidgets::materialSwitch(inputId = "exclude_0",
                                             label = "Exclude Model Estimate |g| < 0.1?",
                                             status = "success")
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
                     plotOutput(outputId = "histogram")
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
                     shiny::checkboxInput(inputId = "violin_include_dot_size",
                                          label = "Point Size"),
                     shiny::varSelectInput(inputId = "violin_dot_size",
                                           label = "choose a statistic for the dot size",
                                           data = Reactive_Output_Data)


                 ),
                 mainPanel(
                     h4("Vioin Plot for selected statistics"),
                     plotOutput(outputId = "violin_plot")
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
                     checkboxInput(inputId = "include_point_aes", label = "Point Aesthetics"),
                     shiny::varSelectInput(inputId = "size_plot", label = "choose a statistic for the dot size", data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "color_plot", label = "choose a statistic for the dot color", data = Reactive_Output_Data),
                     checkboxInput(inputId = "include_custom_lims", label = "Use Custom Axis Limits (essentially zooming in or out) and update correlation"),
                     numericInput(inputId = "x_min_plot", label = "Minimum of X-Axis", value = 0),
                     numericInput(inputId = "x_max_plot", label = "Maximum of X-Axis", value = 100),
                     numericInput(inputId = "y_min_plot", label = "Minimum of Y-Axis", value = 0),
                     numericInput(inputId = "y_max_plot", label = "Maximum of Y-Axis", value = 100)
                 ),
                 mainPanel(
                     h4("Scatter Plot for selected statistics"),
                     plotOutput(outputId = "scatter_plot")
                 )
             )
    ),

    ## tab for Forrest Plots

    tabPanel("Forrest Plots",
             shiny::sidebarLayout(
                 sidebarPanel(
                     actionButton(inputId = "upload_forrest",
                                  label = "Upload Data"),
                     shiny::varSelectInput(inputId = "forrest_data1",
                                           label = "choose a statistic of interest",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "forrest_data2",
                                           label = "choose the according standard error",
                                           data = Reactive_Output_Data),
                     shiny::varSelectInput(inputId = "forrest_data3",
                                           label = "choose information on aggregation (likely the lab)",
                                           data = Reactive_Output_Data)
                 ),
                 shiny::mainPanel(
                     h4("Forrest Plot for selected statistics"),
                     shiny::plotOutput(outputId = "forrest_plot")
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

    ### Data Input

    IPD_data_input <- reactive({

        if (length(input$IPD) > 0) {
            upload_info <- input$IPD

            IPD_list <- lapply(upload_info$datapath,readr::read_csv)

            IPD_analzed <- MetaPipe::full_pipeline(Data = IPD_list,
                                                   Project = input$project_col,
                                                   Replication = input$replication_col,
                                                   Lab = input$lab_col,
                                                   DV = input$DV_col,
                                                   Group = input$group_col,
                                                   output_folder = "Data/Output/",
                                                   folder_name = "Meta_Pipe_Output")

            IPD_analzed$MetaPipe_Data

        } else {

        }

    })

    MAD_data_input <- reactive({
        upload_info <- input$MAD
        readr::read_csv(file = upload_info$datapath)
    })

    MA_data <- eventReactive( input$confirm_upload, {
        if (input$select_upload == "MAD") {
            MAD_data_input()
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
        if (input$exclude_0 == TRUE) {
            df <- subset(df, abs(df$MA__Model_Estimate__SMD_g) > 0.1 )
        }

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
        write.csv(df, file = here::here("Data", "Output", "Reactive Output.csv"))
        df

    })

    ## download button

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Data Selection-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(data(), file)
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
        updateVarSelectInput(session, "violin_dot_size",
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

    # forrest plot
    observeEvent(input$upload_forrest, {
        Reactive_Output_Data <- readr::read_csv(here::here("Data", "Output", "Reactive Output.csv"))[,-1]

        updateVarSelectInput(session, "forrest_data1",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "forrest_data2",
                             data = Reactive_Output_Data)
        updateVarSelectInput(session, "forrest_data3",
                             data = Reactive_Output_Data)

    })


    ## create output

    # tab 1: Reactive Data Table
    output$table = DT::renderDT(
        data(), options = list(lengthChange = FALSE)
    )

    # tab 2: Histogram
    output$histogram <- renderPlot({

        # select data from first input for the histogram
        data1 <- unlist(data() %>% dplyr::select(input$hist_data1))
        hist_data1 <- data.frame(Data = data1,
                                 Statistic = rep(as.character(input$hist_data1), times = length(data1)))
        # select data from second input for the histogram
        data2 <- unlist(data() %>% dplyr::select(input$hist_data2))
        hist_data2 <- data.frame(Data = data2,
                                 Statistic = rep(as.character(input$hist_data2), times = length(data2)))
        # select data from third input for the histogram
        data3 <- unlist(data() %>% dplyr::select(input$hist_data3))
        hist_data3 <- data.frame(Data = data3,
                                 Statistic = rep(as.character(input$hist_data3), times = length(data3)))

        if (input$hist_include_variable2 == FALSE & input$hist_include_variable3 == FALSE) {
            hist_data <- hist_data1
        } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == FALSE) {
            hist_data <- rbind(hist_data1, hist_data2)
        } else if (input$hist_include_variable2 == FALSE & input$hist_include_variable3 == TRUE) {
            hist_data <- rbind(hist_data1, hist_data3)
        } else if (input$hist_include_variable2 == TRUE & input$hist_include_variable3 == TRUE) {
            hist_data <- rbind(hist_data1, hist_data2, hist_data3)
        }

        hist_plot_output <- ggplot2::ggplot(hist_data, aes(x = Data, fill = Statistic, color = Statistic)) + geom_histogram(position="identity",alpha = 0.7)
        hist_plot_output

    })

    # tab 3: Violin Plot
    output$violin_plot <- renderPlot({


        # select data from first input for the histogram
        data1 <- unlist(data() %>% dplyr::select(input$violin_1))
        violin_1 <- data.frame(Data = data1,
                               Statistic = rep(as.character(input$violin_1), times = length(data1)))

        # select data from first input for the histogram
        data2 <- unlist(data() %>% dplyr::select(input$violin_2))
        violin_2 <- data.frame(Data = data2,
                               Statistic = rep(as.character(input$violin_2), times = length(data2)))

        # select data from first input for the histogram
        data3 <- unlist(data() %>% dplyr::select(input$violin_3))
        violin_3 <- data.frame(Data = data3,
                               Statistic = rep(as.character(input$violin_3), times = length(data3)))

        # select data from first input for the histogram
        data4 <- unlist(data() %>% dplyr::select(input$violin_4))
        violin_4 <- data.frame(Data = data4,
                               Statistic = rep(as.character(input$violin_4), times = length(data4)))

        # select data from first input for the histogram
        data5 <- unlist(data() %>% dplyr::select(input$violin_5))
        violin_5 <- data.frame(Data = data5,
                               Statistic = rep(as.character(input$violin_5), times = length(data5)))

        # select data from first input for the histogram
        data6 <- unlist(data() %>% dplyr::select(input$violin_6))
        violin_6 <- data.frame(Data = data6,
                               Statistic = rep(as.character(input$violin_6), times = length(data6)))


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

        if (input$violin_include_dot_size == FALSE) {
            violin_data$dot_size <- rep(1, nrow(violin_data) )
        } else if (input$violin_include_dot_size == TRUE ) {
            violin_data$dot_size <- rep(unlist(data() %>% dplyr::select(input$violin_dot_size)), input$include_violins)
        }


        p <- ggplot(violin_data, aes(x=Statistic, y=Data)) +
            geom_violin(trim=TRUE) #+ scale_y_discrete(expand = c(3,5))

        violin_plot_output <- p + geom_boxplot(width = 0.1) +
            geom_jitter(width = 0.3, aes(color = Statistic,
                                         size = violin_data$dot_size
            ), alpha = 0.7) +
            guides( color = FALSE) +
            theme_light() +
            theme(text = element_text(size=15)) +
            theme(axis.title.x = element_blank())

        violin_plot_output

    })


    # tab 4: Scatter Plot
    output$scatter_plot <- renderPlot({

        # Plot Data
        X <- unlist(data() %>% dplyr::select(input$x_plot))
        Y <- unlist(data() %>% dplyr::select(input$y_plot))
        Point_Size <- as.factor(unlist(data() %>% dplyr::select(input$size_plot)))
        Point_Color <- as.factor(unlist(data() %>% dplyr::select(input$color_plot)))
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
            geom_point(if (input$include_point_aes == TRUE) {aes(colour = Point_Color, size = Point_Size)}else{}) +
            ggtitle(glue::glue("Correlation between currently depicted X and Y data: {round(unique(plot_data$cor_x_y), digits = 2)}"))

        scatter_plot_output


    })

    # tab 5: Forrest Plot
    output$forrest_plot <- renderPlot({

        #X <- unlist(data() %>% dplyr::select(input$x_plot))

        plot_data <- data.frame(Est = as.numeric(unlist(data() %>% dplyr::select(input$forrest_data1))),
                                SE = as.numeric(unlist(data() %>% dplyr::select(input$forrest_data2))),
                                Unit = unlist(data() %>% dplyr::select(input$forrest_data3)))


        metaviz::viz_forest(x = plot_data[,c("Est", "SE")],
                            study_labels = plot_data[,c("Unit")],
                            summary_label = "Summary effect",
                            xlab = "Standard Deviation")

    })

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

