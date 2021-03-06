#' Creating Lab Summaries
#'
#'
#' @import dplyr
#' @import metafor
#' @import utils
#' @import tibble
#' @import mathjaxr
#' @import stats
#'
#'
#' @description
#' \loadmathjax{}
#' \(\let\underscore_\)
#'
#' Function to create a documentation of lab statistics. Components of the standardized mean difference and their standard errors are calculated and reported. This function is the first step of the MetaPipe pipeline. For more details on the pipeline, refer to the documentation of the MetaPipe-package.
#'
#' @param data
#' A list of data frames that contain the individual participant data. The function expects the relevant columns to be named consistently across all list objects. Relevant to this function are columns that represent information on the project (e.g., Many Labs 2), the replication (e.g., Ross1), the lab (the source a data point is assigned to), the group (either the treatment or control condition) and the single data point of the dependent variable per person.
#' @param Project
#' Character vector with the name of the columns in the list elements of "data" that contain the project name(s). If \emph{is.null(Project) == TRUE}, "Project" is chosen as the default.
#' @param Replication
#' Character vector with the name of the columns in the list elements of "data" that contain the replication name(s). If \emph{is.null(Replication) == TRUE}, "Replication" is chosen as the default. Each replication comprises a single effect with direct replications across multiple labs/sources.
#' @param Lab
#' Character vector with the name of the columns in the list elements of "data" that contain the lab names. If \emph{is.null(Lab) == TRUE}, "Lab" is chosen as the default. The meta-analyses in MetaPipe::meta_analyses() and MetaPipe::full_pipeline() are run as random effects models in metafor::rma.mv() with “random = ~ 1 | Lab”. Thus, the pipeline assumes a distribution of true statistics (e.g., treatment means, mean differences, standardized mean differences).
#' @param DV
#' Character vector with the name of the columns in the list elements of "data" that contain the (aggregated) dependent variable. If \emph{is.null(DV) == TRUE}, "DV" is chosen as the default.
#' @param Group
#' Character vector with the name of the columns in the list elements of "data" that contain the (treatment/control) group identification. If \emph{is.null(Group) == TRUE}, "Group" is chosen as the default. These should only contain values of 0 (control group), 1 (treatment group) and NA (unidentified).
#' @param output_folder
#' Specify the output folder for the summaries and the codebook. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).
#' @param suppress_list_output
#' Logical. FALSE by default. If FALSE, the function will return a list output to the environment, containing the lab summaries and the codebook. If TRUE, these are not returned to the environment.
#'
#' @details
#'
#' ### Lab Statistics
#'
#' All components of the standardized mean difference and their standard errors are returned by the function. Each standard error is returned to enable a meta-analysis on each component. The components and their standard errors are implemented as follows (unless other sources are given, effect size statistics are calculated according to Borenstein et al):
#' ## mean
#' \itemize{
  #'  \item{R-Code} \cr
    #'  \code{## apply the function} \cr
    #'  \code{# treatment group mean (T_M):} \cr
    #'  \code{mean(treatment_group$DV)} \cr
    #'  \code{# control group mean (C_M):} \cr
    #'  \code{mean(control_group$DV)} \cr
  #' \item{Model} \cr
    #' {
    #' treatment group mean:
    #' \mjdeqn{\bar{x}\underscore{T} = \frac{1}{n}\sum\underscore{i \in T} x}{}
    #' control group mean:
    #' \mjdeqn{\bar{x}\underscore{C} = \frac{1}{n}\sum\underscore{i \in C} x}{}
    #' }
#' }
#' ## standard error of the mean
#' \itemize{
  #'  \item{R-Code} \cr
    #'  \code{## define the function} \cr
    #'  \code{SE_of_mean_fct <- function(x)\{ } \cr
    #'  \code{estimated_sd <- sqrt(sum((x-mean(x))^2)/(length(x)-1))} \cr
    #'  \code{SE_of_mean <-  sd(x) / sqrt(length(x))} \cr
    #'  \code{return(SE_of_mean)\} } \cr \cr
    #'  \code{## apply the function} \cr
    #'  \code{# standard error of treatment group mean (SE_T_M):} \cr
    #'  \code{SE_of_mean_fct(treatment_group$DV)} \cr
    #'  \code{# standard error of control group mean (SE_C_M):} \cr
    #'  \code{SE_of_mean_fct(control_group$DV)} \cr
  #'  \item{Model} \cr
    #'  {
    #'  \mjdeqn{ \hat{\sigma}\underscore{\bar{x}} = \frac{\hat{\sigma}\underscore{x}}{\sqrt{n}} = \sqrt{\frac{\frac{1}{n-1}\sum\underscore{i=1}^n(x - \bar{x})^2}{n}} }{}
    #'  }
#' }
#' ## standard deviation
#' \itemize{
  #' \item{R-Code} \cr
    #' \code{## apply the function} \cr
    #' \code{# treatment group standard deviation (T_SD):} \cr
    #' \code{ sd(treatment_group$DV)} \cr
    #' \code{# control group standard deviation (C_SD):} \cr
    #' \code{sd(control_group$DV)}
  #'  \item{Model} \cr
    #' {
    #' \mjdeqn{ \hat{\sigma}  = \sqrt{ \frac{ \sum(x-\bar{x}^2) }{n-1}   } }{}
    #' }
#' }
#' ## standard error of the standard deviation
#' \itemize{
  #' \item{R-Code} \cr
    #' \code{## define the function} \cr
    #' \code{SE_SD_fct <- function(x)\{ } \cr
    #' \code{SE_SD <- sd(x) / sqrt(2*(length(x)-1)) # for large n } \cr
    #' \code{return(SE_SD) \} } \cr \cr
    #' \code{## apply the function} \cr
    #' \code{# standard error of the treatment group standard deviation (SE_T_SD):} \cr
    #' \code{SE_SD_fct(treatment_group$DV)} \cr
    #' \code{# standard error of the control group standard deviation (SE_C_SD):} \cr
    #' \code{SE_SD_fct(control_group$DV)}
  #'  \item{Model} \cr
    #' {
    #' \mjdeqn{ \hat{\sigma}\underscore{\hat{\sigma}} = \frac{\hat{\sigma}\underscore{x}}{\sqrt{2(n-1)}} = \sqrt{\frac{\frac{1}{n-1}\sum\underscore{i=1}^n(x - \bar{x})^2}{2(n-1)}} }{}
    #' \mjeqn{ \hat{\sigma}\underscore{\hat{\sigma}} }{} is a simplified version of \mjeqn{ \sigma\underscore{K\underscore{n}S} }{} in Ahn & Fessler (2003). The authors demonstrate that for n > 10 it is reasonable to use Kn = 1. As for the overwhelming majority of samples n > k may be assumed, we excluded the term \mjeqn{K\underscore{n}}{}. For more details, please refer to \cr
    #' \emph{ Ahn, S., & Fessler, J. A. (2003). Standard errors of mean, variance, and standard deviation estimators. EECS Department, The University of Michigan, 1-2.}
    #' }
#' }
#' ## mean difference (MD)
#' \itemize{
  #' \item{R-Code} \cr
    #' \code{## apply the function} \cr
    #' \code{metafor::escalc( } \cr
    #' \code{measure = "MD", } \cr
    #' \code{m1i = mean(treatment_group$DV),} \cr
    #' \code{m2i = mean(control_group$DV), } \cr
    #' \code{sd1i = sd(treatment_group$DV), } \cr
    #' \code{sd2i = sd(control_group$DV), } \cr
    #' \code{n1i = length(treatment_group$DV), } \cr
    #' \code{n2i = length(control_group$DV), } \cr
    #' \code{vtype = "HO" # assuming homoscedasticity } \cr
    #' \code{)$yi } \cr
  #'  \item{Model} \cr
    #' {
    #' \mjdeqn{ D = \bar{x}\underscore{T} -  \bar{x}\underscore{C} }{}
    #' }
#' }
#' ## standard error of mean difference (SE_MD)
#' \itemize{
  #' \item{R-Code} \cr
    #' \code{## apply the function} \cr
    #' \code{metafor::escalc( } \cr
    #' \code{measure = "MD", } \cr
    #' \code{m1i = mean(treatment_group$DV),} \cr
    #' \code{m2i = mean(control_group$DV), } \cr
    #' \code{sd1i = sd(treatment_group$DV), } \cr
    #' \code{sd2i = sd(control_group$DV), } \cr
    #' \code{n1i = length(treatment_group$DV), } \cr
    #' \code{n2i = length(control_group$DV), } \cr
    #' \code{vtype = "HO" # assuming homoscedasticity } \cr
    #' \code{)$vi } \cr
  #'  \item{Model} \cr
    #' {
    #' \mjdeqn{ \hat{\sigma}\underscore{\bar{x}\underscore{T} -  \bar{x}\underscore{C}} = \sqrt{ \frac{n\underscore{T}+ n\underscore{C}}{ n\underscore{T} n\underscore{C} } \sigma^2\underscore{TC} } = \sqrt{ \frac{n\underscore{T}+ n\underscore{C}}{ n\underscore{T} n\underscore{C} } \frac{ \sum\underscore{i = 1}^n (x\underscore{T}-\bar{x}\underscore{T})^2 + \sum\underscore{i = 1}^n (x\underscore{C}-\bar{x}\underscore{C})^2 }{ n\underscore{T} + n\underscore{C} - 2 }   } }{}
    #' }
#' }
#' ## pooled standard deviation (pooled_SD)
#' \itemize{
  #' \item{R-Code} \cr
    #' \code{## define the function} \cr
    #' \code{pooled_SD_fct <- function(t,c)\{ } \cr
    #' \code{pooled_SD <- sqrt((} \cr
    #' \code{(sum((t-mean(t))^2))+ # sample sum of squares sums treatment group} \cr
    #' \code{(sum((c-mean(c))^2)) # sample sum of squares control group)/} \cr
    #' \code{(length(t) + length(c) -2) # n+n-2} \cr
    #' \code{) # end of sqrt} \cr
    #' \code{return(pooled_SD)\} } \cr
    #' \code{## apply the function} \cr
    #' \code{pooled_SD_fct(treatment_group$DV, control_group$DV)}
  #' \item{Model} \cr
    #' {
    #' \mjdeqn{ \hat{\sigma}\underscore{TC} = \sqrt{ \frac{ \sum\underscore{i = 1}^n (x\underscore{T}-\bar{x}\underscore{T})^2 + \sum\underscore{i = 1}^n (x\underscore{C}-\bar{x}\underscore{C})^2 }{ n\underscore{T} + n\underscore{C} - 2 } } }{}
    #' }
#' }
#' ## standard error of pooled standard deviation (SE_pooled_SD)
#' \itemize{
  #' \item{R-Code} \cr
    #' \code{## define the function} \cr
    #' \code{SE_pooled_SD_fct <- function(t,c)\{ } \cr
    #' \code{pooled_SD <- sqrt((} \cr
    #' \code{(sum((t-mean(t))^2))+ # sample sum of squares sums treatment group} \cr
    #' \code{(sum((c-mean(c))^2)) # sample sum of squares control group)/} \cr
    #' \code{(length(t) + length(c) -2) # n+n-2} \cr
    #' \code{) # end of sqrt} \cr
    #' \code{SE_pooled_SD <- pooled_SD/sqrt(2*(length(t)+length(c)-1))} \cr
    #' \code{return(SE_pooled_SD)\} } \cr
    #' \code{## apply the function} \cr
    #' \code{SE_pooled_SD_fct(treatment_group$DV, control_group$DV)} \cr
    #'
  #' \item{Model} \cr
    #' {
    #' \mjdeqn{ \hat{\sigma}\underscore{\hat{\sigma}\underscore{TC}} = \frac{ \hat{\sigma}\underscore{TC} }{ \sqrt{ 2(n\underscore{T}+n\underscore{C}-1) } } }{}
    #' The standard error is equivalent to that of the standard deviation. For further information, refer to the "standard error of the standard deviation" section.
    #' }
#' }
#' ## standardized mean difference (SMD)
#' \itemize{
  #' \item{R-Code} \cr
    #' \code{## apply the function} \cr
    #' \code{metafor::escalc(} \cr
    #' \code{measure = "SMD",} \cr
    #' \code{m1i = mean(treatment_group$DV),} \cr
    #' \code{m2i = mean(control_group$DV),} \cr
    #' \code{sd1i = sd(treatment_group$DV),} \cr
    #' \code{sd2i = sd(control_group$DV),} \cr
    #' \code{n1i = length(treatment_group$DV),} \cr
    #' \code{n2i = length(control_group$DV),} \cr
    #' \code{vtype = "LS2" # Borenstein variance} \cr
    #' \code{)$yi} \cr
    #' \code{## apply the function} \cr
  #' \item{Model} \cr
    #' {
    #' \mjdeqn{ g = d \left( 1 - \frac{3}{4(n\underscore{T}+n\underscore{C}-2) -1} \right)  }{}
    #' with
    #' \mjdeqn{ d =  \frac{ \bar{x}\underscore{T} -  \bar{x}\underscore{C} }{ \sqrt{ \frac{ \sum\underscore{i = 1}^n (x\underscore{T}-\bar{x}\underscore{T})^2 + \sum\underscore{i = 1}^n (x\underscore{C}-\bar{x}\underscore{C})^2 }{ n\underscore{T} + n\underscore{C} - 2 } }   }}{}
    #' }
#' }
#' ## standard error of standardized mean difference (SE_SMD)
#' \itemize{
  #' \item{R-Code} \cr
    #' \code{## apply the function} \cr
    #' \code{sqrt(metafor::escalc(} \cr
    #' \code{measure = "SMD",} \cr
    #' \code{m1i = mean(treatment_group$DV),} \cr
    #' \code{m2i = mean(control_group$DV),} \cr
    #' \code{sd1i = sd(treatment_group$DV),} \cr
    #' \code{sd2i = sd(control_group$DV),} \cr
    #' \code{n1i = length(treatment_group$DV),} \cr
    #' \code{n2i = length(control_group$DV),} \cr
    #' \code{vtype = "LS2" # Borenstein variance} \cr
    #' \code{)$vi)} \cr
    #' \code{## apply the function} \cr
  #' \item{Model} \cr
    #' {
    #' \mjdeqn{ \hat{\sigma}\underscore{g} = \sqrt{ \hat{\sigma}\underscore{d}^2 \left( 1 - \frac{3}{4(n\underscore{T}+n\underscore{C}-2) -1} \right)^2 } }{}
    #' with
    #' \mjdeqn{ \hat{\sigma}\underscore{d}^2 = \frac{n\underscore{T}+n\underscore{C}}{n\underscore{T}n\underscore{C}} + \frac{d^2}{2(n\underscore{T}+n\underscore{C})} }{}
    #' }
#' }
#'
#'
#'
#' @return
#' The function create_lab_summaries returns a list consisting of two elements: A codebook and a list of data frames. Each data frame contains all lab summary statistics for the according Replication/Effect.
#' The summary statistics returned (including their standard error) are the means and standard deviations for control and experimental groups, pooled standard deviations, raw mean differences and standardized mean differences (Hedge's g according to Borenstein).
#'
#' #### Example
#'
#' For an example, please refer to the github repository:
#' https://github.com/JensFuenderich/MetaPipe/blob/main/Supplementary%20Material/Code%20Examples/create_lab_summaries().R
#'
#'
#' @export

create_lab_summaries <- function(data, Project = NULL, Replication = NULL, Lab = NULL, DV = NULL, Group = NULL, output_folder, suppress_list_output = FALSE){

  ## use standard column names in case is.null("column name") == TRUE
  if (is.null(Project) == TRUE) {
    Project <- "Project"
  }
  if (is.null(Replication) == TRUE) {
    Replication <- "Replication"
  }
  if (is.null(Lab) == TRUE) {
    Lab <- "Lab"
  }
  if (is.null(DV) == TRUE) {
    DV <- "DV"
  }
  if (is.null(Group) == TRUE) {
    Group <- "Group"
  }

  ## renaming all list object columns according to function input
  # creating a function to rename the columns
  renamer <- function(x){
    data[[x]] %>%
      rename(Project = {{ Project }},
             Replication = {{ Replication }},
             Lab = {{ Lab }},
             DV = {{ DV }},
             Group = {{ Group }})
  }
  # applying the function
  data_List <- lapply(1:length(data), renamer)
  # renaming the list according to original data list    ###### MIGHT BE IRRELEVANT
  names(data_List) <- names(data)


  ## a function to create a summary for a single lab, returns a df with one row
  single_lab_summary <- function(data){

    # split the lab df into treatment and control group
    treatment_group <- subset(data, data$Group == 1)
    control_group <- subset(data, data$Group == 0)

    # create empty df to be filled by the function
    lab.df <- data.frame(t(rep(0,16)))

    # create vector with column names
    names(lab.df) <- c(
      # sample sizes
      "T_N",
      "C_N",
      # means
      "T_M", "SE_T_M",
      "C_M", "SE_C_M",
      # SDs
      "T_SD", "SE_T_SD",
      "C_SD", "SE_C_SD",
      # raw mean difference
      "MD", "SE_MD",
      # pooled SD
      "pooled_SD", "SE_pooled_SD",
      # standardized mean differences
      "SMD", "SE_SMD"
      )

    ## calculate and insert the statistics and their standard errors into the lab.df

    ## calculating means

    # 1.1: getting to T_M
    lab.df["T_M"]<- if (length(treatment_group$DV) < 2) {
      treatment_group$DV
    }else{
      mean(treatment_group$DV)
    }

    # 2.1: getting to C_M
    lab.df["C_M"] <- if (length(control_group$DV) < 2) {
      control_group$DV
    }else{
      mean(control_group$DV)
    }

    ## calculating standard errors of means

    # custom function for standard error of the mean
    SE_of_mean_fct <- function(x){
      estimated_sd <- sqrt(sum((x-mean(x))^2)/(length(x)-1))
      SE_of_mean <-  sd(x) / sqrt(length(x))
      return(SE_of_mean)
    }

    # 1.2: getting to SE_T_M
    lab.df["SE_T_M"] <- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      SE_of_mean_fct(treatment_group$DV)
    }

    # 2.2: getting to SE_C_M
    lab.df["SE_C_M"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      SE_of_mean_fct(control_group$DV)
    }

    ## calculating standard deviations

    # 3.1: getting to T_SD
    lab.df["T_SD"] <- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      sd(treatment_group$DV)
    }

    # 4.1: getting to C_SD
    lab.df["C_SD"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      sd(control_group$DV)
    }

    ## calculating standard errors of standard deviations

    # custom function for standard error of the standard deviation
    SE_SD_fct <- function(x){
      SE_SD <- sd(x) / sqrt(2*(length(x)-1)) # for large n
      return(SE_SD)
    }

    # 3.2: getting to SE_T_SD
    lab.df["SE_T_SD"]<- if (length(treatment_group$DV) < 2) {
      NA
    }else{
      SE_SD_fct(treatment_group$DV)
    }

    # 4.2: getting to SECSD
    lab.df["SE_C_SD"] <- if (length(control_group$DV) < 2) {
      NA
    }else{
      SE_SD_fct(control_group$DV)
    }

    ## calculating mean difference

    # 5.1: getting to MD
    lab.df["MD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
      NA
    }else{
      metafor::escalc(measure = "MD",
                      m1i = mean(treatment_group$DV),
                      m2i = mean(control_group$DV),
                      sd1i = sd(treatment_group$DV),
                      sd2i = sd(control_group$DV),
                      n1i = length(treatment_group$DV),
                      n2i = length(control_group$DV),
                      vtype = "HO" # assuming homoscedasticity
      )$yi
    }

    ## calculating standard error of mean difference

    # 5.2: getting to SE_MD
    lab.df["SE_MD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
      NA
    }else{
      sqrt(metafor::escalc(measure = "MD",
                           m1i = mean(treatment_group$DV),
                           m2i = mean(control_group$DV),
                           sd1i = sd(treatment_group$DV),
                           sd2i = sd(control_group$DV),
                           n1i = length(treatment_group$DV),
                           n2i = length(control_group$DV),
                           vtype = "HO" # assuming homoscedasticity

      )$vi)
    }

    ## calculating pooled standard deviation

    # function for pooled SD
    pooled_SD_fct <- function(t,c){
      pooled_SD <- sqrt(
        (
          (sum((t-mean(t))^2))+ # sample sum of squares treatment group
            (sum((c-mean(c))^2)) # sample sum of squares control group
          )/
          (length(t) + length(c) -2) # n+n-2
        ) # end of sqrt
      return(pooled_SD)
    }

    # 6.1: getting to pooled_SD
    lab.df["pooled_SD"] <- if (length(treatment_group$DV) < 2 & length(control_group$DV) < 2) {
      NA
    }else{
      pooled_SD_fct(treatment_group$DV, control_group$DV)
    }

    ## calculating standard error of pooled standard deviation

    # creating custom function estimating standard error as estimate of a common population
    SE_pooled_SD_fct <- function(t,c){
      pooled_SD <- sqrt(
        (
          (sum((t-mean(t))^2))+ # sample sum of squares treatment group
            (sum((c-mean(c))^2)) # sample sum of squares control group
        )/
          (length(t) + length(c) -2) # n+n-2
      ) # end of sqrt
      SE_pooled_SD <- pooled_SD/sqrt(2*(length(t)+length(c)-1))
      return(SE_pooled_SD)
    }

    # 6.2: getting to SE_pooled_SD
    lab.df["SE_pooled_SD"] <- if (length(treatment_group$DV) < 2 & length(control_group$DV) < 2) {
      NA
    }else{
      SE_pooled_SD_fct(treatment_group$DV, control_group$DV)
    }

    # 8.1 getting to SMD (Borenstein's g)
    lab.df["SMD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
      NA
    }else{
      metafor::escalc(measure = "SMD",
                      m1i = mean(treatment_group$DV),
                      m2i = mean(control_group$DV),
                      sd1i = sd(treatment_group$DV),
                      sd2i = sd(control_group$DV),
                      n1i = length(treatment_group$DV),
                      n2i = length(control_group$DV),
                      vtype = "LS2" # Borenstein variance
      )$yi
    }

    # 8.2 getting to SE_SMD
    lab.df["SE_SMD"] <- if (length(treatment_group$DV) < 1 | length(control_group$DV) < 1) {
      NA
    }else{
      sqrt(metafor::escalc(measure = "SMD",
                           m1i = mean(treatment_group$DV),
                           m2i = mean(control_group$DV),
                           sd1i = sd(treatment_group$DV),
                           sd2i = sd(control_group$DV),
                           n1i = length(treatment_group$DV),
                           n2i = length(control_group$DV),
                           vtype = "LS2" # Borenstein variance
      )$vi)
    }

    # 9.1 n of Treatment
    lab.df["T_N"] <- length(treatment_group$DV)

    # 9.1 n of Control
    lab.df["C_N"] <- length(control_group$DV)

    # add descriptive columns
    descriptive_columns <- data.frame(Project = unique(data$Project),
                                      Replication = unique(data$Replication),
                                      Lab = unique(data$Lab))
    lab.df <- cbind(descriptive_columns, lab.df)

    return(lab.df)

  }

  ## create a nested list object which split the replication dfs into lab dfs
  # create the function
  split_Labs <- function(data_Frame){
    data_Frame %>%
      split(., .$Lab)
  }
  # apply the function
  # data_List_Nested_Labs <- purrr::map(data_List, split_Labs)
  data_List_Nested_Labs <- lapply(data_List, split_Labs)

  ## perform the lab summaries
  # create a function that applies the single_lab_summary function to all replications
  create_summaries <- function(x){
    Single_Replication <- data_List_Nested_Labs[[x]]
    # dplyr::bind_rows(purrr::map(Single_Replication, single_lab_summary))
    dplyr::bind_rows(lapply(Single_Replication, single_lab_summary))
  }
  # apply the function
  List_of_Lab_Summaries_per_Replication <- lapply(1:length(data_List_Nested_Labs), create_summaries)

  ## rename the output with Project and Replication name
  # create vector with names
  names_for_list <- unlist(lapply(1:length(List_of_Lab_Summaries_per_Replication), function(x){
    paste(unique(List_of_Lab_Summaries_per_Replication[[x]]$Project), "_", unique(List_of_Lab_Summaries_per_Replication[[x]]$Replication), sep = "")
    }))
  # rename output
  names(List_of_Lab_Summaries_per_Replication) <- names_for_list

  ### Create codebook

  # create df with abbreviations and explanations
  abbr_library <- as.data.frame(base::rbind(c("T_", "treatment group_"),
                                            c("C_", "control group_"),
                                            c("_N", "_number of participants"),
                                            c("_K", "_number of labs"),
                                            c("_MD", "_mean difference"),
                                            c("_Model_Estimate_", "_model estimate for_"),
                                            c("_M", "_mean"),
                                            c("_SD", "_standard deviation"),
                                            c("SE_", "standard error of the_"),
                                            c("SMD", "standardized mean difference"),
                                            c("pooled_", "pooled_"),
                                            c("Lab__", "lab level:__")
  ))
  # rename columns of the df
  names(abbr_library) <- c("Abbreviation", "Full Name")

  description_vector <- names(List_of_Lab_Summaries_per_Replication[[1]]) # arbitrary selection of replication, just a place to get the column names from

  # sorry for this, did not want to loop
  # check if there's enough pipes in that orchestra
  #nrow(abbr_library)

  description_vector %<>%
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
    gsub(abbr_library$Abbreviation[12], abbr_library$`Full Name`[12], .)

  description_vector <- sub(pattern = "_", replacement = " ", description_vector)

  codebook <- data.frame(Variable_Name = names(List_of_Lab_Summaries_per_Replication[[1]]), Variable_Description = description_vector)
  codebook <- codebook[-c(1:2),]

  # do this one by hand, otherwise the abbr "MD" messes up the code
  codebook[codebook$Variable_Name == "MD",2] <- "mean difference"


  ## Outputs

  if (missing(output_folder)) {

    base::print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    # create function
    export_fun <- function(x){
      replication_data <- List_of_Lab_Summaries_per_Replication[[x]]
      project_name <- unique(replication_data$Project)
      replication_name <- unique(replication_data$Replication)
      write.csv(replication_data,
                paste(output_folder, project_name, "_", replication_name, "_lab_summaries.csv", sep = ""),
                row.names = FALSE)
    }
    # sorry for the loop, this way I didn't have to struggle with suppressing some output
    for (i in 1:length(List_of_Lab_Summaries_per_Replication)) {
      export_fun(i)
    }
    # export codebook
    write.csv(codebook,
              paste(output_folder, "codebook_for_merged_lab_summaries.csv", sep = ""),
              row.names = FALSE)

  }

  if (suppress_list_output == TRUE) {

    base::print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and merged lab summaries.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(List_of_Lab_Summaries_per_Replication, codebook)

    # rename list elements
    names(output) <- c("lab_summaries","codebook")

    # return the output (function aborts here)
    return(output)

  }


}
