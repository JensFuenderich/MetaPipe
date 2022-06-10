#' Meta Analyses
#'
#'
#' @import metafor
#' @import purrr
#' @import dplyr
#' @import mathjaxr
#'
#' @description
#' \loadmathjax{}
#' \(\let\underscore_\)
#' Function to run meta-analyses one the standardized mean difference and its components. The meta-analyses are \cr
#' [...] \cr
#' This function is the first step of the MetaPipe pipeline. For more details on the pipeline, refer to the documentation of the MetaPipe-package.
#'
#'
#' @param data
#' The function expects the input to be a data frame. The input may either be the data frame produced by the MetaPipe::merge_lab_summaries() function, or one with the same columns names. A template of this data frame is available at [OSF LINK], as is a codebook for unambiguous identification of the abbreviations:  [OSF LINK].
#' @param output_folder
#' ???
#' @param suppress_list_output
#' ???
#' @param method
#' A character string to specify the type of model to be fittet. Default ist “REML”. For more details, refer to the metafor documentation.
#'
#' @details
#'
#' All meta-analyses within the function are written with metafor::rma.mv. The multivariate version of the rma function is deployed to enable the use of sparse matrices (“sparse = TRUE”) for optimal performance, where possible. They are fitted as a random-effects model with “random = ~ 1 | Lab” and a restricted maximum likelihood estimation (“REML”).
#' The function runs seven meta-analyses per replication:
#' \itemize{
  #'  \item{treatment mean (yi = T_M, sei = SE_T_M)}
  #'  \item{control mean (yi = C_M, sei = SE_C_M)}
  #'  \item{treatment standard deviation (yi = T_SD, sei = SE_T_SD)}
  #'  \item{control standard deviation (yi = C_SD, sei = SE_C_SD)}
  #'  \item{mean difference (yi = MD, sei = SE_MD)}
  #'  \item{pooled standard deviation (yi = pooled_SD, sei = SE_pooled_SD)}
  #'  \item{standardized mean difference (yi = SMD, sei = SE_SMD)}
#'  }
#'
#'
#' @return
#' The output is a list with two objects: A data frame with the meta-analytical results and a codebook for unambiguous identification of its columns.
#' ## meta analyses
#' The data frame contains information to identify each analysis (Project, Replication) and statistical output from the seven meta-analyses per replication. The statistical output for each meta-analysis includes:
#' \itemize{
  #' \item{A model estimate for the y of interest (Model_Estimate__).}
  #' \item{The number of labs included in the analysis (Empirical__K).}
  #' \item{The estimated \mjeqn{\tau^2}{} (sigma2 from the rma.mv object) value (Tau2__).}
  #' \item{The estimated \mjeqn{\tau}{} (the square root of the sigma2 from the rma.mv object) value (Tau2__).}
  #' \item{The estimated \mjeqn{I^2}{} value. \mjeqn{I^2}{} is not part of the rma.mv output object and has to be calculated from \mjeqn{\tau}{}.
  #' \mjdeqn{ I^2 = 100 \frac{ \hat{\tau}^2  }{ \hat{\tau}^2 + \tilde{v}} }{}
  #' with
  #' \mjdeqn{ \tilde{v} = \frac{(k-1)\sum w\underscore{i}}{\left(\sum  w\underscore{i}\right)-\sum w\underscore{i}^2} }{}
  #' Transformation according to: https://wviechtb.github.io/metafor/reference/print.rma.html
  #' }
  #' \item{The estimated \mjeqn{H^2}{} value. \mjeqn{H^2}{} is not part of the rma.mv output object and has to be calculated from \mjeqn{\tau}{}.
  #' \mjdeqn{ H^2 = 100 \frac{ \hat{\tau}^2 + \tilde{v} }{\tilde{v}} }{}
  #' with
  #' \mjdeqn{ \tilde{v} = \frac{(k-1)\sum w\underscore{i}}{\left(\sum  w\underscore{i}\right)-\sum w\underscore{i}^2} }{}
  #' }
  #' \item{The Q statistic (QE__).}
  #' \item{The p-value from the test on the Q statistic (QEp__).}
#' }
#' ## codebook
#' A codebook that applies to the data frame (meta_analyses).
#'
#' @export
#'




## aktuell produziert die Funktion eine genestete Liste und ein df, ich nehme aber nur das df als Output

meta_analyses <- function(data, output_folder, suppress_list_output = FALSE, method = "REML"){

  ## input is a large df with all projects & replications


  ### Run meta-analyses

  ## create a function that runs all meta-analyses for one replication/effect

  single_replication_analyses <- function(subset_Replication){

    # create a vector with the column names for the analysis
    col_names <- c(
      # N per Replication & K (Number of Labs):
      "Empirical__K",
      "Empirical__N",
      # Meta-analytic Estimates:
      "Model_Estimate__C_M",
      "Model_Estimate__T_M",
      "Model_Estimate__C_SD",
      "Model_Estimate__T_SD",
      "Model_Estimate__pooled_SD",
      "Model_Estimate__MD",
      "Model_Estimate__SMD",
      # K of Meta-analytic Estimates:
      "Model_Estimate__C_M_K",
      "Model_Estimate__T_M_K",
      "Model_Estimate__C_SD_K",
      "Model_Estimate__T_SD_K",
      "Model_Estimate__pooled_SD_K",
      "Model_Estimate__MD_K",
      "Model_Estimate__SMD_K",
      # Tau2 Analyses and SE of Tau2:
      "Tau2__C_M",
      "Tau2__T_M",
      "Tau2__C_SD",
      "Tau2__T_SD",
      "Tau2__pooled_SD",
      "Tau2__MD",
      "Tau2__SMD",
      # Tau Analyses:
      "Tau__C_M",
      "Tau__T_M",
      "Tau__C_SD",
      "Tau__T_SD",
      "Tau__pooled_SD",
      "Tau__MD",
      "Tau__SMD",
      # I2 Analyses:
      "I2__T_M",
      "I2__C_M",
      "I2__T_SD",
      "I2__C_SD",
      "I2__MD",
      "I2__pooled_SD",
      "I2__SMD",
      # H2 Analyses:
      "H2__T_M",
      "H2__C_M",
      "H2__TSD",
      "H2__C_SD",
      "H2__MD",
      "H2__pooled_SD",
      "H2__SMD",
      # QE Values:
      "QE__T_M",
      "QE__C_M",
      "QE__T_SD",
      "QE__C_SD",
      "QE__MD",
      "QE__pooled_SD",
      "QE__SMD",
      # QEp Values:
      "QEp__T_M",
      "QEp__C_M",
      "QEp__T_SD",
      "QEp__C_SD",
      "QEp__MD",
      "QEp__pooled_SD",
      "QEp__SMD"
    )


    # create a df for the results of the analysis from the subset
    Replication.df <- data.frame(t(rep(0,length(col_names))))
    # rename columns
    names(Replication.df) <- col_names

    ## replace infinite values (Inf) in input df with NA
    subset_Replication <- do.call(data.frame, lapply(subset_Replication, function(value){replace(value, is.infinite(value),NA)}))

    ## insert information on sample sizes and number of labs

    # N
    Replication.df["Empirical__N"] <- sum(subset_Replication$T_N + subset_Replication$C_N)
    # K
    Replication.df["Empirical__K"] <- length(subset_Replication$Lab)

    ## Transformations for rma.mv output, which does not include I2 and H2
    # transformations according to https://cran.r-project.org/web/packages/metafor/metafor.pdf

    # function for estimate of v
    v_est_fct <- function(rma_mv_obj, data){
      k <- nrow(data)
      # this version seems to be inefficient, but the original version with diag() crashes and returns "long vectors not supported yet"
      (k -1) * sum(1/rma_mv_obj$V[col(rma_mv_obj$V)==row(rma_mv_obj$V)]) / ( ( sum(1/rma_mv_obj$V[col(rma_mv_obj$V)==row(rma_mv_obj$V)] ) )^2 - sum((1/rma_mv_obj$V[col(rma_mv_obj$V)==row(rma_mv_obj$V)])^2) )
      # original version:
      #(k -1) * sum(1/diag(rma_mv_obj$V)) / ( ( sum(1/diag(rma_mv_obj$V) ) )^2 - sum((1/diag(rma_mv_obj$V))^2) )
    }

    I2_fct <- function(rma_mv_obj, v_est){
      # calculating I2 from tau and v of rma.mv output
      rma_mv_obj$sigma2 / (rma_mv_obj$sigma2 + v_est) * 100
    }


    H2_fct <- function(rma_mv_obj, v_est){
      # calculating H2 from tau and v of rma.mv output
      (rma_mv_obj$sigma2 + v_est) / v_est
    }

    ## run meta-analyses (currently on 8 statistics) and fill "Replication.df" with the output

    # 1 Heterogeneity of treatment group mean
    if ( nrow(na.omit(subset_Replication[, c("Lab", "T_M", "SE_T_M")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_T_M <- metafor::rma.mv(yi = T_M,
                                 V = SE_T_M^2,
                                 random = ~ 1 | Lab,
                                 method = method,
                                 sparse = TRUE,
                                 data = na.omit(subset_Replication[, c("Lab", "T_M", "SE_T_M")]))
      # v estimate for calculating I2 and H2
      v_estimate <- v_est_fct(rma_mv_obj = Het_T_M,
                              data = na.omit(subset_Replication[, c("Lab", "T_M", "SE_T_M")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Model_Estimate__T_M"] <- Het_T_M$b
      Replication.df["Model_Estimate__T_M_K"] <- Het_T_M$k
      Replication.df["Tau2__T_M"] <- Het_T_M$sigma2
      Replication.df["Tau__T_M"] <- sqrt(Het_T_M$sigma2)
      Replication.df["I2__T_M"] <- I2_fct(rma_mv_obj = Het_T_M, v_est = v_estimate)
      Replication.df["H2__T_M"] <- H2_fct(rma_mv_obj = Het_T_M, v_est = v_estimate)
      Replication.df["QE__T_M"] <- Het_T_M$QE
      Replication.df["QEp__T_M"] <- Het_T_M$QEp

      rm(Het_T_M)

    }


    # 2 Heterogeneity of control group mean
    if ( nrow(na.omit(subset_Replication[, c("Lab", "C_M", "SE_C_M")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_C_M <- metafor::rma.mv(yi = C_M,
                                 V = SE_C_M^2, random = ~ 1 | Lab,
                                 method = method,
                                 sparse = TRUE,
                                 data = na.omit(subset_Replication[, c("Lab", "C_M", "SE_C_M")]))
      # v estimate for calculating I2 and H2
      v_estimate <- v_est_fct(rma_mv_obj = Het_C_M,
                              data = na.omit(subset_Replication[, c("Lab", "C_M", "SE_C_M")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Model_Estimate__C_M"] <- Het_C_M$b
      Replication.df["Model_Estimate__C_M_K"] <- Het_C_M$k
      Replication.df["Tau2__C_M"] <- Het_C_M$sigma2
      Replication.df["Tau__C_M"] <- sqrt(Het_C_M$sigma2)
      Replication.df["I2__C_M"] <- I2_fct(rma_mv_obj = Het_C_M, v_est = v_estimate)
      Replication.df["H2__C_M"] <- H2_fct(rma_mv_obj = Het_C_M, v_est = v_estimate)
      Replication.df["QE__C_M"] <- Het_C_M$QE
      Replication.df["QEp__C_M"] <- Het_C_M$QEp

      rm(Het_C_M)
    }

    # 3 Heterogeneity of treatment group sd
    if ( nrow(na.omit(subset_Replication[, c("Lab", "T_SD", "SE_T_SD")])) <= 1) {} else {
      # run the meta-analysis
      Het_T_SD <- metafor::rma.mv(yi = T_SD,
                                  V = SE_T_SD^2,
                                  random = ~ 1 | Lab,
                                  method = method,
                                  sparse = TRUE,
                                  data = na.omit(subset_Replication[, c("Lab", "T_SD", "SE_T_SD")]))
      # v estimate for calculating I2 and H2
      v_estimate <- v_est_fct(rma_mv_obj = Het_T_SD,
                              data = na.omit(subset_Replication[, c("Lab", "T_SD", "SE_T_SD")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Model_Estimate__T_SD"] <- Het_T_SD$b
      Replication.df["Model_Estimate__T_SD_K"] <- Het_T_SD$k
      Replication.df["Tau2__T_SD"] <- Het_T_SD$sigma2
      Replication.df["Tau__T_SD"] <- sqrt(Het_T_SD$sigma2)
      Replication.df["I2__T_SD"] <- I2_fct(rma_mv_obj = Het_T_SD, v_est = v_estimate)
      Replication.df["H2__T_SD"] <- H2_fct(rma_mv_obj = Het_T_SD, v_est = v_estimate)
      Replication.df["QE__T_SD"] <- Het_T_SD$QE
      Replication.df["QEp__T_SD"] <- Het_T_SD$QEp

      rm(Het_T_SD)
    }

    # 4 Heterogeneity of control group sd
    if (nrow(na.omit(subset_Replication[, c("Lab", "C_SD", "SE_C_SD")])) <= 1) {} else {
      # run the meta-analysis
      Het_C_SD <- metafor::rma.mv(yi = C_SD,
                                  V = SE_C_SD^2,
                                  random = ~ 1 | Lab,
                                  method = method,
                                  sparse = TRUE,
                                  data = na.omit(subset_Replication[, c("Lab", "C_SD", "SE_C_SD")]))
      # v estimate for calculating I2 and H2
      v_estimate <- v_est_fct(rma_mv_obj = Het_C_SD,
                              data = na.omit(subset_Replication[, c("Lab", "C_SD", "SE_C_SD")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Model_Estimate__C_SD"] <- Het_C_SD$b
      Replication.df["Model_Estimate__C_SD_K"] <- Het_C_SD$k
      Replication.df["Tau2__C_SD"] <- Het_C_SD$sigma2
      Replication.df["Tau__C_SD"] <- sqrt(Het_C_SD$sigma2)
      Replication.df["I2__C_SD"] <- I2_fct(rma_mv_obj = Het_C_SD, v_est = v_estimate)
      Replication.df["H2__C_SD"] <- H2_fct(rma_mv_obj = Het_C_SD, v_est = v_estimate)
      Replication.df["QE__C_SD"] <- Het_C_SD$QE
      Replication.df["QEp__C_SD"] <- Het_C_SD$QEp

      rm(Het_C_SD)
    }

    # 5 Heterogeneity of mean difference
    if ( nrow(na.omit(subset_Replication[, c("Lab", "MD", "SE_MD")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_MD <- metafor::rma.mv(yi = MD, V = SE_MD^2,
                                random = ~ 1 | Lab,
                                method = method,
                                sparse = TRUE,
                                data = na.omit(subset_Replication[, c("Lab", "MD", "SE_MD")]))
      # v estimate for calculating I2 and H2
      v_estimate <- v_est_fct(rma_mv_obj = Het_MD,
                              data = na.omit(subset_Replication[, c("Lab", "MD", "SE_MD")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Model_Estimate__MD"] <- Het_MD$b
      Replication.df["Model_Estimate__MD_K"] <- Het_MD$k
      Replication.df["Tau2__MD"] <- Het_MD$sigma2
      Replication.df["Tau__MD"] <- sqrt(Het_MD$sigma2)
      Replication.df["I2__MD"] <- I2_fct(rma_mv_obj = Het_MD, v_est = v_estimate)
      Replication.df["H2__MD"] <- H2_fct(rma_mv_obj = Het_MD, v_est = v_estimate)
      Replication.df["QE__MD"] <- Het_MD$QE
      Replication.df["QEp__MD"] <- Het_MD$QEp

      rm(Het_MD)
    }

    # 6 Heterogeneity of pooled SD
    if ( nrow(na.omit(subset_Replication[, c("Lab", "pooled_SD", "SE_pooled_SD")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_pooled_SD <- metafor::rma.mv(yi = pooled_SD,
                                       V = SE_pooled_SD^2,
                                       random = ~ 1 | Lab,
                                       method = method,
                                       sparse = TRUE,
                                       data = na.omit(subset_Replication[, c("Lab", "pooled_SD", "SE_pooled_SD")]))
      # v estimate for calculating I2 and H2
      v_estimate <- v_est_fct(rma_mv_obj = Het_pooled_SD,
                              data = na.omit(subset_Replication[, c("Lab", "pooled_SD", "SE_pooled_SD")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Model_Estimate__pooled_SD"] <- Het_pooled_SD$b
      Replication.df["Model_Estimate__pooled_SD_K"] <- Het_pooled_SD$k
      Replication.df["Tau2__pooled_SD"] <- Het_pooled_SD$sigma2
      Replication.df["Tau__pooled_SD"] <- sqrt(Het_pooled_SD$sigma2)
      Replication.df["I2__pooled_SD"] <- I2_fct(rma_mv_obj = Het_pooled_SD, v_est = v_estimate)
      Replication.df["H2__pooled_SD"] <- H2_fct(rma_mv_obj = Het_pooled_SD, v_est = v_estimate)
      Replication.df["QE__pooled_SD"] <- Het_pooled_SD$QE
      Replication.df["QEp__pooled_SD"] <- Het_pooled_SD$QEp

      rm(Het_pooled_SD)
    }


    # 8 Heterogeneity of effect size g (Borenstein)
    if ( nrow(na.omit(subset_Replication[, c("Lab", "SMD", "SE_SMD")])) <= 1 ) {} else {
      # run the meta-analysis
      Het_SMD <- metafor::rma.mv(yi = SMD,
                                   V = SE_SMD^2,
                                   random = ~ 1 | Lab,
                                   method = method,
                                   sparse = TRUE,
                                   data = na.omit(subset_Replication[, c("Lab", "SMD", "SE_SMD")]))
      # v estimate for calculating I2 and H2
      v_estimate <- v_est_fct(rma_mv_obj = Het_SMD,
                              data = na.omit(subset_Replication[, c("Lab", "SMD", "SE_SMD")]))
      # insert the meta analysical results at the appropriate columns in the df
      Replication.df["Model_Estimate__SMD"] <- Het_SMD$b
      Replication.df["Model_Estimate__SMD_K"] <- Het_SMD$k
      Replication.df["Tau2__SMD"] <- Het_SMD$sigma2
      Replication.df["Tau__SMD"] <- sqrt(Het_SMD$sigma2)
      Replication.df["I2__SMD"] <- I2_fct(rma_mv_obj = Het_SMD, v_est = v_estimate)
      Replication.df["H2__SMD"] <- H2_fct(rma_mv_obj = Het_SMD, v_est = v_estimate)
      Replication.df["QE__SMD"] <- Het_SMD$QE
      Replication.df["QEp__SMD"] <- Het_SMD$QEp

      rm(Het_SMD)
    }

    Project <- unique(subset_Replication$Project)
    Replication <- unique(subset_Replication$Replication)
    Replication.df <- tibble::add_column(Replication.df, Project, .before = "Empirical__K")
    Replication.df <- tibble::add_column(Replication.df, Replication, .before = "Empirical__K")

    return(Replication.df)

  }

  ## prepare data for analyses

  # create list: 1 list object = 1 replication
  data_list_Project_split <- split(data, data$Project)

  # create nested list with the replications as level 2 list objects
  nested_data_list_Replication_split <- lapply(1:length(data_list_Project_split), function(x){split(data_list_Project_split[[x]], data_list_Project_split[[x]]$Replication)})
  names(nested_data_list_Replication_split) <- names(data_list_Project_split)

  nested_list_output <- lapply(1:length(nested_data_list_Replication_split),function(x){purrr::map(nested_data_list_Replication_split[[x]], single_replication_analyses)})

  names(nested_list_output) <- names(data_list_Project_split)

  meta_analyses <- dplyr::bind_rows(lapply(1:length(nested_list_output), function(x){dplyr::bind_rows(nested_list_output[[x]])}))

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
  description_vector <- names(meta_analyses)

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

  codebook_for_meta_analyses <- data.frame(Variable_Name = names(meta_analyses), Variable_Description = description_vector)

  ## Outputs

  if (missing(output_folder)) {

    print("You chose not to export the data as .csv files.")

  } else {

    # export .csv files
    write.csv(meta_analyses,
              glue::glue("{output_folder}meta analyses.csv"),
              row.names = FALSE)
    write.csv(codebook_for_meta_analyses,
              glue::glue("{output_folder}codebook for meta analyses.csv"),
              row.names = FALSE)

  }

  if (suppress_list_output == TRUE) {

    print("You chose not to return results in R. If you specified an output folder, check that folder for the code book and merged lab summaries.")

  } else if (suppress_list_output == FALSE) {

    # create list output
    output <- list(meta_analyses, codebook_for_meta_analyses)

    # rename list elements
    names(output) <- c("meta_analyses", "codebook_for_meta_analyses")

    # return the output (function aborts here)
    return(output)

  }


}
