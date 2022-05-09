#' Meta Analyses
#'
#'
#' @import metafor
#' @import purrr
#' @import dplyr
#' @export
#'
## aktuell produziert die Funktion eine genestete Liste und ein df, ich nehme aber nur das df als Output

meta_analyses <- function(data){

  ## input is a large df with all projects & replications


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
                                 method = "REML",
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
                                 method = "REML", sparse = TRUE,
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
                                  method = "REML",
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
                                  method = "REML",
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
                                method = "REML", sparse = TRUE,
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
                                       method = "REML",
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
                                   method = "REML",
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

  df_output <- dplyr::bind_rows(lapply(1:length(nested_list_output), function(x){dplyr::bind_rows(nested_list_output[[x]])}))

  return(df_output)

}
