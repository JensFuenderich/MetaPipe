% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_lab_summaries.R
\name{create_lab_summaries}
\alias{create_lab_summaries}
\title{Creating Lab Summaries}
\usage{
create_lab_summaries(
  data,
  Project = NULL,
  Replication = NULL,
  Lab = NULL,
  DV = NULL,
  Group = NULL,
  output_folder,
  suppress_list_output = FALSE
)
}
\arguments{
\item{data}{A list of data frames that contain the individual participant data. The function expects the relevant columns to be named consistently across all list objects. Relevant to this function are columns that represent information on the project (e.g., Many Labs 2), the replication (e.g., Ross1), the lab (the source a data point is assigned to), the group (either the treatment or control condition) and the single data point of the dependent variable per person.}

\item{Project}{Character vector with the name of the columns in the list elements of "data" that contain the project name(s). If \emph{is.null(Project) == TRUE}, "Project" is chosen as the default.}

\item{Replication}{Character vector with the name of the columns in the list elements of "data" that contain the replication name(s). If \emph{is.null(Replication) == TRUE}, "Replication" is chosen as the default. Each replication comprises a single effect with direct replications across multiple labs/sources.}

\item{Lab}{Character vector with the name of the columns in the list elements of "data" that contain the lab names. If \emph{is.null(Lab) == TRUE}, "Lab" is chosen as the default. The meta-analyses in MetaPipe::meta_analyses() and MetaPipe::full_pipeline() are run as random effects models in metafor::rma.mv() with “random = ~ 1 | Lab”. Thus, the pipeline assumes a distribution of true statistics (e.g., treatment means, mean differences, standardized mean differences).}

\item{DV}{Character vector with the name of the columns in the list elements of "data" that contain the (aggregated) dependent variable. If \emph{is.null(DV) == TRUE}, "DV" is chosen as the default.}

\item{Group}{Character vector with the name of the columns in the list elements of "data" that contain the (treatment/control) group identification. If \emph{is.null(Group) == TRUE}, "Group" is chosen as the default. These should only contain values of 0 (control group), 1 (treatment group) and NA (unidentified).}

\item{output_folder}{Specify the output folder for the summaries and the codebook. If no folder is specified, the function will return its output only to the R environment (unless this is suppressed under suppress_list_output).}

\item{suppress_list_output}{Logical. FALSE by default. If FALSE, the function will return a list output to the environment, containing the lab summaries and the codebook. If TRUE, these are not returned to the environment.}
}
\value{
The function create_lab_summaries returns a list consisting of two elements: A codebook and a list of data frames. Each data frame contains all lab summary statistics for the according Replication/Effect.
The summary statistics returned (including their standard error) are the means and standard deviations for control and experimental groups, pooled standard deviations, raw mean differences and standardized mean differences (Hedge's g according to Borenstein).
\subsection{Example}{

For an example, please refer to the github repository:
https://github.com/JensFuenderich/MetaPipe/blob/main/Supplementary\%20Material/Code\%20Examples/create_lab_summaries().R
}
}
\description{
\loadmathjax{}
\(\let\underscore_\)

Function to create a documentation of lab statistics. Components of the standardized mean difference and their standard errors are calculated and reported. This function is the first step of the MetaPipe pipeline. For more details on the pipeline, refer to the documentation of the MetaPipe-package.
}
\details{
\subsection{Lab Statistics}{

All components of the standardized mean difference and their standard errors are returned by the function. Each standard error is returned to enable a meta-analysis on each component. The components and their standard errors are implemented as follows (unless other sources are given, effect size statistics are calculated according to Borenstein et al):
}

\subsection{mean}{

\itemize{
\item{R-Code} \cr
\code{## apply the function} \cr
\code{# treatment group mean (T_M):} \cr
\code{mean(treatment_group$DV)} \cr
\code{# control group mean (C_M):} \cr
\code{mean(control_group$DV)} \cr
\item{Model} \cr
{
treatment group mean:
\mjdeqn{\bar{x}\underscore{T} = \frac{1}{n}\sum\underscore{i \in T} x}{}
control group mean:
\mjdeqn{\bar{x}\underscore{C} = \frac{1}{n}\sum\underscore{i \in C} x}{}
}
}
}

\subsection{standard error of the mean}{

\itemize{
\item{R-Code} \cr
\code{## define the function} \cr
\code{SE_of_mean_fct <- function(x)\{ } \cr
\code{estimated_sd <- sqrt(sum((x-mean(x))^2)/(length(x)-1))} \cr
\code{SE_of_mean <-  sd(x) / sqrt(length(x))} \cr
\code{return(SE_of_mean)\} } \cr \cr
\code{## apply the function} \cr
\code{# standard error of treatment group mean (SE_T_M):} \cr
\code{SE_of_mean_fct(treatment_group$DV)} \cr
\code{# standard error of control group mean (SE_C_M):} \cr
\code{SE_of_mean_fct(control_group$DV)} \cr
\item{Model} \cr
{
\mjdeqn{ \hat{\sigma}\underscore{\bar{x}} = \frac{\hat{\sigma}\underscore{x}}{\sqrt{n}} = \sqrt{\frac{\frac{1}{n-1}\sum\underscore{i=1}^n(x - \bar{x})^2}{n}} }{}
}
}
}

\subsection{standard deviation}{

\itemize{
\item{R-Code} \cr
\code{## apply the function} \cr
\code{# treatment group standard deviation (T_SD):} \cr
\code{ sd(treatment_group$DV)} \cr
\code{# control group standard deviation (C_SD):} \cr
\code{sd(control_group$DV)}
\item{Model} \cr
{
\mjdeqn{ \hat{\sigma}  = \sqrt{ \frac{ \sum(x-\bar{x}^2) }{n-1}   } }{}
}
}
}

\subsection{standard error of the standard deviation}{

\itemize{
\item{R-Code} \cr
\code{## define the function} \cr
\code{SE_SD_fct <- function(x)\{ } \cr
\code{SE_SD <- sd(x) / sqrt(2*(length(x)-1)) # for large n } \cr
\code{return(SE_SD) \} } \cr \cr
\code{## apply the function} \cr
\code{# standard error of the treatment group standard deviation (SE_T_SD):} \cr
\code{SE_SD_fct(treatment_group$DV)} \cr
\code{# standard error of the control group standard deviation (SE_C_SD):} \cr
\code{SE_SD_fct(control_group$DV)}
\item{Model} \cr
{
\mjdeqn{ \hat{\sigma}\underscore{\hat{\sigma}} = \frac{\hat{\sigma}\underscore{x}}{\sqrt{2(n-1)}} = \sqrt{\frac{\frac{1}{n-1}\sum\underscore{i=1}^n(x - \bar{x})^2}{2(n-1)}} }{}
\mjeqn{ \hat{\sigma}\underscore{\hat{\sigma}} }{} is a simplified version of \mjeqn{ \sigma\underscore{K\underscore{n}S} }{} in Ahn & Fessler (2003). The authors demonstrate that for n > 10 it is reasonable to use Kn = 1. As for the overwhelming majority of samples n > k may be assumed, we excluded the term \mjeqn{K\underscore{n}}{}. For more details, please refer to \cr
\emph{ Ahn, S., & Fessler, J. A. (2003). Standard errors of mean, variance, and standard deviation estimators. EECS Department, The University of Michigan, 1-2.}
}
}
}

\subsection{mean difference (MD)}{

\itemize{
\item{R-Code} \cr
\code{## apply the function} \cr
\code{metafor::escalc( } \cr
\code{measure = "MD", } \cr
\code{m1i = mean(treatment_group$DV),} \cr
\code{m2i = mean(control_group$DV), } \cr
\code{sd1i = sd(treatment_group$DV), } \cr
\code{sd2i = sd(control_group$DV), } \cr
\code{n1i = length(treatment_group$DV), } \cr
\code{n2i = length(control_group$DV), } \cr
\code{vtype = "HO" # assuming homoscedasticity } \cr
\code{)$yi } \cr
\item{Model} \cr
{
\mjdeqn{ D = \bar{x}\underscore{T} -  \bar{x}\underscore{C} }{}
}
}
}

\subsection{standard error of mean difference (SE_MD)}{

\itemize{
\item{R-Code} \cr
\code{## apply the function} \cr
\code{metafor::escalc( } \cr
\code{measure = "MD", } \cr
\code{m1i = mean(treatment_group$DV),} \cr
\code{m2i = mean(control_group$DV), } \cr
\code{sd1i = sd(treatment_group$DV), } \cr
\code{sd2i = sd(control_group$DV), } \cr
\code{n1i = length(treatment_group$DV), } \cr
\code{n2i = length(control_group$DV), } \cr
\code{vtype = "HO" # assuming homoscedasticity } \cr
\code{)$vi } \cr
\item{Model} \cr
{
\mjdeqn{ \hat{\sigma}\underscore{\bar{x}\underscore{T} -  \bar{x}\underscore{C}} = \sqrt{ \frac{n\underscore{T}+ n\underscore{C}}{ n\underscore{T} n\underscore{C} } \sigma^2\underscore{TC} } = \sqrt{ \frac{n\underscore{T}+ n\underscore{C}}{ n\underscore{T} n\underscore{C} } \frac{ \sum\underscore{i = 1}^n (x\underscore{T}-\bar{x}\underscore{T})^2 + \sum\underscore{i = 1}^n (x\underscore{C}-\bar{x}\underscore{C})^2 }{ n\underscore{T} + n\underscore{C} - 2 }   } }{}
}
}
}

\subsection{pooled standard deviation (pooled_SD)}{

\itemize{
\item{R-Code} \cr
\code{## define the function} \cr
\code{pooled_SD_fct <- function(t,c)\{ } \cr
\code{pooled_SD <- sqrt((} \cr
\code{(sum((t-mean(t))^2))+ # sample sum of squares sums treatment group} \cr
\code{(sum((c-mean(c))^2)) # sample sum of squares control group)/} \cr
\code{(length(t) + length(c) -2) # n+n-2} \cr
\code{) # end of sqrt} \cr
\code{return(pooled_SD)\} } \cr
\code{## apply the function} \cr
\code{pooled_SD_fct(treatment_group$DV, control_group$DV)}
\item{Model} \cr
{
\mjdeqn{ \hat{\sigma}\underscore{TC} = \sqrt{ \frac{ \sum\underscore{i = 1}^n (x\underscore{T}-\bar{x}\underscore{T})^2 + \sum\underscore{i = 1}^n (x\underscore{C}-\bar{x}\underscore{C})^2 }{ n\underscore{T} + n\underscore{C} - 2 } } }{}
}
}
}

\subsection{standard error of pooled standard deviation (SE_pooled_SD)}{

\itemize{
\item{R-Code} \cr
\code{## define the function} \cr
\code{SE_pooled_SD_fct <- function(t,c)\{ } \cr
\code{pooled_SD <- sqrt((} \cr
\code{(sum((t-mean(t))^2))+ # sample sum of squares sums treatment group} \cr
\code{(sum((c-mean(c))^2)) # sample sum of squares control group)/} \cr
\code{(length(t) + length(c) -2) # n+n-2} \cr
\code{) # end of sqrt} \cr
\code{SE_pooled_SD <- pooled_SD/sqrt(2*(length(t)+length(c)-1))} \cr
\code{return(SE_pooled_SD)\} } \cr
\code{## apply the function} \cr
\code{SE_pooled_SD_fct(treatment_group$DV, control_group$DV)} \cr

\item{Model} \cr
{
\mjdeqn{ \hat{\sigma}\underscore{\hat{\sigma}\underscore{TC}} = \frac{ \hat{\sigma}\underscore{TC} }{ \sqrt{ 2(n\underscore{T}+n\underscore{C}-1) } } }{}
The standard error is equivalent to that of the standard deviation. For further information, refer to the "standard error of the standard deviation" section.
}
}
}

\subsection{standardized mean difference (SMD)}{

\itemize{
\item{R-Code} \cr
\code{## apply the function} \cr
\code{metafor::escalc(} \cr
\code{measure = "SMD",} \cr
\code{m1i = mean(treatment_group$DV),} \cr
\code{m2i = mean(control_group$DV),} \cr
\code{sd1i = sd(treatment_group$DV),} \cr
\code{sd2i = sd(control_group$DV),} \cr
\code{n1i = length(treatment_group$DV),} \cr
\code{n2i = length(control_group$DV),} \cr
\code{vtype = "LS2" # Borenstein variance} \cr
\code{)$yi} \cr
\code{## apply the function} \cr
\item{Model} \cr
{
\mjdeqn{ g = d \left( 1 - \frac{3}{4(n\underscore{T}+n\underscore{C}-2) -1} \right)  }{}
with
\mjdeqn{ d =  \frac{ \bar{x}\underscore{T} -  \bar{x}\underscore{C} }{ \sqrt{ \frac{ \sum\underscore{i = 1}^n (x\underscore{T}-\bar{x}\underscore{T})^2 + \sum\underscore{i = 1}^n (x\underscore{C}-\bar{x}\underscore{C})^2 }{ n\underscore{T} + n\underscore{C} - 2 } }   }}{}
}
}
}

\subsection{standard error of standardized mean difference (SE_SMD)}{

\itemize{
\item{R-Code} \cr
\code{## apply the function} \cr
\code{sqrt(metafor::escalc(} \cr
\code{measure = "SMD",} \cr
\code{m1i = mean(treatment_group$DV),} \cr
\code{m2i = mean(control_group$DV),} \cr
\code{sd1i = sd(treatment_group$DV),} \cr
\code{sd2i = sd(control_group$DV),} \cr
\code{n1i = length(treatment_group$DV),} \cr
\code{n2i = length(control_group$DV),} \cr
\code{vtype = "LS2" # Borenstein variance} \cr
\code{)$vi)} \cr
\code{## apply the function} \cr
\item{Model} \cr
{
\mjdeqn{ \hat{\sigma}\underscore{g} = \sqrt{ \hat{\sigma}\underscore{d}^2 \left( 1 - \frac{3}{4(n\underscore{T}+n\underscore{C}-2) -1} \right)^2 } }{}
with
\mjdeqn{ \hat{\sigma}\underscore{d}^2 = \frac{n\underscore{T}+n\underscore{C}}{n\underscore{T}n\underscore{C}} + \frac{d^2}{2(n\underscore{T}+n\underscore{C})} }{}
}
}
}
}
