#' Computes the standard error of the mean over an id variable.
#'
#' This function will compute the standard error of the mean given a raw
#' (trial-by-trial) dataset, a dependent variable of interest, an id variable
#' (e.g., "subjects" or "items"), and a set of grouping variables (e.g.,
#' "condition"). It also outputs several statistics: SD, N, and confidence
#' intervals. \cr\cr Two versions of the function are provided. The standard
#' \code{serr} function will accept \code{dplyr}-like arguments (without quotes)
#' for column names. The other \code{serr_} function works on quoted strings,
#' and all grouping variables must be supplied as a vector of characters to the
#' \code{grouping} argument.
#'
#' @param data A dataframe or datatable
#' @param dv   Variable representing your dependent variable. Use the bare variable name when using \code{serr}.
#' @param id   Variable representing an id variable (e.g., "subject" or "item"). Use the bare variable name when using \code{serr}.
#' @param ...  Variables to group by (e.g., conditions) when using \code{serr}. Use bare variable names separated by commas.
#' @param confint_serr Size (in standard errors) for confidence intervals. Default confidence intervals are 1 SERR. To get 95\% confidence intervals, enter 1.96.
#' @keywords standard error of the mean, grouped operations
#' @import dplyr
#' @import lazyeval
#' @importFrom magrittr "%>%"
#' @importFrom stats "sd"
#' @importFrom stats "setNames"
#' @export
#' @examples
#' library(languageR)
#' lexdec.rt <- lexdec[lexdec$Correct=="correct",]
#' serr_summary.rt <- serr(data = lexdec.rt, dv = RT, id = Subject, Class, Complex)
#' print(serr_summary.rt)
serr <- function(data, dv, id, ..., confint_serr=1){
  dv=lazyeval::lazy(dv)
  id=lazyeval::lazy(id)
  dots=lazyeval::lazy_dots(...)

  funs<- list(~mean(mdv), ~sd(mdv), ~n(), ~(sd_dv / sqrt(N)), ~(mean_dv + confint_serr*SERR), ~(mean_dv - confint_serr*SERR))
  fun_names <- c("mean_dv", "sd_dv", "N", "SERR", "upper", "lower")

  df <- data %>%
    dplyr::group_by_(id, .dots = dots) %>%
    dplyr::summarise_(mdv = lazyeval::interp(~mean(var, na.rm = T), var = dv)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::summarise_(.dots = setNames(funs,fun_names)) %>%
    dplyr::ungroup() %>%
    dplyr::rename_(.dots = setNames(list("mean_dv", "sd_dv"),
                             c(paste("mean", dv$expr, sep = "_"), paste("sd", dv$expr, sep = "_"))))
  # df <- data %>%
  #   dplyr::group_by_(id, .dots = dots) %>%
  #   dplyr::summarise_(mdv = lazyeval::interp(~mean(var, na.rm = T), var = dv)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by_(.dots = dots) %>%
  #   dplyr::summarise(mean_dv = mean(mdv),
  #                    sd_dv   = sd(mdv),
  #                    N       = n(),
  #                    SERR    = sd_dv / sqrt(N),
  #                    upper   = mean_dv + confint_serr*SERR,
  #                    lower   = mean_dv - confint_serr*SERR) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::rename_(.dots = setNames(list("mean_dv", "sd_dv"),
  #                                   c(paste("mean", dv$expr, sep = "_"), paste("sd", dv$expr, sep = "_"))))


  return(df)
}

#' @export
#' @rdname serr
#' @inheritParams serr
#' @param grouping  Character vector of the set of variables to group by (e.g.,
#'   conditions) when using \code{serr_}.
serr_ <- function(data, dv, id, grouping, confint_serr=1){
  funs<- list(~mean(mdv), ~sd(mdv), ~n(), ~(sd_dv / sqrt(N)), ~(mean_dv + confint_serr*SERR), ~(mean_dv - confint_serr*SERR))
  fun_names <- c("mean_dv", "sd_dv", "N", "SERR", "upper", "lower")

  df <- data %>%
    dplyr::group_by_(.dots = c(id, grouping)) %>%
    dplyr::summarise_(mdv = lazyeval::interp(~mean(var,na.rm=T), var=as.name(dv))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = grouping) %>%
    dplyr::summarise_(.dots = setNames(funs,fun_names)) %>%
    dplyr::ungroup() %>%
    dplyr::rename_(.dots = setNames(list("mean_dv","sd_dv"),
                                    c(paste("mean", dv, sep="_"), paste("sd", dv, sep="_"))))

  # df <- data %>%
  #   dplyr::group_by_(.dots = c(id, grouping)) %>%
  #   dplyr::summarise_(mdv = lazyeval::interp(~mean(var,na.rm=T), var=as.name(dv))) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by_(.dots = grouping) %>%
  #   dplyr::summarise(mean_dv = mean(mdv),
  #                    sd_dv   = sd(mdv),
  #                    N      = n(),
  #                    SERR   = sd_dv / sqrt(N),
  #                    upper  = mean_dv + confint_serr*SERR,
  #                    lower  = mean_dv - confint_serr*SERR) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::rename_(.dots = setNames(list("mean_dv","sd_dv"),
  #                                   c(paste("mean", dv, sep="_"), paste("sd", dv, sep="_"))))
  return(df)
}
