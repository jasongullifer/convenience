#' \code{serr} computes the standard error of the mean over an id variable.
#'
#' This function will compute the standard error of the mean given a dataset, a
#' dependent variable, an id variable, and a set of grouping variables. This
#' function uses standard evaluation.
#'
#' @param data      A dataframe or datatable
#' @param dv        Variable representing your dependent variable. Use quoted
#'   variable name.
#' @param id        Variable representing an id variable (e.g., "subject" or
#'   "item"). Use quoted variable name.
#' @param grouping  Concatenated set of variables to group by (e.g.,
#'   conditions).
#' @keywords standard error of the mean, grouped operations
#' @export
#' @examples
#' serr(data, "rt", "subject", "blockcode2", "WordType")

serr_ <- function(data, dv, id, grouping){
  df <- data %>%
    dplyr::group_by_(.dots = c(id, grouping)) %>%
    dplyr::summarise_(mdv = lazyeval::interp(~mean(var,na.rm=T), var=as.name(dv))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = grouping) %>%
    dplyr::summarise(mean_dv = mean(mdv),
              sd_dv   = sd(mdv),
              N      = n(),
              SERR   = sd_dv / sqrt(N),
              upper  = mean_dv + SERR,
              lower  = mean_dv - SERR) %>%
    dplyr::rename_(.dots = setNames(list("mean_dv","sd_dv"),
                             c(paste("mean", dv, sep="_"), paste("sd", dv, sep="_"))))
  return(df)
}
