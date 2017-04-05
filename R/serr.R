#' \code{serr} computes the standard error of the mean over an id variable
#'
#' This function will compute the standard error of the mean given a dataset, a
#' dependent variable, an id variable, and a set of grouping variables.
#'
#' @param data A dataframe or datatable
#' @param dv   Variable representing your dependent variable. Use the bare variable name.
#' @param id   Variable representing an id variable (e.g., "subject" or "item"). Use the bare variable name.
#' @param ...  Variables to group by (e.g., conditions). Use bare variable names spearated by commas
#' @keywords standard error of the mean, grouped operations
#' @export
#' @examples
#' serr(data, rt, subject, blockcode2, WordType)

serr<-function(data, dv, id, ...){
  dv=lazyeval::lazy(dv)
  id=lazyeval::lazy(id)
  dots=lazyeval::lazy_dots(...)

  df <- data %>%
    dplyr::group_by_(id, .dots = dots) %>%
    dplyr::summarise_(mdv = lazyeval::interp(~mean(var, na.rm = T), var = dv)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::summarise(mean_dv = mean(mdv),
              sd_dv   = sd(mdv),
              N       = n(),
              SERR    = sd_dv / sqrt(N),
              upper   = mean_dv + SERR,
              lower   = mean_dv - SERR) %>%
    dplyr::rename_(.dots = setNames(list("mean_dv", "sd_dv"),
                             c(paste("mean", dv$expr, sep = "_"), paste("sd", dv$expr, sep = "_"))))

  return(df)
}
