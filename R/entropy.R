#' Computes Shannon Entropy
#'
#' Note: this package is deprecated. Please use \code{languageEntropy::entropy} instead. Given a vector of discrete probabilities, this function will output the Shannon Entropy (a measure of uncertainty)
#'

#' @param x A vector of discrete probabilities
#' @param base Base of the logarithm. Shannon Entropy uses base 2. Other common bases are e (natural) and 10 (hartley).
#' @keywords shannon entropy
#' @export
#' @examples
#' prob_table <- data.frame(event=c("A","B"), prob=c(.25,.75))
#' entropy(prob_table$prob)
entropy <- function(x,base=2){
  .Deprecated("languageEntropy::entropy")
  entr <- 0 # total entropy

  if(sum(x,na.rm=T) == 0){ # if the data is all 0s or NAs, then return NA
    return(NA)
  }else{
    for (i in x){
      if(i > 0 & !is.na(i)){
        entr = entr + (i * log(1 / i, base = base)) # compute entropy and add it to the total
      }
    }
  }
  return(entr)
}
