#' Fixes survey monkey piping
#'
#' Given a set of data from survey monkey, this function will search for all
#' instances of pipes (e.g., {{ Q2 }}) and replace these pipes with answers from
#' the corresponding questions (i.e., the answer for Q2).
#' @param data A dataframe or datatable containing survey moneky data.
#' @param data_start Column where the actual data start. Should be 1, unless
#'   there are surevey monkey data columns that do not pertain to questions.
#' @param subject_column   Subject id variable.
#' @param brackets  Signifier for survey monkey piping. The old format uses
#'   square brackets, and new format uses curly braces.
#' @keywords survey monkey piping
#' @import dplyr
#' @import stringr
#' @importFrom magrittr "%>%"
sm_replace <- function(data, data_start = 1, subject_column = "Subject", brackets = c("[","{")) {

  #Make sure any factors are characters
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)

  sm_columns <- data_start - 1

  matches <- c()
  ## Look at every cell and extract every unique occurance of {{ QXXX }} where XXX is a number and [ QXXX ]
  if("{" %in% brackets | "}" %in% brackets){
    matches <- c(matches,unique(stringr::str_extract(as.matrix(data), "\\{+[:blank:]*Q\\d+[:blank:]*\\}+")))
  }

  if("[" %in% brackets | "]" %in% brackets){
    matches <- c(matches, unique(stringr::str_extract(as.matrix(data), "\\[+[:blank:]*Q\\d+[:blank:]*\\]+")))
  }

  matches <- matches[!is.na(matches)] #get rid of NAs that crop up
  matcher <- as.data.frame(matches,stringsAsFactors = F) #make a dataframe

  matcher$Qnum <- as.numeric(stringr::str_extract(matcher$matches, "\\d+")) # extract only the number from the matched string
  matcher$column <- matcher$Qnum + sm_columns # look for the extracted number + the number of surveymonkey columns to get the actual column

  for (n in 1:nrow(matcher)){ #for each of the unique matches
    #print(paste("looking for:",matcher[n,"matches"]))

    for (i in 1:nrow(data)){ #for each subject
      #print(paste("  participant:", data[i,subject_column_name]))

      for (j in 1:ncol(data)){ #for each column

        if(!is.na(data[i,j]) & data[i,j] == matcher[n,"matches"]){
          #print(paste("    replacing", data[i,j], "with", as.character(data[i,matcher[n,"column"]]),
          #            "from column", matcher[n,"column"]))

          data[i,j] = as.character(data[i,matcher[n,"column"]]) #replace the cell with the data in the appropriate column
        }
      }
    }
  }
  return(data)
}







