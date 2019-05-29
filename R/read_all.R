#' Read all files in a given directory
#'
#' @param dir Directory to read from
#' @param type Specify either "csv" (comma separated file) or "txt" (tab delimited file). Defaults to "csv".
#' @param euro Specify TRUE/FALSE whether the file format is european (e.g., Euro decimals are commas). Defaults to FALSE.
#' @param encoding Specify file encoding. Defaults to "UTF-8". Excel typically generates "latin1" files.
#'
#' @importFrom plyr "rbind.fill"
#' @importFrom utils "read.delim"
#' @export
read_all <- function(dir="./", type="csv", euro=F, encoding="UTF-8"){
  data<-data.frame() # create a blank dataframe

  if(type=="csv" & euro==F){
    sep=","
    dec="."
    filetype="*.csv"
  }else if(type == "csv" & euro==T){
    sep=";"
    dec=","
    filetype="*.csv"
  }else if(type == "txt" & euro==F){
    sep="\t"
    dec="."
    filetype="*.txt"
  }else if(type== "txt" & euro==T){
    sep="\t"
    dec=","
    filetype="*.txt"
  }

  files <- dir(dir,filetype) # get a list of the csv files
  for (file in files){ #for each file
    cur_file <- read.delim(paste0(dir,file), stringsAsFactors = F, encoding=encoding, sep=sep, dec=dec) #read the file
    data <- plyr::rbind.fill(data,cur_file) #combine the current datset with the previous datasets
  }
  print(paste("Read", length(files), "files from",dir))
  return(data)
}




