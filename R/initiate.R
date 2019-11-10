#' Initiate an empty cleaning log
#'
#' @param dir The directory name of the location where it needs to be saved.
#' @param filename Name of the logbook. Default is cleaning_logbook.csv. Needs to have a csv extention.
#' @param extra_columns Must be a list with the names of the objects being the names of the columns in the cleaning log
#' and the content of the list being the names of the dataset columns associated with it (see example). This is not required.
#' @return Nothing, it just writes the empty logbook.
#' @examples
#' dir <- getwd()
#' extra_columns <- list(population_group = "population_group", governorate = "governorate_mcna")
#' \dontrun{
#' init_cleaning_log(dir, extra_columns = extra_columns)
#' }
init_cleaning_log <- function(dir, filename = "cleaning_logbook.csv", extra_columns = NULL){
  if (!endsWith(filename, ".csv")) stop("Filename extention has to be .csv")
  log_file <- sprintf("%s/%s", dir, filename)
  if (file.exists(log_file)){
    check <- readline(prompt="Warning!\nThe filename given already exists, if you continue it will be overwritten.\nDo you wish to continue? (y/n)")
    if(check != "y") stop("Aborted.") else {
      warning("Overwriting logbook...")
    }
  }
  log_start <- data.frame(uuid = character(), log_date = character(),
                          stringsAsFactors = F)
  for (i in 1:length(extra_columns)) {
    log_start[,names(extra_columns)[i]] <- character()

  }
  log_end <- data.frame(question.name = character(),
                        issue = character(), feedback = character(),
                        action = character(), changed = logical(), old.value = character(),
                        new.value = character(),
                        stringsAsFactors = F)
  log <- cbind(log_start, log_end)
  write.csv(log, log_file, row.names = F, fileEncoding = "UTF-8")
}

