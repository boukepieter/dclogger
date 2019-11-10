#' Execute all loggged cleaning changes
#'
#' @param dir The directory name of the location where it needs to be saved.
#' @param datasetname Name of the dataset. Needs to have a csv extention and be in the <dir> directory.
#' @param logbookname Name of the logbook. Default is cleaning_logbook.csv. Needs to have a csv extention and be in the <dir> directory.
#' @param uuid_column Name of the UUID column in the dataset. By default (if argument left empty) it will use grep to look for uuid in
#' the column names but if there are no or multiple matches it will give an error.
#' @return Nothing but the cleaned dataset will be saved as <datasetname>_cleaned.csv and the logbook will be overwritten with some comments
#' in the <changed> column. TRUE if it is changed or an error if the question name could not be found.
#' @examples
#' dir <- getwd()
#' extra_columns <- list(population_group = "population_group", governorate = "governorate_mcna")
#' \dontrun{
#' init_cleaning_log(dir, extra_columns = extra_columns)
#'
#' data(mcna2019)
#' require(dplyr)
#' flag <- mcna2019$num_hh_member - mcna2019$num_family_member < 0
#' mcna2019[which(flag), c("num_hh_member", "num_family_member")]
#' uuid <- mcna2019$X_uuid[which(flag)]
#' log <- log_cleaning(mcna2019, uuid, action = "change",  extra_columns = extra_columns,
#'                     question.name = "num_hh_member",
#'                     issue="Number of family members should always be lower or equal
#'                     to household size, but for these it is not.",
#'                     new.value = mcna2019$num_family_member[which(flag)], dir = dir)
#' write.csv(mcna2019, "data.csv", row.names = F)
#' execute_cleaning_changes(dir, "data.csv", "cleaning_logbook.csv", uuid_column = "X_uuid")
#' }
execute_cleaning_changes <- function(dir, datasetname = "parent.csv", logbookname = "cleaning_logbook.csv", uuid_column=NULL) {
  if (!file.exists(sprintf("%s/%s",dir, logbookname))){
    stop("no cleaning file found")
  }
  log <- read.csv(sprintf("%s/%s",dir, logbookname), stringsAsFactors = F, encoding = "UTF-8")
  data <- read.csv(sprintf("%s/%s", dir, datasetname), stringsAsFactors = F, encoding = "UTF-8")
  match <- grep(pattern = "uuid", x = names(data))
  if (is.null(uuid_column)){
    if (length(match) < 1) {
      stop("cannot find uuid column")
    } else if (length(match) > 1) {
      stop("multiple uuid columns found")
    } else {
      uuid_column <- names(data)[match]
    }
  }
  for(i in 1:nrow(log)){
    if (log$action[i] == "change" & log$uuid[i] %in% data$X_uuid) {
      if (log$question.name[i] %in% names(data)){
        data[which(data[,uuid_column] == log$uuid[i]), trimws(log$question.name[i], which = "right")] <- log$new.value[i]
        log$changed[i] <- TRUE
      } else {
        log$changed[i] <- "ERROR: cannot find question"
      }
    } else if (log$action[i] == "deletion" & log$uuid[i] %in% data$X_uuid) {
      data <- data[-which(data[,uuid_column] == log$uuid[i]), ]
    }
  }
  write.csv(log, sprintf("%s/%s",dir, logbookname), row.names = F, fileEncoding = "UTF-8")
  write.csv(data, sprintf("%s/%s_cleaned.csv", dir, strsplit(datasetname, split = "\\.")[[1]][1]), row.names = F, fileEncoding = "UTF-8")
}
