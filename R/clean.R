#' Log a cleaning change
#'
#' @param data The dataset that is being cleaned. Has to be a data.frame (no tibble).
#' @param uuid A vector with uuid character objects. Can be one but also multiple.
#' If there are multiple the cleaning will iterate over all of them and use the same question.name, issue and new.value for them.
#' @param action This is the action that needs to be done on the defined row. It is either flag (f), change (c) or deletion (d).
#' @param extra_columns Must be a list with the names of the objects being the names of the columns in the cleaning log
#' and the content of the list being the names of the dataset columns associated with it (see example). This is not required.
#' But it should be the same as the initiated logbook.
#' @param question.name The name of the column in which the value is that needs to be flagged or changed.
#' @param new.value The value that the old value needs to be changed to (only for change action).
#' @param issue A description of the issue why it is being cleaned.
#' @param dir The directory name of the location where it needs to be saved.
#' @param filename Name of the logbook. Default is cleaning_logbook.csv. Needs to have a csv extention.
#' @return It returns the logbook as data.frame.
#' @examples
#' dir <- getwd()
#' print(dir)
#' extra_columns <- list(population_group = "population_group", governorate = "governorate_mcna")
#' \dontrun{
#' init.cleaning.log(dir, extra_columns = extra_columns)
#' }
#'
#' data(mcna2019)
#' \dontrun{
#' library(dplyr)
#' idp_first_place <- data %>% filter(idp_first_place == "yes")
#' flag <- difftime(as.POSIXct(idp_first_place$arrival_date_idp, format="%Y-%m-%d"),
#'                  as.POSIXct(idp_first_place$displace_date_idp, format="%Y-%m-%d"), units = "weeks") > 4
#' idp_first_place[which(flag), c("displace_date_idp", "arrival_date_idp")]
#' uuid <- idp_first_place$X_uuid[which(flag)]
#' log <- log.cleaning(mcna2019, uuid, action = "f",  extra_columns = extra_columns,
#'                                     question.name="arrival_date_idp",
#'                                     issue="The difference between displace date and arrival date while it being first place of displacement is more than 4 weeks",
#'                                     dir = dir)
#' }
log.cleaning <- function(data, uuid, action, extra_columns = list(),
                         question.name = NULL, new.value = NULL, issue = NULL,
                         dir, filename = "cleaning_logbook.csv") {
  action <- ifelse(action == "c", "change", ifelse(action == "d", "deletion", ifelse(action == "f", "flag", action)))
  if (class(data) != "data.frame") {
    stop("Dataset should be of class data.frame.")
  }
  if (!action %in% c("change", "deletion", "flag")) {
    stop("Action given is not recognized.")
  }
  if (action == "change" & (is.null(question.name) | is.null(new.value))) {
    stop("For a change all the parameters of old.value, question.name and new.value should be given.")
  }
  if (action == "flag" & is.null(question.name)) {
    stop("For a flag the parameter question.name should be given.")
  }
  log_file <- sprintf("%s/%s", dir, filename)
  if (! file.exists(log_file)){
    stop("Cleaning log not found, check the file.name or initiate it first with dclogger::init.cleaning.log()")
  } else {
    log <- read.csv(log_file, stringsAsFactors = F, encoding = "UTF-8")
  }
  if (! all(names(log)[3:(which(names(log) == "question.name") - 1)] == names(extra_columns))) {
    check <- readline(prompt="Warning!\nextra_column information is different from the existing cleaning log.\n
                      This may lead to unexpected results and lost information.\nDo you wish to continue? (y/n)")
    if(check != "y") stop("Aborted.")
  }

  for (i in 1:length(uuid)){
    log[nrow(log)+1,"uuid"] <- uuid[i]
    row_nr <- which(data$X_uuid == uuid[i])
    log$log_date[nrow(log)] <- format(Sys.Date(), "%d-%m-%Y")
    for (j in 1:length(extra_columns)) {
      log[nrow(log), names(extra_columns)[j]] <- data[row_nr, extra_columns[[j]]]
    }
    log$action[nrow(log)] <- action
    log$issue[nrow(log)] <- issue
    if (action != "deletion") {
      log$question.name[nrow(log)] <- question.name
      log$old.value[nrow(log)] <- data[row_nr, question.name]
    }
    if (action == "change") {
      log$new.value[nrow(log)] <- new.value
    }
  }
  write.csv(log, log_file, row.names = F, fileEncoding = "UTF-8")
  return(log)
}
