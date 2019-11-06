log.cleaning <- function(data, partners, psu, uuid, action,
                         question.name=NULL, new.value=NULL, issue=NULL,
                         dir) {
  action <- ifelse(action == "c", "change", ifelse(action == "d", "deletion", ifelse(action == "f", "flag", action)))
  if (!action %in% c("change", "deletion", "flag")) {
    stop("action given is not a valid action")
  }
  if (action == "change" & (is.null(question.name) | is.null(new.value))) {
    stop("For a change all the parameters of old.value, question.name and new.value should be given.")
  }
  if (action == "flag" & is.null(question.name)) {
    stop("For a flag the parameter question.name should be given.")
  }
  log_file <- sprintf("%s/cleaning_logbook.csv",dir)
  if (file.exists(sprintf("%s/cleaning_logbook.csv",dir))){
    log <- read.csv(log_file, stringsAsFactors = F, encoding = "UTF-8")
  } else {
    log <- data.frame(survey = character(), uuid = character(), governorate = character(), log_date = character(),
                      location_id = character(), location_name = character(), district = character(),
                      ngo = character(), ngo_name = character(), enumerator = character(),
                      question.name = character(),
                      issue = character(), feedback = character(),
                      action = character(), changed = logical(), old.value = character(),
                      new.value = character(),
                      stringsAsFactors = F)
  }
  for (i in 1:length(uuid)){
    log[nrow(log)+1,"uuid"] <- uuid[i]
    row_nr <- which(data$X_uuid == uuid[i])
    log$survey[nrow(log)] <- ifelse("ngo" %in% names(data), "parent", "loop")
    log$governorate[nrow(log)] <- data$governorate_mcna[row_nr]
    log$log_date[nrow(log)] <- format(Sys.Date(), "%d-%m-%Y")
    log$location_id[nrow(log)] <- data$cluster_location_id[row_nr]
    log$location_name[nrow(log)] <- psu[match(data$cluster_location_id[row_nr], psu[,3]),7]
    log$district[nrow(log)] <- psu[match(data$cluster_location_id[row_nr], psu[,3]),5]
    log$ngo[nrow(log)] <- data$ngo[row_nr]
    log$ngo_name[nrow(log)] <- partners[match(data$ngo[row_nr], partners[,1]),2]
    log$enumerator[nrow(log)] <- data$enumerator_num[row_nr]
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
