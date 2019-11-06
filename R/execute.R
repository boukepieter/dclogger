execute.cleaning.changes <- function(dir, filenameparent = "parent", filenamechild = "child", uuid_column=NULL) {
  if (!file.exists(sprintf("%s/cleaning_logbook.csv",dir))){
    stop("no cleaning file found")
  }
  log <- read.csv(sprintf("%s/cleaning_logbook.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
  data <- read.csv(sprintf("%s/%s.csv", dir, filenameparent), stringsAsFactors = F, encoding = "UTF-8")
  child <- read.csv(sprintf("%s/%s.csv", dir, filenamechild), stringsAsFactors = F, encoding = "UTF-8")
  match_parent <- grep(pattern = "uuid", x = names(data))
  match_child <- grep(pattern = "uuid", x = names(child))
  if (is.null(uuid_column)){
    if (length(match_parent) < 1 | length(match_child) < 1) {
      stop("cannot find uuid column")
    } else if (length(match_parent) > 1 | length(match_child) > 1) {
      stop("multiple uuid columns found")
    } else {
      uuid_column_parent <- names(data)[match_parent]
      uuid_column_child <- names(child)[match_child]
    }
  }
  parents <- which(log$survey == "parent")
  children <- which(log$survey == "loop")
  for(i in parents){
    if (log$action[i] == "change" & log$uuid[i] %in% data$X_uuid) {
      if (log$question.name[i] %in% names(data)){
        data[which(data[,uuid_column_parent] == log$uuid[i]), trimws(log$question.name[i], which = "right")] <- log$new.value[i]
        log$changed[i] <- TRUE
      } else {
        log$changed[i] <- "ERROR: cannot find question"
      }
    } else if (log$action[i] == "deletion" & log$uuid[i] %in% data$X_uuid) {
      data <- data[-which(data[,uuid_column_parent] == log$uuid[i]), ]
    }
  }
  tobedeleted <- c()
  for(i in children){
    uuid_split <- strsplit(log$uuid[i], split="|", fixed=T)[[1]]
    if (log$action[i] == "change" & uuid_split[1] %in% data$X_uuid) {
      if (log$question.name[i] %in% names(child)){
        child[which(child[,uuid_column_child] == uuid_split[1])[as.numeric(uuid_split[2])], trimws(log$question.name[i], which = "right")] <- log$new.value[i]
        log$changed[i] <- TRUE
      } else {
        log$changed[i] <- "ERROR: cannot find question"
      }
    } else if (log$action[i] == "deletion" & uuid_split[1] %in% data$X_uuid) {
      tobedeleted <- c(tobedeleted, which(child[,uuid_column_child] == uuid_split[1])[as.numeric(uuid_split[2])])
    }
  }
  if (length(tobedeleted > 0)){
    child <- child[-tobedeleted, ]}
  write.csv(log, sprintf("%s/cleaning_logbook.csv", dir), row.names = F, fileEncoding = "UTF-8")
  write.csv(data, sprintf("%s/%s_cleaned.csv", dir, filenameparent), row.names = F, fileEncoding = "UTF-8")
  write.csv(child, sprintf("%s/%s_cleaned.csv", dir, filenamechild), row.names = F, fileEncoding = "UTF-8")
}
