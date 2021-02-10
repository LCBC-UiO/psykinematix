
#' Read in psykinematix data
#'
#' Psykometrix data come in a large excel sheet
#' where there are several tables in the sheet.
#' This function reads in the data and formats
#' the tables into distinct subtables to be used.
#'
#' @param path path to psykinematix file
read_psykinematix <- function(path){
  sheets <- readxl::excel_sheets(path)

  suppressMessages(
    dt <- lapply(sheets,
                 function(x) readxl::read_excel(
                   path,
                   sheet = x,
                   col_names = FALSE)
    )
  )

  dt <- lapply(dt, cleanup_sheet)
  names(dt) <- janitor::make_clean_names(sheets)
  dt
}


#' Extract specific psykinematix table
#'
#' @param data list contining psykinematix tables
#' @param table table name to extract
extract_table <- function(data, table = "trials") {
  dt <- lapply(1:length(data),
               function(x){
                 if(table %in% names(data[[x]])){
                   data.frame(table = names(data)[x],
                              data[[x]][[table]]
                   )
                 }
               }
  )
  do.call(rbind, dt)
}

write_tables <- function(df, path){
  if(!dir.exists(path)) dir.create(path, recursive = TRUE)

  for(i in unique(df$wave_code)){
    # reduser data i tmp til kun en wave
    tmp <- filter(df, wave_code == i)

    # lagre et filnavn
    filename <- sprintf("mic_s2c_%s.tsv", i)

    # skriv filen
    write.table(tmp, file = file.path(path, filename), sep="\t", row.names = FALSE, quote = FALSE)
  }
}


cleanup_sheet <- function(x){

  if(ncol(x)<2) return(NULL)

  # There are NA rows between each "table"
  index <- as.logical(is.na(x[,1]))

  # create a nested structure with each subtable
  nested_data <- tibble(
    rows = 1:nrow(x),
    index = index,
    x
  ) %>%
    mutate(
      rows = ifelse(index, rows, NA),
      rows = lag(rows),
      rows = ifelse(!is.na(rows), `...1` , NA)
    ) %>%
    fill(rows) %>%
    select(-index) %>%
    nest_by(rows) %>%
    mutate(data = list(cleanup_tables(data)))

  # Combine all tabels with "trial_index" into one large table
  trial_idx <- which(sapply(nested_data$data, function(x) "trial_index" %in% names(x)))
  trial_data <- nested_data$data[[trial_idx[1]]]
  for(i in trial_idx[-1]){
    common_names <- names(trial_data) %in% names(nested_data$data[[i]])
    common_names <- names(trial_data)[common_names]
    trial_data <- left_join(trial_data,
                            nested_data$data[[i]],
                            by =common_names)
  }
  nested_data <- nested_data[trial_idx*-1, ]

  # omorganiser dataene til en liste
  nested_list <- nested_data$data
  names(nested_list) <- janitor::make_clean_names(unlist(nested_data[,1]))
  time <- gsub("\\(|\\)", "", unlist(strsplit(unlist(nested_list[[1]][1,1]), " ")))

  nested_list[[1]] <- list(
    experiment = unname(unlist(nested_data[1,1])),
    testing_mode = strsplit(names(nested_list[[1]]), "_")[[1]][3],
    date = unname(time[2]),
    time = unname(time[3])
  )
  names(nested_list)[1] <- "information"

  if(length(nested_list) < 8) return(NULL)

  nested_list$trials <- select(trial_data, trial_index, everything())

  empty <- which(sapply(nested_list, purrr::is_empty))
  for(i in rev(empty)) nested_list[[i]] <- NULL

  nested_list
}

#' Cleanup psykometrix sheet data
#'

#' This function cleans up those tables into separate
#' distinct tables to be used.
#' Tables must already be separated into distinct parts.
#'
#' @param x table to cleanup
cleanup_tables <- function(x){
  heads <- as.character(x[2, ])
  x <- x[c(-1,-2), ]
  names(x) <- tolower(heads)
  x <- janitor::clean_names(x)
  x <- na_col_rm(x)
  x <- na_row_rm(x)
  x
}

na_col_rm <- function(x){
  idx <- apply(x, 2, function(y) all(is.na(y)))
  x[, !idx]
}

na_row_rm <- function(x){
  idx <- apply(x, 1, function(y) all(is.na(y)))
  x[!idx,]
}
