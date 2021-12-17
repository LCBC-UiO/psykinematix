#' Read psykinematix file
#'
#' Convenience function, will automatically detect
#' file type based on file extension and use
#' the correct reading function
#'
#' @param path string. path to psykinematix file
#' @param type string. override extension detection
#' @param ... other arguments to the read functions
#'
#' @return depending on read function called, data or string vector
#' @export
#'
# #' @examples
read_pkmx <- function(path, type = NULL, ...){
  if(is.null(type))
    type <- tools::file_ext(path)
  type <- match.arg(type, c("rpkmx", "lpkmx", "dpkmx"))
  read_func <- switch(type,
    "rpkmx" = read_rpkmx,
    "lpkmx" = read_lpkmx,
    "dpkmx" = read_dpkmx
  )
  read_func(path, ...)
}

#' Read psykinematix log file
#'
#' Reads in the psykinematix log
#' file (lpkmx) as a string vector.
#'
#' @param path string. path to file.
#' @param ... other arguments to \code{\link[base]{readLines}}
#'
#' @return string vector of the log file contents
#' @export
#'
# #' @examples
read_lpkmx <- function(path, ...){
  readLines(path, ...)
}

read_rpkmx <- function(path, clean_names = TRUE, ...){
  dtl <- readLines(path)
  dtl <- gsub(";", ",", dtl)
  dtl <- gsub("=", ":", dtl)
  dtl <- sapply(dtl, function(x){
    x <- gsub("\"", "", x)
    if(!grepl("Name", x))
      x <- gsub(" ", "", x)
    if(!grepl(":", x) | grepl("\\{|\\}", x))
      return(x)
    k <- strsplit(x, ":")[[1]]
    pnct <- ""
    if(grepl(",$", k[2])){
      pnct <- ","
      k[2] <- gsub(",$", "", k[2])
    }
    k <- sprintf('"%s"', k)
    sprintf("%s%s", paste0(k, collapse = ": "), pnct)
  })
  dtl <- unname(dtl)
  for(i in 2:length(dtl)){
    if(dtl[i-1] == "}" & dtl[i] == "{")
      dtl[i-1] <- "},"
    if(grepl(",$", dtl[i-1]) & grepl("}", dtl[i]))
      dtl[i-1] <- gsub(",$", "", dtl[i-1])
    if(grepl(":\\{$", dtl[i])){
      dtl[i] <- sprintf('"%s":{', strsplit(dtl[i], ":\\{")[[1]])
    }
  }
  dtl <- c("[", dtl, "]")
  dt <- clean_rpkmx(dtl, clean_names = clean_names)
  id <- gsub(paste0(".", tools::file_ext(path)), "", basename(path))
  dplyr::tibble(file = id,
         dt)
}

read_dpkmx <- function(path, ...){
  stop("We have not implemented a data reader for this file type yet.")
}


## Utils ----

clean_rpkmx <- function(dtl, clean_names = TRUE){
  dt <- jsonlite::fromJSON(dtl)
  dt <- dplyr::as_tibble(dt)
  if(clean_names)
    dt <- janitor::clean_names(dt)
  dt_flat <- flatten_rpkmx(dt, clean_names = clean_names)
  utils::type.convert(dt_flat)
}

flatten_rpkmx <- function(data, clean_names = TRUE){
  idx <- which(unlist(lapply(data, is.data.frame)))
  dt <- data
  for(i in rev(idx)){
    tmp <- data[,i][[1]]
    if(inherits(tmp, "character")){
      tmp <- data[,i]
    }
    nm <- names(data)[i]
    if(any(sapply(tmp, is.data.frame))){
      tmp <- flatten_rpkmx(tmp)
    }
    if(clean_names)
      tmp <- janitor::clean_names(tmp)
    names(tmp) <- sprintf("%s_%s", nm, names(tmp))
    dt <- cbind(dt, tmp)
    dt[, i] <- NULL
  }
  dplyr::as_tibble(dt)
}
