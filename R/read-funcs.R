read_rpkmx <- function(path){
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
  browser()

  dt <- jsonlite::parse_json(c("[", dtl, "]"))
}
