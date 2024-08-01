read_url <- function(url, use_rvest = TRUE) {
  pagina <- "error"
  pogingen <- 0
  while(pagina[1] == "error") {
    pagina <- tryCatch(
      {
        if(use_rvest) {
          page <- read_html(url)
        } else page <- readLines(url)
        flog.debug(paste("Succesfully read url", url))
        return(page)
      },
      error = function(cond) {
        flog.warn(paste("Poging om url", url, "te lezen mislukt"))
        return("error")
      }
    )
    pogingen <- pogingen + 1
    if(pogingen == 3) {
      flog.error(paste("Pagina met url", url, "kon niet worden gelezen"))
      break
    } 
  }

  return(pagina)
}