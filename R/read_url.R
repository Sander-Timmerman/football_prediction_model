read_url <- function(url, use_rvest = TRUE) {
  pagina <- "error"
  pogingen <- 0
  while(pagina == "error") {
    pagina <- tryCatch(
      {
        if(use_rvest) {
          read_html(url)
        } else readLines(url)
      },
      error = function(cond) {
        flog.warn(paste("Poging om url", url, "te lezen mislukt"))
        return("error")
      }
    )
    pogingen <- pogingen + 1
    if(pogingen == 1) {
      # flog.error(paste("Pagina met url", url, "kon niet worden gelezen"))
      break
    } 
  }

  return(pagina)
}