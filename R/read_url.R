read_url <- function(url, use_rvest = TRUE, stop_if_failed = FALSE, object_to_save = NULL, object_name, run_number) {
  page <- NULL
  attempts <- 0
  while(is.null(page[1])) {
    page <- tryCatch(
      {
        if(use_rvest) {
          page <- read_html(url, encoding = "UTF-8")
        } else page <- readLines(url, encoding = "UTF-8")
        if(attempts > 0) {
          flog.info(paste("Attempt to read", url, "succeeded, resuming"))
        } else flog.debug(paste("Succesfully read url", url))
        page
      },
      error = function(cond) {
        flog.warn(paste("Attempt to read", url, "failed"))
        return(NULL)
      }
    )
    attempts <- attempts + 1
    if(attempts == 3 & is.null(page[1])) {
      if(stop_if_failed) {
        if(!is.null(object_to_save)) {
          saveRDS(object_to_save, file = file.path("cache", run_number, paste0(object_name, ".rds")))
          flog.info(paste0("Saved ", object_name, " to cache"))
        }
        stop(paste("Page with url", url, "could not be read"))
      } else {
        flog.error(paste("Page with url", url, "could not be read. Returning NULL value"))
      }
      break
    } 
  }
  return(page)
}