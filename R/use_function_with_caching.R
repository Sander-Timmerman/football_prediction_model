use_function_with_caching <- function(cache_path, cache_name, func_to_use, ...) {
  if(!is.null(cache_path)) {
    if(file.exists(cache_path) & substr(cache_path, nchar(cache_path) - 3, nchar(cache_path)) == ".rds") {
      flog.info(paste0("Loaded ", cache_name, " from cache"))
      return(readRDS(cache_path))
    } else {
      flog.warn(paste0("Didn't find ", cache_name, " in cache with path ", cache_path))
    }
  }
  
  result <- func_to_use(...)
  saveRDS(result, file = file.path("cache", paste0(cache_name, "_", Sys.Date(), ".rds")))
  flog.info(paste0("Saved ", cache_name, " to cache"))
  
  return(result)
}