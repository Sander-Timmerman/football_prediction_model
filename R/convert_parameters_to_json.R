convert_parameters_to_json <- function(call) {
  func_name <- as.character(call[[1]])
  func <- get(func_name, envir = parent.frame())
  default_args <- formals(func)
  provided_args <- as.list(call)[-1]
  args_list <- lapply(names(default_args), function(arg) {
    if(arg %in% names(provided_args)) {
      return(eval(provided_args[[arg]], parent.frame()))
    } else {
      return(eval(default_args[[arg]], parent.frame()))
    }
  })
  names(args_list) <- names(default_args)
  
  param_json <- toJSON(args_list)
  return(param_json)
}
