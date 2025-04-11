edit_blogger_page <- function(gt_table, blogger_info, page_title) {
  google_token <- blogger_info$google_token
  if(difftime(Sys.time(), blogger_info$time_requested, units = "mins") > 59) {
    google_token$refresh()
  }
  access_token <- google_token$credentials$access_token
  
  url <- paste0("https://www.googleapis.com/blogger/v3/blogs/", 
                blogger_info$config$blog_id, 
                "/pages/", 
                blogger_info$config$pages[[page_title]]$id)
  
  body <- list(title = page_title,
               content = as_raw_html(gt_table))

  response <- PUT(url,
                  add_headers(Authorization = paste("Bearer", access_token),
                              "Content-Type" = "application/json"),
                  body = toJSON(body),
                  encode = "json")
  
  if(response$status_code == 200) {
    flog.info(paste("Updated blogger for page", page_title))
  } else {
    flog.error(paste0("Blogger not updated for page ", page_title, ". Status code: ", response$status_code))
  }
  return(gt_table)
}