configure_google_token <- function() {
  config <- fromJSON(file = "blogger_config.json")
  
  app <- oauth_app("google", key = config$client_id, secret = config$client_secret)
  google_token <- oauth2.0_token(
    oauth_endpoints("google"),
    app,
    scope = "https://www.googleapis.com/auth/blogger"
  )
  
  return(list(config = config, google_token = google_token, time_requested = file.info(".httr-oauth")$mtime))
}