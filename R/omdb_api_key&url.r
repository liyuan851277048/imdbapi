OMDB_URL="https://www.omdbapi.com/";

omdb_api_key <- function(force = FALSE) {

  env <- Sys.getenv('OMDB_API_KEY')
  if (!identical(env, "") && !force) return(env)

  if (!interactive()) {
    stop("Please set env var OMDB_API_KEY to your OMDB API key",
      call. = FALSE)
  }

  message("Couldn't find env var OMDB_API_KEY.")
  message("Please enter your API key and press enter:")
  code <- readline(": ")

  if (identical(code, "")) {
    stop("OMDB API key entry failed", call. = FALSE)
  }

  message("Updating OMDB_API_KEY env var:")
  Sys.setenv(OMDB_API_KEY = code)

  code

}