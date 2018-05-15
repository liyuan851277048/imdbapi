find_by_title <- function(title, type=NULL, season=NULL, episode=NULL,
                          year_of_release=NULL, plot="short", include_tomatoes=FALSE,
                          api_key=omdb_api_key()) {

  if (!is.null(type)) {
    if (!type %in% c("movie", "series", "episode", "game")) {
      message('"type" must be one of "movie", "series", "episode" or "game"')
      return(data.frame())
    }
  }

  para <- list(t=title, type=type, y=year_of_release,
                 plot=plot, r="json", tomatoes=include_tomatoes, apikey=api_key)

  if (!is.null(season) & is.null(episode) |
      !is.null(episode) & is.null(season)) {
    message('Both "season" and "episode" must be specified if one is."')
    return(data.frame())
  } else {
    para["Season"] <- season
    para["Episode"] <- episode
  }

  resp <- httr::GET(OMDB_URL, query=para)
  httr::stop_for_status(resp)
  tmp <- httr::content(resp, as="parsed")

  if (tmp$Response == "False") {
    message(tmp$Error)
    return(data.frame())
  }

  ret <- dplyr::as_data_frame(tmp)
  tmp[ tmp == "N/A" ] <- NA
  class(ret) <- c("omdb", class(ret))

  suppressWarnings(fix_omdb(ret))

}



find_by_id <- function(id, type=NULL, year_of_release=NULL,
                          plot="short", include_tomatoes=FALSE,
                          api_key=omdb_api_key()) {


  para <- list(i=id, type=type, y=year_of_release,
                 plot=plot, r="json", tomatoes=include_tomatoes, apikey=api_key)
  resp <- httr::GET(OMDB_URL, query=para)
  httr::stop_for_status(resp)
  tmp <- httr::content(resp, as="parsed")

  if (tmp$Response == "False") {
    message(tmp$Error)
    return(data.frame())
  }

  ret <- dplyr::as_data_frame(tmp)
  tmp[ tmp == "N/A" ] <- NA
  class(ret) <- c("omdb", class(ret))

  suppressWarnings(fix_omdb(ret))

}

fix_omdb <- function(x) {

  if ("Released" %in% colnames(x))          x$Released          <- as.Date(x$Released, format="%d %b %Y")
  if ("DVD" %in% colnames(x))               x$DVD               <- as.Date(x$DVD, format="%d %b %Y")

  if ("imdbRating" %in% colnames(x))        x$imdbRating        <- as.numeric(x$imdbRating)
  if ("imdbVotes" %in% colnames(x))         x$imdbVotes         <- as.numeric(gsub(",", "", x$imdbVotes))
  if ("tomatoRating" %in% colnames(x))      x$tomatoRating      <- as.numeric(x$tomatoRating)
  if ("tomatoUserRating" %in% colnames(x))  x$tomatoUserRating  <- as.numeric(x$tomatoUserRating)

  if ("tomatoMeter" %in% colnames(x))       x$tomatoMeter       <- as.integer(x$tomatoMeter)
  if ("tomatoReviews" %in% colnames(x))     x$tomatoReviews     <- as.integer(x$tomatoReviews)
  if ("tomatoFresh" %in% colnames(x))       x$tomatoFresh       <- as.integer(x$tomatoFresh)
  if ("tomatoRotten" %in% colnames(x))      x$tomatoRotten      <- as.integer(x$tomatoRotten)
  if ("tomatoUserMeter" %in% colnames(x))   x$tomatoUserMeter   <- as.numeric(x$tomatoUserMeter)
  if ("tomatoUserReviews" %in% colnames(x)) x$tomatoUserReviews <- as.integer(x$tomatoUserReviews)

  x

}

ID_download <- function(ID){
  dplyr::bind_rows(lapply(ID, function(x) {
    find_by_id(x, include_tomatoes = TRUE)
  }))
}
