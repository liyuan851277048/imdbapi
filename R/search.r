search_by_title <- function(term, type=NULL, year_of_release=NULL, page = 1,
                          api_key=omdb_api_key()) {

  para <- list(s=term, type=type, y=year_of_release, page = page, r="json", apikey=api_key)
  resp <- httr::GET(OMDB_URL, query=para)
  httr::stop_for_status(resp)
  tmp <- httr::content(resp, as="parsed")

  if (!("Search" %in% names(tmp))) {
    message(tmp$Error)
    return(data.frame())
  }

  dplyr::bind_rows(lapply(tmp$Search, as.data.frame, stringsAsFactors=FALSE))

}
