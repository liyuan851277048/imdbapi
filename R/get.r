get_actors <- function(omdb) {

  if (!inherits(omdb, "omdb")) {
    message("get_actors() expects an omdb object")
    return(NULL)
  }

  if ("Actors" %in% names(omdb)) {
    str_split(omdb$Actors, ",[ ]*")[[1]]
  }

}

get_directors <- function(omdb) {

  if (!inherits(omdb, "omdb")) {
    message("get_directors() expects an omdb object")
    return(NULL)
  }

  if ("Director" %in% names(omdb)) {
    str_split(omdb$Director, ",[ ]*")[[1]]
  }

}

get_writers <- function(omdb) {

  if (!inherits(omdb, "omdb")) {
    message("get_writers() expects an omdb object")
    return(NULL)
  }

  if ("Writer" %in% names(omdb)) {
    str_split(omdb$Writer, ",[ ]*")[[1]]
  }

}

get_countries <- function(omdb) {

  if (!inherits(omdb, "omdb")) {
    message("get_countries() expects an omdb object")
    return(NULL)
  }

  if ("Country" %in% names(omdb)) {
    str_split(omdb$Country, ",[ ]*")[[1]]
  }

}

get_genres <- function(omdb) {

  if (!inherits(omdb, "omdb")) {
    message("get_genres() expects an omdb object")
    return(NULL)
  }

  if ("Genre" %in% names(omdb)) {
    str_split(omdb$Genre, ",[ ]*")[[1]]
  }

}