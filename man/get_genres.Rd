
\name{get_genres}
\alias{get_genres}
\title{Get genres from an omdb object as a vector}
\usage{
get_genres(omdb)
}
\arguments{
\item{omdb}{an object returned by \code{find_by_title} or \code{find_by_id}}
}
\value{
A character vector of genres
}
\description{
Splits the field at commas and returns a vector of individual entries
}
\examples{\donttest{
get_genres(find_by_id("tt0031381"))
}
}
