#' Abbreviation to full state name
#'
#' @param abbr Abbreviation
#'
#' @return Full state name
#' @export
#'
#' @examples
#' abbr2name("CA")
abbr2name <- function(abbr) {
  i <- which(state.abb == abbr)
  name <- state.name[i]
  return(name)
}

#' State name to Abbreviation
#'
#' @param name State name
#'
#' @return Abbreviation
#' @export
#'
#' @examples
#' abbr2name("California")
name2abbr <- function(name) {
  i <- which(state.name == name)
  abbr <- state.abb[i]
  return(abbr)
}

#' Find area of a state
#'
#' @param state State name
#'
#' @return The area in sq mi of the state
#' @export
#'
#' @examples
#' state2area("California")
state2area <- function(state) {
  i <- which(state.name == state)
  area <- as.numeric(state.area[i])
  return(area)
}
