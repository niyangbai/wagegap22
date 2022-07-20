#' @title The FIPS Code of U.S.
#'
#' @description A data set with FIPS Code and Geographical information.
#'
#' @format A data frame with 3145 rows and 13 variables.
#'
#' @source <https://github.com/kjhealy/fips-codes>
"fips_code"

#' Turn FIPS Code to Full Name
#'
#' @description Return the name of a county according to the given FIPS code.
#'
#' @param fips FIPS codes are numbers which uniquely identify geographic areas.
#'
#' @return The full name of the county
#' @export
#'
#' @examples
#' fips2name('01001')
fips2name <- function(fips) {
  i <- which(fips_code$fips == as.numeric(fips))
  name <- toString(fips_code$long_name[i])
  return(name)
}

#' Turn FIPS Code to State Name
#'
#' @param fips FIPS codes are numbers which uniquely identify geographic areas.
#'
#' @description Return the state name of a county according to the given FIPS code.
#'
#' @return The state name of the county
#' @export
#'
#' @examples
#' fips2state('01001')
fips2state <- function(fips) {
  i <- which(fips_code$fips == as.numeric(fips))
  name <- toString(fips_code$state_name[i])
  return(name)
}

#' Turn FIPS Code to County Name
#'
#' @param fips FIPS codes are numbers which uniquely identify geographic areas.
#'
#' @description Return the county name of a county according to the given FIPS code.
#'
#' @return The county name of the county
#' @export
#'
#' @examples
#' fips2county('01001')
fips2county <- function(fips) {
  i <- which(fips_code$fips == as.numeric(fips))
  name <- toString(fips_code$county_name[i])
  return(name)
}

#' Turn FIPS Code to Region Name
#'
#' @param fips FIPS codes are numbers which uniquely identify geographic areas.
#'
#' @description Return the region name of a county according to the given FIPS code.
#'
#' @return The region name of the county
#' @export
#'
#' @examples
#' fips2region('01001')
fips2region <- function(fips) {
  i <- which(fips_code$fips == as.numeric(fips))
  region <- toString(fips_code$region_name[i])
  return(region)
}
