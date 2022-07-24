#' Find difference of minimum wage
#'
#' @param start_year The start year
#' @param end_year The end year
#' @param data Minimum wage
#'
#' @return The difference of minimum wage
#' @export
#'
#' @examples
#' find_dif(2010, 2015)
find_dif <- function(start_year, end_year, data = minwage_state_year) {
  starts <- data[which(data[["year"]] == start_year),]
  ends <- data[which(data[["year"]] == end_year),]
  data <- merge(starts, ends, by = "state")
  df <- data["state"]
  df$dif <- data$State.Minimum.Wage.x - data$State.Minimum.Wage.y
  return(df)
}
