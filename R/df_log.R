df_log <- function(df, columns) {
  for (i in columns) {
    df[[i]] <- log(df[[i]])
  }
  return(df)
}
