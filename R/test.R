df <- head(db_data, 100)

x <- function(fields) {
  result <- df %>% mutate_at(.vars = fields, .funs = dmy_hms)
  return(result)
}

dt <- x(c('dia', 'FLARG', 'FVIR', 'HorafL', 'HorafV'))