kdeMode <- function(x, 
                    ind = seq_along(x), # Compatibility with boot::boot()
                    bw = "SJ", # Recomendation from ?density
                               # But Adams suggests: the single bandwidth h is 
                               # replaced by n different bandwidths dependent on x_i, i = 1,..., n
                    ...) {
  d <- density(x[ind], 
               n = 2^10, # http://stackoverflow.com/a/16255678 and ?density
               ...)
  mo <- d$x[d$y == max(d$y)]
  if(length(mo) > 1) warning(paste0("Multiple modes: ", 
                                    paste(mo, collapse = ", "),
                                    ". Minimal mode returned."))
  min(mo)
}