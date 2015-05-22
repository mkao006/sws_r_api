kdeModeBoot <- function(x,
                        reps = 100) { # Suggested by Adam
  boot::boot(x, kdeMode, R = reps)$t0
}