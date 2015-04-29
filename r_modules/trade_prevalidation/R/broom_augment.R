# Temporary replacement from broom::augment

broom_augment <- function(model) {
  cbind(model.frame(model), .std.resid = rstandard(model))
}