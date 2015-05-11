# Temporary replacement from broom::augment

broom_augment <- function(model) {
  sm <- summary(model)
  cbind(model.frame(model), 
        .hat = hatvalues(model),
        .cooksd = cooks.distance(model),
        .fitted = unname(model$fitted.values),
        .resid  = unname(model$residuals),
        .std.resid = rstandard(model))
}