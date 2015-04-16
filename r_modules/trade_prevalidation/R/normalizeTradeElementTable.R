

normalizeTradeElementTable <- function(elTable) {

elTable %>%
  select_(~code, ~description) %>%
  filter(!(stringr::str_detect(code,
                               "^SD"))) %>%       # TODO: Can't do NSE here
  mutate_(unit = ~(stringr::str_replace_all(
    stringr::str_extract(description,             # Units're extracted from desc
                         "\\[.*\\]$"),            # Extract units with "[]"
    "\\[|\\]", "")),                              # Remove "[]"
    direction = ~detectTradeDirection(code, description),
    backflow  = ~stringr::str_detect(description, "^Re-"),
    unitgroup = ~groupUnits(unit)) %>%
  select_(~code, ~direction, ~backflow, ~unit, ~unitgroup, ~description)
}