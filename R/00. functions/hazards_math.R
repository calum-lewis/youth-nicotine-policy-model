apply_or <- function(p_base, ORs) {
  odds_base <- p_base / (1 - p_base)
  odds_new  <- odds_base * prod(ORs, na.rm = TRUE)
  odds_new / (1 + odds_new)
}
