# Smoking–Vaping microsimulation model
# Microsim 1
# Update transition probabilities based on occupied state

update_probs1 <- function(
    occupied_state,
    m_trans_probs) {
  
  v_probs <- rep(NA, nrow(m_trans_probs))
  
  v_probs[occupied_state == "N"] <- m_trans_probs["N",]
  v_probs[occupied_state == "S"] <- m_trans_probs["S",]
  v_probs[occupied_state == "V"] <- m_trans_probs["V",]
  v_probs[occupied_state == "D"] <- m_trans_probs["D",]
  
  ifelse(
    test = abs(sum(v_probs) - 1) < 1e-12,
    yes  = return(v_probs),
    no   = stop("Probabilities do not sum to 1")
  )
}
