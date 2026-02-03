# Smoking–Vaping microsimulation model
# Microsim 1
# Run microsimulation (states only)

run_microSim1 <- function(
    v_starting_states,
    num_i,
    num_cycles,
    v_states_names,
    m_trans_probs,
    starting_seed = 1) {
  
  m_States <- matrix(NA_character_, nrow = num_i, ncol = num_cycles + 1)
  
  for (i in 1:num_i) {
    set.seed(starting_seed + i)
    
    m_States[i, 1] <- v_starting_states[i]
    
    for (t in 1:num_cycles) {
      
      v_trans_probs <- update_probs1(
        occupied_state = m_States[i, t],
        m_trans_probs  = m_trans_probs
      )
      
      m_States[i, t + 1] <- sample(
        x    = v_states_names,
        prob = v_trans_probs,
        size = 1
      )
    }
  }
  
  m_StateCounts <- matrix(
    0L,
    nrow = length(v_states_names),
    ncol = num_cycles + 1,
    dimnames = list(v_states_names, paste0("cycle_", 0:num_cycles))
  )
  
  for (t in 0:num_cycles) {
    tab <- table(factor(m_States[, t + 1], levels = v_states_names))
    m_StateCounts[, t + 1] <- as.integer(tab)
  }
  
  list(
    m_States      = m_States,
    m_StateCounts = m_StateCounts
  )
}
