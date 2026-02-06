## define function inputs

run_microsim <- function(
    syn, # synth pop
    P,   # trans probs
    state_levels = c("smoke", "vape", "dual", "nonuse"), # only states ppl can be in (and order)
    n_months = 120, # how many cycles
    seed = 1
) {

  
  set.seed(seed)
  
  #save the number of ppl in sim
  num_i <- nrow(syn)
  
  # ---- 1) Smoke/vape trajectory storage  ----
  
  # create a matrix to save each individuals vaping/smoking habits over time
  # each row will be an individual
  # each column will be a month
  m_states <- matrix(NA_character_, nrow = num_i, ncol = n_months + 1)
  # set first column results as starting state
  m_states[, 1] <- as.character(syn$state)
  
  # matrixes that just track smoking or vaping
  m_smoke <- matrix(NA_integer_, nrow = num_i, ncol = n_months + 1)
  m_vape  <- matrix(NA_integer_, nrow = num_i, ncol = n_months + 1)
  m_smoke[, 1] <- syn$smoke_now
  m_vape[, 1]  <- syn$vape_now
  
  # total prevalence during each month
  prevalence <- data.frame(
    month  = 0:n_months,
    smoke  = NA_real_,
    vape   = NA_real_,
    dual   = NA_real_,
    nonuse = NA_real_
  )
  # month 0 = starting states
  prevalence[1, 2:5] <- c(mean(syn$state == "smoke"),
                    mean(syn$state == "vape"),
                    mean(syn$state == "dual"),
                    mean(syn$state == "nonuse"))
  
  
  # ---- 2) Simulation loop over months ----
  for (m in 1:n_months) {
    
    # create blank state for next period
    state_next <- character(num_i)
    
    # for each person
    for (i in 1:num_i) {
      # grab their current state and save
      current_state <- as.character(syn$state[i])
      # given their current state, get the t probs row for that state
      probs <- P[current_state, ]
      # use sample using the t probs to give next state
      # for this to work state_levels and probs need to be in the same order
      # so that the states match onto their respective probabilties
      state_next[i] <- sample(
        x = state_levels, # the possible outcomes
        size = 1, # only one outcome
        prob = probs # probablities of each outcome
      )
    }
    
    # update state with new state
    syn$state <- factor(state_next, levels = state_levels)
    
    # update smoke/vape flags
    syn$smoke_now <- as.integer(syn$state %in% c("smoke", "dual"))
    syn$vape_now  <- as.integer(syn$state %in% c("vape",  "dual"))
    
    # save states to trajectory matrix
    m_states[, m + 1] <- as.character(syn$state)
    m_smoke[,  m + 1] <- syn$smoke_now
    m_vape[,   m + 1] <- syn$vape_now
    
    # save totals to prevalance matrix
    prev[m + 1, 2:5] <- c(mean(syn$state == "smoke"),
                          mean(syn$state == "vape"),
                          mean(syn$state == "dual"),
                          mean(syn$state == "nonuse"))
  }
  # return results
  list(
    syn_final = syn,
    m_states  = m_states,
    m_smoke   = m_smoke,
    m_vape    = m_vape,
    prev      = prev
  )
}
