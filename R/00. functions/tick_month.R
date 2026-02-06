tick_month <- function(syn, P, state_levels) {
  
  ## Step 1: one random draw per person
  #generate random number from 1-100k for each person
  #use to determine behaviour based on probs
  u <- runif(nrow(syn))
  state_next <- NA_character_
  
  ## Step 2: assign state_next based off probs
  # run for each possible state
  for (s in state_levels) {
    # how many people in each state
    idx <- which(syn$state == s)
    # if no-one in state, move on
    if (!length(idx)) next
    
    #save the row of transition probs for that state
    #cumulate them to 1, will each have their own section in cum
    
    # e,g (placeholder probs)
    #[0.00, 0.03] → smoke
    #[0.03, 0.06] → vape
    #[0.06, 0.07] → dual
    #[0.07, 1.00] → nonuse
    
    cum <- cumsum(P[s, ])
    state_next[idx] <- state_levels[
      #find interval/section that random number idx falls in
      findInterval(u[idx], vec = c(0, cum), rightmost.closed = TRUE) #rightmost forces draws of 1 to fall in last category and not break
    ]
  }
  
  ## make sure state_next in the correct order
  state_next <- factor(state_next, levels = state_levels)
  ## check for missing values
  stopifnot(!anyNA(state_next))
  
  ## final mapping back to binary columns
  syn$smoke_now <- as.integer(state_next %in% c("smoke", "dual")) #set smokenow to 1 if smoke or dual
  syn$vape_now  <- as.integer(state_next %in% c("vape",  "dual")) #set vapenow to 1 if vape or dual
  
  ## commit update
  syn$state <- state_next #end of tick, set state_next as current state
  
  return(syn)
}
