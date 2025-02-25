
# Simulates a set of darts, where player 1 throws first
# Returns +1 if player 1 wins, -1 if player 2 wins
simulate_darts_set <- function(p1_avg, p2_avg, best_of_legs = 5){
  p1_legs <- 0  # Count of legs won by player 1
  p2_legs <- 0  # Count of legs won by player 2
  
  leg_tracker <- 1  # Tracks whose turn it is to throw first
  while(p1_legs < ceiling(best_of_legs/2) & p2_legs < ceiling(best_of_legs/2)){
    
    # Simulate number of darts needed for each player to finish 501 points
    n_darts_p1 <- rpois(1, (501/p1_avg * 3) - 9) + 9
    n_darts_p2 <- rpois(1, (501/p2_avg * 3) - 9) + 9
    n_visits_p1 <- ceiling(n_darts_p1 / 3)  # Convert darts to visits (each visit = 3 darts)
    n_visits_p2 <- ceiling(n_darts_p2 / 3)  # Convert darts to visits (each visit = 3 darts)
    
    # If it's an odd leg, player 1 throws first
    if(leg_tracker %% 2 == 1){
      p1_legs <- p1_legs + (n_visits_p1 <= n_visits_p2)  # Player 1 wins if they finish in fewer or equal visits
      p2_legs <- p2_legs + (n_visits_p1 > n_visits_p2)   # Player 2 wins if they finish in fewer visits
      
    } else { # If it's an even leg, player 2 throws first
      p1_legs <- p1_legs + (n_visits_p1 < n_visits_p2)   # Player 1 wins if they finish in fewer visits
      p2_legs <- p2_legs + (n_visits_p1 >= n_visits_p2)  # Player 2 wins if they finish in equal or fewer visits
    }
    
    leg_tracker <- leg_tracker + 1  # Move to the next leg
  }
  
  # Determine the winner of the set
  result = 0
  if (p1_legs == ceiling(best_of_legs / 2)){
    result = 1  # Player 1 wins
  } else if (p2_legs == ceiling(best_of_legs / 2)){
    result = -1  # Player 2 wins
  }
  
  return(result)
}


# Simulates a full match of darts, where players compete in sets
# Returns a vector [p1_sets, p2_sets] representing the number of sets won by each player
simulate_darts_match <- function(p1_avg, p2_avg, best_of_sets = 7){
  p1_sets <- 0  # Count of sets won by player 1
  p2_sets <- 0  # Count of sets won by player 2
  
  set_tracker <- 1  # Tracks whose turn it is to throw first
  
  while(p1_sets < ceiling(best_of_sets / 2) & p2_sets < ceiling(best_of_sets / 2)){
    if(set_tracker %% 2 == 1){
      set_result <- simulate_darts_set(p1_avg, p2_avg)  # Player 1 throws first
    } else { 
      set_result <- -simulate_darts_set(p2_avg, p1_avg)  # Player 2 throws first
    }
    
    # Update set scores
    if (set_result == 1){
      p1_sets <- p1_sets + 1
    } else if (set_result == -1){
      p2_sets <- p2_sets + 1
    }
    
    set_tracker <- set_tracker + 1  # Move to the next set
  }
  
  return(c(p1_sets, p2_sets))  # Return final set score
}


# Simulates multiple darts matches and records the results
# n: number of matches to simulate
# Returns a matrix where each row represents a match result [p1_sets, p2_sets]
multi_game_sim <- function(n, p1_avg, p2_avg, best_of_sets = 7){
  games <- matrix(nrow = n, ncol = 2)  # Create an empty matrix to store results
  colnames(games) <- c("p1", "p2")  # Name columns
  
  for (i in 1:n){
    games[i,] <- simulate_darts_match(p1_avg, p2_avg, best_of_sets = 7)  # Simulate each match
  }
  
  return(games)  # Return all match results
}



# Simulate 1000 matches where Wright throws first
# Using averages from Ochepedia for the last 12 months
sims <- multi_game_sim(1000, 92.53, 98.28, 7)

# Calculate probability of Wright winning 4 sets in a match
mean(sims[,1] == 4)

# Calculate probability of Wright winning 4-1 or 4-0
mean(sims[,1] == 4 & (sims[,2] == 1 | sims[,2] == 0))

