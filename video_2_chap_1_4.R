# Video 2. Chap1-4

# Chap 2 exercises
# 2M1: Compute grid and plot approximations (uniform)
twoMone <- function(n_W , nTrials) {
  # define grid
  p_grid <- seq( from=0 , to=1 , length.out=20 )
  
  # define prior (here uniform)
  prior <- rep( 1 , 20 )
  
  # compute likelihood at each value in grid
  likelihood <- dbinom( n_W , size=nTrials , prob=p_grid )
  
  # compute product of likelihood and prior
  unstd.posterior <- likelihood * prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstd.posterior / sum(unstd.posterior)
  
  plot( p_grid , posterior , type="b" ,
        xlab="probability of water" , ylab="posterior probability" )
  mtext( "20 points" )
}
twoMone(n_W = 3, nTrials = 3)
twoMone(n_W = 3, nTrials = 4)
twoMone(n_W = 5, nTrials = 7)
# 2M2: Compute grid and plot approximations (0 when p < 0.5, positive 
# constant when p >= 0.5)
twoMtwo <- function(n_W , nTrials) {
  # define grid
  p_grid <- seq( from=0 , to=1 , length.out=20 )
  
  # define prior (0 when p <0.5, positive constant when p>= 0.5)
  prior = ifelse(p_grid < 0.5,0,1)
  
  # compute likelihood at each value in grid
  likelihood <- dbinom( n_W , size=nTrials , prob=p_grid )
  
  # compute product of likelihood and prior
  unstd.posterior <- likelihood * prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstd.posterior / sum(unstd.posterior)
  
  plot( p_grid , posterior , type="b" ,
        xlab="probability of water" , ylab="posterior probability" )
  mtext( "20 points" )
}
twoMtwo(n_W = 3, nTrials = 3)
twoMtwo(n_W = 3, nTrials = 4)
twoMtwo(n_W = 5, nTrials = 7)
# 2M3: Two globes, Earth 70% covered in water, Mars 100% land.
# Both globes equally likely. Get Land. What is P(Earth | Land)
p_land_given_earth = 0.3 #likelihood
p_earth = 0.5 #prior
p_land = 0.5*0.3+0.5*1 # marginal likelihood
p_earth_given_land = p_land_given_earth * p_earth/p_land #posterior

#2M4: 3 cards: (1st) 2 black sides, (2nd) B/W, (3rd) 2 white.
# pick a card a see black. What's P(other side black) ?
# using counting method
paramater = c(0,0.5,1)
ways = c(0,1,2)
n_left = 3
unstd.posterior <- sum(ways)/n_left

#2H1: 2 species of bears, equally likely
# species_1: 10% twin
# species_2: 20% twin
# Observe twin, what's P(next birth will be twin)
prior = c(1,1)
likelihood = c(0.1,0.2)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
p_next_twin = sum(posterior * likelihood)

#2H2: Posterior that after observing a twin, the bear is species is 1
p_species_one = posterior[1]

#2H3: Second birth but it's single. Posterior that it's species 1
unstd.posterior <- (1-likelihood) * posterior
birth_posterior <- unstd.posterior / sum(unstd.posterior)
p_species_one = birth_posterior[1]

#2H4: Vet has a test to identify the species that has the true positive rate:
# TP_1: .8
# TP_2: .65
# Vet does the test and says it came back as species 1. First ignore births, 
# and then include them (twin, and then single)

tp_1 = .8
fp_1 = 1-tp_1
tp_2 = .65
fp_2 = 1-tp_2
prior = c(1,1)
likelihood = c(tp_1, fp_2) # either a true positive species1, or false positive sp_2
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

# instead of 50/50 prior, we use the priors after obvserving the 2 births
unstd.posterior <- likelihood * birth_posterior
posterior <- unstd.posterior / sum(unstd.posterior)

