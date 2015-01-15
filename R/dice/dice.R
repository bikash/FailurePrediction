### Simulation & Modeling of Hidden Markov Model (HMM)
library(TeachingDemos)
library(HMM)
library(ggplot2)
set.seed(1)

### Define our variables
TPM <- matrix(c(.95, .05, 
                .1, .9), 2, byrow = TRUE)
EPM <- matrix(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
                1/10, 1/10, 1/10, 1/10, 1/10, 1/2), 2, byrow = TRUE)
simulations <- 500

### Create a dataframe to hold our results
dice <- rep(NA, simulations)
number <- rep.int(0, simulations)
results <- data.frame(dice, number)

### Simulate
# Assume we start with a fair dice
state <- "FAIR"
for (i in 1:simulations) {
  if (state == "FAIR") {
    # Check to see if we're staying with a FAIR dice
    p <- runif(1)
    if (p <= TPM[1,2]) {
      # If not, roll loaded dice
      roll <- dice(rolls = 1, ndice = 1, sides = 6, load = EPM[2,])[[1]]
      # Remember new state
      state <- "LOADED"
    }
    else {
      # Roll fair dice
      roll <- dice(rolls = 1, ndice = 1, sides = 6, load = EPM[1,])[[1]]
      # Remember old state
      state <- "FAIR"
    }
  }
  if (state == "LOADED") {
    # Check to see if we're staying with a LOADED dice
    p <- runif(1)
    if (p < TPM[2,1]) {
      # If not, roll fair dice
      roll <- dice(rolls = 1, ndice = 1, sides = 6, load = EPM[1,])[[1]]
      # Remember new state
      state <- "FAIR"
    }
    else {
      # Roll loaded dice
      roll <- dice(rolls = 1, ndice = 1, sides = 6, load = EPM[2,])[[1]]
      # Remember old state
      state <- "LOADED"
    }
  }
  # Save dice roll and state
  results[i, 1] <- state
  results[i, 2] <- roll
}

### Modeling
# Create hmm using our TPM/EPM
hmm <- initHMM(c("FAIR", "LOADED"), c(1, 2, 3, 4, 5, 6),
               transProbs = TPM, emissionProbs = EPM)
# Pull in results from the simulation
obs <- results[, 2]
# Save Viterbi/Posterior predictions as a new column
results$viterbi <- viterbi(hmm, obs)
results$posterior <- posterior(hmm, obs)[1, ]
results$posterior[results$posterior >= 0.5] <- "FAIR"
results$posterior[results$posterior < 0.5] <- "LOADED"
# Check out results
table(results$dice)
table(results$viterbi)
table(results$posterior)

### Plot predictions with true sequence
p1 <- ggplot(aes(x = seq_along(dice)), data = results) +
  geom_point(aes(y = dice)) + 
  ylab("State") + xlab("Dice Roll (In Sequence)") + ylab("State") +
  ggtitle("Actual Results")

p2 <- ggplot(aes(x = seq_along(dice)), data = results) +
  geom_point(aes(y = dice), color = "#F8766D") + 
  geom_point(aes(y = viterbi), color = "#00BFC4") +
  xlab("Dice Roll (In Sequence)") + ylab("State") +
  ggtitle("Viterbi Predictions")

p3 <- ggplot(aes(x = seq_along(dice)), data = results) +
  geom_point(aes(y = dice), color = "#F8766D") + 
  geom_point(aes(y = posterior), color = "#00BFC4") +
  xlab("Dice Roll (in sequence)") + ylab("State") +
  ggtitle("Posterior Predictions")

grid.arrange(p1, p2, p3, ncol = 1)

### Posterior probabilities
# Calculate Posterior probabilities for dice, save as new column
results$posterior_probabilities <- posterior(hmm, obs)[1, ]
rects <- data.frame(ymin = c(0, 0.5), ymax = c(0.5, 1), Prediction = c("Tails", "Heads"))
# Plot posterior probabilities
ggplot() + 
  geom_rect(data = rects, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = Prediction), alpha = 0.2) +
  geom_line(data = results, aes(x = seq_along(dice), y = posterior_probabilities), color = "grey", size = 0.7) +
  geom_point(data = results, aes(x = seq_along(dice), y = posterior_probabilities), stat = "identity") +
  ggtitle("Posterior Probabilities") +
  xlab("Dice Roll (In Sequence)") + 
  ylab("Probability of Heads")