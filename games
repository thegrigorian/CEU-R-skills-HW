set.seed(42)

# Part 1

# creating the simulation of 100 rounds of roulette

simulation <- function() {
  
# roulette function for randomly getting a number 
  roulette <- function() sample(0:36, 1)
  
# assigning the base budget to the wallet
  wallet <- 100
  
# the amount of bet is set to a dollar
  bet <- 1

# run the roulette for 100 rounds
  for (i in 1:100) {
    wallet <- wallet - bet
    number <- roulette()
# there is a win if the number is more than 18
    if (number > 18)  {
    wallet <- wallet + bet * 2
    } 
    
# else the bet is just lost
                  
  }
  
# the budget left after playing 100 rounds with a 100$ budget
  wallet

}

# run the simulation for 1000 times
res <- replicate(1000, simulation())

# print out the structure of the results
str(res)

# Question 1

# draw a histogram on the (1000) resulting budgets at the end of the 100-100 rounds.
hist(res)

# Question 2

# the average amount of lost dollars in 100 rounds
100 - mean(res[which(res < 100)])

# Question 3

# times (out of 1000)  we won at least $1
length(res[which(res > 100)])


# Part 2 

# applying MDS on the "eurodist" database
mds <- cmdscale(as.dist(eurodist))
mds


# regular scatter plot with overlay of text
plot(mds)
text(mds[, 1], mds[, 2], names(eurodist))

# save mds as data frame
mds <- as.data.frame(mds)

# using ggplot2 to map the eurodist
library(ggplot2)
ggplot(mds, aes(V1, V2, label = rownames(mds))) +
  geom_text() + theme_bw()


# using ggrepel to avoid overlapping text labels
library(ggrepel)
ggplot(mds, aes(V1, V2, label = rownames(mds))) +
  geom_text_repel() + theme_bw()



