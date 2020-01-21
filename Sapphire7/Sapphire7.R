library(tidyverse)
library(data.table)
library(parallel)

# GLOBAL VARIABLES
N_SIMS <- 3
N_CORES <- 3 
path <- "./test.csv"

createSapphire7Tickets <- function(){
  nTickets <- 420000
  sapphire7_tickets <- c(rep(7, nTickets/7),
                         rep(15, nTickets/38),
                         rep(25, nTickets/33),
                         rep(70, nTickets/81),
                         rep(700, nTickets/5250),
                         rep(1000, nTickets/11667),
                         rep(7777, nTickets/60000))
  sapphire7_tickets <- c(sapphire7_tickets, rep(0, nTickets - length(sapphire7_tickets)))
  return(sample(sapphire7_tickets))
}



simSapphire7Game <- function(id = 1){
  tickets <- createSapphire7Tickets()
  nTickets <- length(tickets)
  
  loser <- NULL
  seven <- NULL
  fifteen <- NULL
  twenty_five <- NULL
  seventy <- NULL
  seven_hundred <- NULL
  thousand <- NULL
  seven_777 <- NULL
  expVal <- NULL
  
  for(i in 1:nTickets){
    # print(i)
    tickets <- tickets[-1]
    loser[i] <-sum(tickets == 0)
    seven[i] <- sum(tickets == 7)
    fifteen[i] <- sum(tickets == 15)
    twenty_five[i] <- sum(tickets == 25)
    seventy[i] <- sum(tickets == 70)
    seven_hundred[i] <- sum(tickets == 700)
    thousand[i] <- sum(tickets == 1000)
    seven_777[i] <- sum(tickets == 7777)
    expVal[i] <- mean(tickets)
  }
  
  
  
  simData <- data.table(`Losers` = loser,
                        `7` = seven,
                        `15` = fifteen,
                        `25` = twenty_five,
                        `70` = seventy,
                        `700` = seven_hundred,
                        `1000` = thousand,
                        `7777` = seven_777,
                        `ExpVal` = expVal,
                        `ticketNum` = 1:nTickets,
                        id = id)
  return(simData)
}


simNGames_Sapphire7 <- function(nSims, nCores){
  cl <- makeCluster(2)
  clusterExport(cl=cl, varlist=c("createSapphire7Tickets", "simSapphire7Game", "data.table"))
  simResults <- parLapply(cl, 1:nSims, simSapphire7Game)
  return(rbindlist(simResults))
}



system.time({
  df <- simNGames_Sapphire7(N_SIMS, N_CORES)
})
fwrite(df, path)

