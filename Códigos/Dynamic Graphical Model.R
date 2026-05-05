# https://github.com/schw4b/DGM
 install.packages("DGM")

library(DGM)

#Running a DGM example with simulated data
#We load simulation data of a 5-node network with 200 samples (time points) of one subject. Time series should already be mean centered.
data("utestdata")
dim(myts)

#Now, let's do a full search across all possible parent models of the size n2(n-1). Here, with n=5, we have 16 possible models for each node, for example for node 3.
#The columns are the 16 different models. First row indicates model number, rows 2-5 the parents, row 6 the model evidence, a log likelihood, and row 7 the discount factor delta, reflecting the smoothness of the time-varying regression coefficient (theta). 
result=exhaustive.search(myts,3)
result$model.store

#To get the winning model, we simply maximize across model evidence.
#Model number 3 with node 2 as a parent is most likely.
which.max(result$model.store[6,])

#Analysis on the subject-level
#We do a full search on the subject level (exhautive search on each node). The list returned contains all the models (models), the winning models (winner), the adjacency matrix of the network (adj).
s=subject(myts)
names(s)

# The adj structure contains the adjacency matrix of the network (am), the model evidence (lpl), and the discount factors delta (df).
names(s$adj)
s$adj

#Plot network as adjacency matrix
#The full network structure can be plotted as follows:
  
gplotMat(s$adj$am, hasColMap = F, title = "network")








