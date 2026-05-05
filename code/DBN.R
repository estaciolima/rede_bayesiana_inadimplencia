if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")

library(bnlearn)
library(Rgraphviz)

#Slide 173
#The asia data set is a small synthetic data set from Lauritzen and Spiegelhalter that tries to implement a 
#diagnostic model for lung diseases (tuberculosis, lung cancer or bronchitis) after a visit to Asia.
# D: dyspnoea.
# T: tuberculosis.
# L: lung cancer.
# B: bronchitis.
# A: visit to Asia.
# S: smoking.
# X: chest X-ray.
# E: tuberculosis versus lung cancer/bronchitis.

head(asia)
true.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
graphviz.plot(true.dag)
graphviz.plot(cpdag(true.dag))

# bnlearn implements several constraint-based algorithms, each with its own 
#function: pc.stable, gs(), iamb(), mmpc(), si.hiton.pc(), etc.

cpdag = si.hiton.pc(asia, undirected = FALSE)
graphviz.plot(cpdag)
cpdag2 = pc.stable(asia, undirected = FALSE)
graphviz.plot(cpdag2)
cpdag3 = gs(asia, undirected = FALSE)
graphviz.plot(cpdag3)
cpdag4 = mmpc(asia, undirected = FALSE)
graphviz.plot(cpdag4)

#mutual information: an information-theoretic distance measure. It's proportional to the 
#log-likelihood ratio (they differ by a 2n factor) and is related to the deviance of the
#tested models. The asymptotic χ2test (mi and mi-adf), the Monte Carlo permutation test (mc-mi),
#the sequential Monte Carlo permutation test (smc-mi), and the semiparametric test (sp-mi) are 
#implemented. Compared to mi, mi-adf adjusts the degrees of freedom for structural zeroes 
#and automatically favours independence if there are fewer than 5 observations per parameter.
#shrinkage estimator for the mutual information (mi-sh)

cpdag12 = si.hiton.pc(asia, test = "mc-mi", undirected = FALSE)
cpdag13 = si.hiton.pc(asia, test = "mi", undirected = FALSE)
cpdag14 = si.hiton.pc(asia, test = "smc-mi", undirected = FALSE)
graphviz.plot(cpdag12)
graphviz.plot(cpdag13)
graphviz.plot(cpdag14)

par(mfrow = c(1, 2))
graphviz.plot(cpdag(true.dag))
graphviz.plot(cpdag12)

#Another very useful function is ci.test(), which performs a single
#marginal or conditional independence test using the same backends as
#constraint-based algorithms.

  #H0: S é independente de E.
  #Se o p-valor < 0.05, rejeitamos a hipótese nula 
  #e S e E não são independentes.
  ci.test(x = "S", y = "E", data = asia, test = "mc-mi")

  #H0: S é independente de E dado L.
  #Se o p-valor >= 0.05, não rejeitamos a hipótese nula.
  ci.test(x = "S", y = "E", z = "L", data = asia, test = "mc-mi")
  
  ci.test(x = "S", y = "E", z = "L", data = asia, test = "x2")     # assintótico
  ci.test(x = "S", y = "E", z = "L", data = asia, test = "mc-x2")  # permutação (Monte Carlo)

# Usando constraint-based algorithms, pode dar erro na hora de estimar os parâmetros, pois 
# é necessário ter uma DAG (e não PDAG). Neste caso, primeiro todos os links não direcionados
# precisam ser direcionados
  fitted <- bn.fit(cpdag, data = asia, method = "bayes") #erro
  
  bn.dag <- pdag2dag(cpdag, ordering = names(asia)) #solução: Aqui pdag2dag() resolve as arestas não direcionadas seguindo a ordem que você fornece.
  par(mfrow = c(1, 2))
  graphviz.plot(true.dag)
  graphviz.plot(bn.dag)
  
  fitted <- bn.fit(bn.dag, data = asia, method = "bayes")
  fitted
 
# Aprendizado da estrutura baseado em escores
  bn.s <- hc(asia, score = "bds", iss = 10)
  par(mfrow = c(1, 2))
  graphviz.plot(true.dag)
  graphviz.plot(bn.s)
  
  # Comparação com BDeu
  bn_bdeu <- hc(asia, score = "bde", iss = 10)
  par(mfrow = c(1, 3))
  graphviz.plot(true.dag)
  graphviz.plot(bn.s)
  graphviz.plot(bn_bdeu)
  
  #In addition to scores and their tuning parameters 
  #here iss for the imaginary sample size of BDeu, 
  #hc() has arguments restart for the number of random restarts: O argumento restart = 10 faz com que o algoritmo comece de 10 grafos aleatorios diferentes escolhendo o melhor resultado final.
  #perturb for the number of attempts to randomly insert/remove/reverse an arc on every random restart.
  asia.restart = hc(asia, score = "bde", iss = 1, restart = 10, perturb = 5)
  par(mfrow = c(1, 3))
  graphviz.plot(true.dag)
  graphviz.plot(bn_bdeu)
  graphviz.plot(asia.restart)
  
  dag.tabu = tabu(asia, score = "bic")
  
  # Hybrid Structure Learning Algorithms
  # a single step of the Sparse Candidate algorithm
  asia.rsmax2 =
    rsmax2(asia, restrict = "si.hiton.pc", restrict.args = list(alpha = 0.01),
           maximize = "tabu", maximize.args = list(tabu = 10))
    par(mfrow = c(1, 3))
    graphviz.plot(true.dag)
    #graphviz.plot(cpdag13) #cpdag13 = si.hiton.pc(asia, test = "mi", undirected = FALSE)
    graphviz.plot(dag.tabu)
    graphviz.plot(asia.rsmax2)
  
    unlist(compare(cpdag(cpdag13), cpdag(true.dag)))
    unlist(compare(cpdag(dag.tabu), cpdag(true.dag)))
    unlist(compare(cpdag(asia.rsmax2), cpdag(true.dag)))
    
# The following two commands are equivalent:
    asia.t1 = rsmax2(asia, restrict = "mmpc", maximize = "hc")
    asia.t2 = mmhc(asia)
    graphviz.plot(asia.t1)
    graphviz.plot(asia.t2)
    
    par(mfrow = c(1, 1))
    plot(asia.t1)
    arcs(asia.t1)
    
    