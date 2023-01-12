a0 = 0
a1 = 2
a2 = 2
c1 = 1
c2 = 1 
mu = 0.25

# MULTINOMIAL-LOGIT DEMAND

mnl1 = function(pp1, pp2) {
  demand1_ces = exp((a1-pp1) / mu) / ( exp((a1-pp1)/mu) + exp((a2-pp2)/mu) + exp((a0)/mu) ) 
  return(demand1_ces)
}
mnl2 = function(pp1, pp2) {
  demand2_ces = exp((a2-pp2) / mu) / ( exp((a1-pp1)/mu) + exp((a2-pp2)/mu) + exp((a0)/mu) )
  return(demand2)
}


#variation to avoid overflow:

mnl1k = function(pp1, pp2) {
  k = max(a1-pp1, a2-pp2, a0)
  demand1_ces = exp((a1-pp1-k) / mu) / ( exp((a1-pp1-k)/mu) + exp((a2-pp2-k)/mu) + exp((a0-k)/mu) )
  return(demand1_ces)
}

mnl2k = function(pp1, pp2) {
  k = max(a1-pp1, a2-pp2, a0)
  demand1_ces = exp((a2-pp2-k) / mu) / ( exp((a1-pp1-k)/mu) + exp((a2-pp2-k)/mu) + exp((a0-k)/mu) )   
  return(demand1_ces)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sigma = 1.2
b1=1
b2=1

# CES DEMAND

ces1 = function(pp1, pp2) {
  demand1_mnl = b1*pp1^(-sigma) / (b1*pp1^(1-sigma) + b2*pp2^(1-sigma)) #if pp0=0    # Le domande CES si chiamano MNL?
  # external good:
  # pp0 =
  # b0  = 
  # demand1_mnl = b1*pp1^(-sigma) / (b1*pp1^(1-sigma) + b2*pp2^(1-sigma) + b0*pp0^(1-sigma))   # Manca una parentesi chiusa a denominatore
  return(demand1_mnl)
}

ces2 = function(pp1, pp2) {
  demand2_mnl = b2*pp2^(-sigma) / (b1*pp1^(1-sigma) + b2*pp2^(1-sigma))
  # external good:
  # pp0 =
  # b0  = 
  # demand2_mnl = b2*pp2^(-sigma) / (b1*pp1^(1-sigma) + b2*pp2^(1-sigma) + b0*pp0^(1-sigma))   # Manca una parentesi chiusa a denominatore, e la domamnda si chiama demand2
  return(demand2_mnl)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


computeProfits = function ( ) {
  
  PI = matrix(data = NA, nrow = numPrices^2, ncol = 2*numAgents)
  h = 0
  for (p1 in 1:numPrices) {
    for (p2 in 1:numPrices) {
      h = h+1
      pp1 = Prices[p1]
      pp2 = Prices[p2]
      
      # CES demand
      
      d1 = ces1(pp1,pp2)
      d2 = ces2(pp1,pp2)
      # d1 = mnl1(pp1,pp2)
      # d2 = mnl2(pp1,pp2)
      
      # Profit
      
      PI[h, 1] = p1
      PI[h, 2] = p2
      PI[h, 3] = (pp1-c1)*d1
      PI[h, 4] = (pp2-c2)*d2
      
    }
  }
  
  return(PI)
}

