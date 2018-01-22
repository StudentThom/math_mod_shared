### helper functions for EM algo ###

# libraries
library(docstring)
library(tictoc)

#functions

mixture_density <- function(x, pi_0, pi_1, alpha, beta){
  
  #' Denisty of a mixture distribution
  #'
  #' Gives density of a mixture uniform with one beta(alpha,beta) distribution
  #' 
  #' @param x vector over which density is calculated, usually on interval (0,1)
  #' @param pi vector of length two with weight of uniform and beta distribution

  pi_0 * dunif(x) + pi_1 * dbeta(x,alpha,beta)
}

expected_resp <- function(x, pi_vector, j, alpha, beta){
 
  #' Expected responsibility W_(i,j) for component j
  #'
  #' Gives for a component j at each data point x_i the component\'s relative
  #' probability such that sum_{j=1}^{k} = 1 for all i
  #'
  #' @param x vector of data
  #' @param pi_vector vector with pi_{j}\'s which give for each j^th component the mixture distribution its weight
  #' @param j component number
  #' @param alpha vector with alpha_1,..,alpha_j,..alpha_k 
  #' @param beta vector with beta_1,..,beta_j,..beta_k 

  sum = 0
  w_i_j = 0
  length_vect = length(pi_vector)
  for (k in 1:length_vect) {
    sum <- sum + ((pi_vector[k] * dbeta(x,alpha[k],beta[k])))
  }
  w_i_j = (pi_vector[j] * dbeta(x, alpha[j], beta[j])) / sum
  
  # return
  return(w_i_j)
}

averaged_resp_weights <- function(data, pi_vector, alpha, beta) {

  #' Averaged responsibility weights
  #'
  #' Calculates avergaed responsibility weights of W_{i,j} to make new estimation for \\pi_{j}\'s
  #' Calculates new alpha_{j}\'s and beta_{j}\'s based on MLE estimation
  #'
  #' @param data vector of data
  #' @param pi_vector vector with \\pi_{j}\'s which give for each j^th component the mixture distribution its weight
  #' @param alpha vector with alpha_1,..,alpha_j,..alpha_k 
  #' @param beta vector with beta_1,..,beta_j,..beta_k 

  pi_new_vector = 0
  for (j in 1:length(pi_vector)){
    sum = 0
    for (i in 1:length(data)){
      sum = sum + expected_resp(data[i], pi_vector, j, alpha, beta)
    }
    # average is new value for pi_j^+
    average = sum / length(data)
    pi_new_vector[j] = average
    
    # make MLE estiamtion for alpha and beta if distribution is not the first one (i.e. uniform)
    if (j != 1){
      tic("alpha_beta_mle")
      out <- optim(c(alpha[j],beta[j]),expectation_f,lower=c(0.1,0.1),method="L-BFGS-B",data=allp$p1,pi_vector=pi_vector,j=j, alphas=alpha,betas=beta,control=list(fnscale = -1))
      par <- out$par
      alpha[j] = par[[1]]
      beta[j] = par[[2]]
      toc()
    }
  }
  
  list(pi_new_vector, alpha, beta)
}

expectation_f <- function(alpha_beta,data, pi_vector, j, alphas, betas){

  #' Likelihood of expectation of f
  #'
  #' Function returns likelihood of single beta distibution with parameters alpha_beta where likelihood is based on other beta distributions in mixture distribution with parameters in alphas and betas
  #'
  #' @param alpha_beta vector with alpha_{j} and beta_{j} of f_{j}
  #' @param data vector of data
  #' @param pi_vector vector with \\pi_{j}\'s which give for each j^th component the mixture distribution its weight
  #' @param alpha vector with alpha_1,..,alpha_j,..alpha_k 
  #' @param beta vector with beta_1,..,beta_j,..beta_k 
  #'
  alpha = alpha_beta[1]
  beta = alpha_beta[2]
  sum = 0
  exp_resp <- 0
  for (i in 1:length(data)) {
    exp_resp <- (expected_resp(data[i], pi_vector, j, alphas, betas))
    sum = sum + (log(dbeta(data[i],alpha,beta)) * exp_resp)
  }
  # return
  sum
}

em_algo <- function(data,pi_vector,alpha,beta, number_of_iterations){
  
  #' Execute EM algorith
  #'
  #' Give initial data the EM algorith is executed by for a number_of_iterations running the Expectation and the Maximalisation step
  #'
  #' @param data vector of data
  #' @param pi_vector vector with \\pi_{j}\'s which give for each j^th component the mixture distribution its weight
  #' @param alpha vector with alpha_1,..,alpha_j,..alpha_k 
  #' @param beta vector with beta_1,..,beta_j,..beta_k 
  #' @param number_of_iterations number of times the E and M step have to be executed
  #'
  #' TO DO?!: replace number_of_titerations criteria with convergence criteria
  #'

  for (n in 1:number_of_iterations){
    lijst = averaged_resp_weights(data, pi_vector, alpha, beta)
    pi_vector = lijst[[1]]
    alpha = lijst[[2]]
    beta = lijst[[3]]
  }
  list(pi_vector, alpha, beta)
}

plot_distribution <- function(data, pi_vector, alpha, beta, main){

  #' Plot mixture distribution
  #'
  #' Plot histogram of data together with plot of mixture density in order to visually obtain whether found distribution fits data
  #'
  #' @param data vector of data
  #' @param pi_vector vector with \\pi_{j}\'s which give for each j^th component the mixture distribution its weight
  #' @param alpha vector with alpha_1,..,alpha_j,..alpha_k 
  #' @param beta vector with beta_1,..,beta_j,..beta_k 
 

  hist(data,freq=FALSE,main=main)
  x <- seq(0,1,0.01)
  y <- vector(mode="numeric", length=length(x))
  sum = 0
  for (i in 1:length(x)){
    for (k in 1:length(pi_vector)){
      y[i] <- y[i] +  pi_vector[k] * dbeta(x[i],alpha[k],beta[k])
    }
  }
  lines(x,y)
}

print_results <- function(alpha,beta,pi_vector,start_time,alpha_new,beta_new,pi_vector_new,end_time) {
  for (j in 1:length(alpha)) {
    print(paste("Old alpha",j, ":", alpha[j], "New alpha",j, ":", alpha_new[j]))
    print(paste("Old beta",j, ":", beta[j], "New beta",j, ":", beta_new[j]))
    print(paste("Old pi",j, ":", pi_vector[j], "New pi",j, ":", pi_vector_new[j]))
  }
  print(end_time - start_time)
}

