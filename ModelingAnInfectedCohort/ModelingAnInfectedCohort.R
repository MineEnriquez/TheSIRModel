# Libraries
library(deSolve)
library(reshape2)
library(ggplot2)

# Inputs
initial_number_infected <- 1000000
initial_number_recovered <- 0
recovery_rate = 1/10
follow_up_duration = 4*7

# Input Vectors
initial_state_values <- c(I = initial_number_infected,
                          R = initial_number_recovered)
parameteres <- c(gamma = recovery_rate)
times <- seq.default(from = 1, to = follow_up_duration, by = 1)

# Model Function:

cohort_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)), {
    # Differential Equations
    dI <- gamma*I*(-1)
    dR <- gamma*I
    return(list(c(dI, dR)))
  })
}

#Solving the model equations:
# DeSolve -> ode:   Solves a system of ordinary differential equations; 
# a wrapper around the implemented ODE solvers
output <- as.data.frame(ode(y = initial_state_values,
                            times = times,
                            func = cohort_model,
                            parms = parameteres))
#printing output
output
