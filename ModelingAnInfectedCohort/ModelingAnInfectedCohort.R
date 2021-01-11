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
parameters <- c(gamma = recovery_rate)
times <- seq.default(from = 0, to = follow_up_duration, by = 1)

# Model Function:

cohort_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)), {
    # Differential Equations
    dI <- -gamma*I
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
                            parms = parameters))
#printing output
output

# 1.0.1 Based on the output, how many people have recovered abter 4 weeks?

# the statement below will print the "time" and the "R" output when the timestep
# is equals 28.
#
output[output$time == 28, c("time", "R")]

# 1.0.2 What portion of the population does this correspond to?
# (a) - Method 1:
output[output$time == 28, "R"] * 100 / initial_number_infected

#(b) - Method 2: Divide the number of recovered peopleat the 4th week  timestep 
# by the total population size calculated using the values on the data.frame ()
# NOTE: we are assuming in this simulation that there is no births or deadths in our model,
# the total population size is the same at each timestep, so we could also

output[output$time == 28, "R"] / (output[output$time == 28, "I"] + output[output$time == 28, "R"])


# Plotting the output:

# For consistency, convert the dataset into a long format
# (so that the number in each compartment at each timestep are all in the same column)

output_long <- melt(as.data.frame(output), id = "time")

ggplot(data = output_long,
       aes(x=time, y=value, colour = variable, group = variable)) + 
geom_line() +
xlab("Time (days)") +
  ylab("Number of people") + 
  labs(title = paste("Number infected and recovered over time when gama =", 
                     parameters["gamma"], "days^-1"))
