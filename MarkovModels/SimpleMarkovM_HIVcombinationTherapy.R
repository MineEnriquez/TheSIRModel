# In this model we will present a Simple Markov Model to study the 
# cost effectiveness of 2 different drugs to threat HIV
# (lamivudine/zidovudine combination Therapy) - further described in
# the book Decision Modeling for Health Economic Evaluation by Chancellor, 1997

# This is simpler extract of the analysis from the book.


# GOAL:
# The model aims to compare costs and utilities of two treatment strategies,
# monotherapy and combined therapy.

# DEPENDENCIES"
# install.packages("heemod")
#  install.packages("diagram")

# We declare 4 different states:

# A:  CD4 cells>200 and < 500 cells/mm3;
# B:  CD4 < 200 cell/mm3, non-AIDS;
# C:  AIDS;
# D:  Death;

library(heemod)
library(diagram)

mat_mono <- define_transition(
  .721,  .202, .067, .010,
  0,    .581, .407, .012,
  0, 0, .750, .250,
  0, 0, 0, 1)
## No named state --> generating names

## MONOTHERAPY
mat_mono

plot(mat_mono)

# rr:   (r)elative (r)isk of event
# rr: 0.509
# The combined therapy group has its transition probabilities multiplied by rr.
# Since rr <1, the combined therapy group has less chance to transition 
# to worst health states.
# The probabilities to stay in the same state are equal to 
#  1 - SUM(P-trans) 
# Where P-trans are theprobabilities to change to another state (because all
# the transition probabilities from a given state must sum to 1)


# C:    probability complement  
# C =  1 - SUM(P-trans)

rr <- .509

## COMBINED
mat_comb <- define_transition(
  C,  .202*rr, .067*rr, .010*rr,
  0,  C, .407*rr, .012*rr,
  0, 0, C, .250*rr,
  0, 0, 0, 1)

## No named state --> generating names

mat_comb

plot(mat_comb)


# ======================
# STATE VALUES
# ======================

# Costs of drugs: lamivudine, zidovudine
cost_zido <- 2278
cost_lami <- 2086

# Costs: drugs, healthcare - minus 6% discount.

# STATE A
# =========
state_A <- define_state(
  cost_health = discount(2756, .06),
  cost_drugs = discount(dispatch_strategy(
    mono = cost_zido,
    comb = cost_zido + cost_lami
  ), .060),
  cost_total = cost_health + cost_drugs,
  life_year = 1
  )
  
  state_A

  # STATE B
  # =========
  state_B <- define_state(
    cost_health = discount(3052, .06),
    cost_drugs = discount(dispatch_strategy(
      mono = cost_zido,
      comb = cost_zido + cost_lami
    ), .060),
    cost_total = cost_health + cost_drugs,
    life_year = 1
    )
  
  state_B
  
  
  # STATE C
  # =========
  state_C <- define_state(
    cost_health = discount(9007, .06),
    cost_drugs = discount(dispatch_strategy(
      mono = cost_zido,
      comb = cost_zido + cost_lami
    ), .060),
    cost_total = cost_health + cost_drugs,
    life_year = 1
    )
  
  state_C

  
  # STATE D
  # =========
  state_D <- define_state(
    cost_health = 0,
    cost_drugs = 0,
    cost_total = 0,
    life_year = 0
  )
  
  state_D
  
  
  # ==========================
  # STRATEGY DEFINITIONS
  # =========================
  
  strat_mono <- define_strategy(
    transition = mat_mono,
    state_A,
    state_B,
    state_C,
    state_D
  )
  
  ## No named state ->  generating names
  
  strat_mono