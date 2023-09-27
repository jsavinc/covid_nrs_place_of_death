## example of multistate modelling approach

library(tidyverse)
library(mstate)
# library(msm)  # for intermittently observed data, ours is precise!


# Create fake data --------------------------------------------------------

set.seed = 42
# n <- 65e3
n <- 1e3

individuals <- tibble(
  id = 1:n,
  sex = sample(c("M","F"),prob = c(0.75,0.25), size = n, replace = TRUE),
  age = as.integer(round(rnorm(n = n, mean = 75, sd = 16)))
)

summarised_admissions <-
  individuals %>%
  mutate(
    n_admissions = rpois(n=n, lambda = 2),
    # TODO: this will do for now, I guess?
    total_los = n_admissions * rpois(n=n, lambda = 8),
    total_los = if_else(total_los > 365, 365L, total_los)
  )

admissions <-
  # summarised_admissions %>% filter(n_admissions > 0) %>%
  # pmap_dfr(
  #   .l = .,
  #   .f = function(id, sex, age, n_admissions, total_los) {
  #     tibble(id = id, los = as.vector(rmultinom(n = 1, size = total_los, prob = rep(1/n_admissions, n_admissions))))
  #   }
  # ) %>%
  # TODO: work out how to randomly generate non-overlapping admissions
  # TODO: e.g. random non-overlapping integer sequences
  # maybe pick random integers, 0-365, 2x as many as admissions, sort in increasing order, then interpolate the resulting pairs
  # but then I lose the work of randomly defining the length of hospitalisations? I guess that's fine, I'm not after a particular distriburtion
  summarised_admissions %>% filter(n_admissions > 0) %>%
  pmap_dfr(
    .l = .,
    .f = function(id, sex, age, n_admissions, total_los) {
      # tibble(id = id, los = as.vector(rmultinom(n = 1, size = total_los, prob = rep(1/n_admissions, n_admissions))))
      tibble(
        id = id,
        id_admission = rep(1:n_admissions, each = 2),
        date = sort(sample(x=0:365, size = 2*n_admissions, replace=FALSE)),
        type = rep(c("start","end"), times = n_admissions)
        ) %>%
        pivot_wider(names_from = type, values_from = date, id_cols = c(id, id_admission))
      
    }
  )

# transition_matrix <- trans.illdeath(names = c("Home","Hospital","Death"))
transition_matrix <- transMat(
  list(
    "Home" = c(2,3),
    "Hospital" = c(1,3),
    "Death" = c()
  )
)
# paths(transition_matrix)  # will be error!

# msprep()

data(ebmt3)

covs <- c("dissub", "age", "drmatch", "tcd", "prtime")
msbmt <- msprep(
  time = c(NA, "prtime", "rfstime"),
  status = c(NA,"prstat", "rfsstat"),
  data = ebmt3,
  trans = tmat,
  keep = covs
)


# Example from training ---------------------------------------------------

load("C:/Users/40011625/OneDrive - Edinburgh Napier University/training/Multistate Models Course May2022/Assignment - Exact Times/D.Rdata")

D %>% filter(ID %in% c(1:2))
D[1:17,]  # instructions wrong - these are first 6 subjects probably


tmat <- attr(D, "trans")
events(D)

sf1 <- survfit(Surv(Tstarta, Tstopa, status) ~ 1, data = D, subset = trans == 1)
sf2 <- survfit(Surv(Tstarta, Tstopa, status) ~ 1, data = D, subset = trans == 2)
sf3 <- survfit(Surv(Tstarta, Tstopa, status) ~ 1, data = D, subset = trans == 3)

library(RColorBrewer)
cols <- brewer.pal(12, "Set3")
plot(sf2, fun = "cumhaz", conf.int = FALSE, lwd = 2, col = cols[2],
     xlab = "Age (years)", ylab = "Cumulative hazard")
lines(sf1, fun = "cumhaz", conf.int = FALSE, lwd = 2, col = cols[1])
lines(sf3, fun = "cumhaz", conf.int = FALSE, lwd = 2, col = cols[3])
legend("topleft", c("H -> A", "H -> C", "H -> M"), lwd = 2, col = cols[1:3],
       bty = "n")

c0 <- coxph(Surv(Tstarta, Tstopa, status) ~ strata(trans), data = D)
msf0 <- msfit(c0, trans = tmat)
plot(msf0, lwd = 2, col = cols)

summary(c0)
summary(msf0)
