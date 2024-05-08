## example of multistate modelling approach

library(tidyverse)
library(mstate)
library(ivs)  # for working with intervals
library(survival) 


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

## now need to convert admissions to alternate transitions between home and
## hospital! to do this, I find complementary intervals to the hospital
## admissions - these are the home stays
home_stays <-
  admissions %>%
  group_by(id) %>%
  reframe(  # reframe allows to create multiple rows per group
    ## using the iv_start/ed and iv_set_difference to find complementary
    ## intervals:
    start_home = iv_start(iv_set_difference(iv(0L, 365L), iv(start, end))),
    end_home = iv_end(iv_set_difference(iv(0L, 365L), iv(start, end)))
  )

## a non-ivs implementation of finding complements
## based on https://stackoverflow.com/a/77273509/2296603
## note that this takes a somewhat different approach, and the resulting intervals are different!

get_all_complementary_integers <- function(start, end, intervals) {
  setdiff(seq.int(start,end,by=1), unlist(intervals))
}

get_contiguous_intervals_from_integers <- function(integers) {
  split(integers, cumsum(c(1L, diff(integers) != 1L)))
}

home_stays2 <-
  admissions %>%
  group_by(id) %>%
  mutate(
    interval = 
      map2(start, end, function(start,end) seq.int(from=start,to=end,by=1))
    ) %>%
  reframe(
    complement = get_contiguous_intervals_from_integers(setdiff(0L:365L, unlist(interval))),
    start = map_int(complement, ~head(.x, 1)),
    end = map_int(complement, ~tail(.x, 1)),
  )
## these come out different - {ivs} uses right-open intervals; here we're using both-sides-closed intervals!

## here I've named the states to make them less confusing: current_state is the
## state the person was in between start and end; new_state is the transition at
## end; I also convert the state to Death at end time of 365 to reflect the
## person dying at the end of the period
transitions <-
  bind_rows(
    admissions %>% select(id, start, end) %>%
      mutate(current_state = "Hospital", new_state = "Home"),
    home_stays %>% select(id, start= start_home, end = end_home) %>%
      mutate(current_state = "Home", new_state = "Hospital")
  ) %>%
  arrange(id, start
        ) %>%
  mutate(
    new_state = if_else(
      condition = end == 365L,
      true = "Death",
      false = new_state
    )
  )

## transitions can have added covariates; for some transitions the covariates
## will change mid-transition (e.g. a policy coming into effect on a certain
## date). Here I'll incorporate that by taking a subset of transitions at a
## certain time, but in the actual study this would be based on an actual date
## so the processing would need to happen before, probably

## define random policy date for each individual; setting the same date separates the data perfectly into a before and after
individuals$policy_date <- sample(x = 1L:365L, size = nrow(individuals), replace = TRUE)
transitions <-
  transitions %>%
  left_join(individuals %>% select(id, policy_date), by = "id")

## if a transition happens before policy change, it's unaffected
## if a transition happens on date of policy change, it's fully 
transitions_with_covariates_before_processing <-
  transitions %>%
  mutate(policy_flag = policy_date > start & policy_date < end)  # closed interval at both ends; 

chunk_1_transitions_without_flag <-
  transitions_with_covariates_before_processing %>%
  filter(!policy_flag) %>%
  mutate(
    policy = if_else(
      end < policy_date,
      FALSE,
      TRUE
    )
  )

## each transition that overlaps the policy date is split into exactly two, so
## the below implementation using reframe() can be simplified as a bind_rows():
chunk_2_transitions_with_flag <-
  bind_rows(
    # first split
    transitions_with_covariates_before_processing %>%
    filter(policy_flag) %>%
    transmute(
      id = id,
      start = start,  # first split = start date stays the same
      end = policy_date,  # end date is policy date
      policy = FALSE,  # policy is not yet in effect
      current_state = current_state,  # current state stays the same
      new_state = "No change"  # censored at the end of first split
    ),
    # second split
    transitions_with_covariates_before_processing %>%
      filter(policy_flag) %>%
      transmute(
        id = id,
        start = policy_date,  # start is policy date
        end = end,  # end is actual end
        policy = TRUE,  # policy TRUE in second split
        current_state = current_state,  # current state stays the same
        new_state = new_state  # new state is now the actual new state
      )
  ) %>%
  arrange(id, start)
  ## below is the slower implementation using reframe()
  # transitions_with_covariates_before_processing %>%
  # filter(policy_flag) %>%
  # mutate(id_transition = 1L : nrow(.)) %>%  # create a temporary id for each row
  # group_by(id, id_transition) %>%
  # reframe(
  #   tibble(  # create two new rows based on each transition
  #     id = rep(id, 2),  # same id for both rows
  #     start = c(start, policy_date),  # split the dates - start to policy change
  #     end = c(policy_date, end),  # split the dates - policy change to end
  #     policy = c(FALSE, TRUE),  # policy is FALSE, then changes to TRUE
  #     current_state = rep(current_state, 2),  # current_state doesn't change
  #     new_state = c("No change", new_state)  # new_state is set to "no change"/censored/reference level for the change in policy - state didn't change, just the covariate changes
  #   )
  # ) %>% select(-id_transition)

## merge the processed transitions
transitions_with_covariates <-
  bind_rows(chunk_1_transitions_without_flag, chunk_2_transitions_with_flag) %>%
  select(-policy_flag) %>%
  arrange(id, start) %>%
  mutate(
    across(c(start, end), as.numeric),
    event = factor(new_state, levels = c("No change", "Home", "Hospital", "Death"))
    ) %>%
  left_join(individuals %>% select(id, sex), by = "id")

## name the possible states
states <- levels(transitions_with_covariates$event)[-1]

cmat <- matrix(0L, 3,3,  dimnames = list(states, states))
cmat[1,2:3] <- 1
cmat[2,c(1,3)] <- 1
statefig(layout = c(2,1), connect = cmat)

## Note: the mstate::transMat() function uses different notation, but can be
## converted
transition_matrix <- mstate::transMat(
  list(
    "Home" = c(2,3),
    "Hospital" = c(1,3),
    "Death" = c()
  )
)
statefig(layout = c(2,1), connect = (!is.na(transition_matrix))*1L)  # convert the matrix entries to 1 where not NA

(check <- survcheck(formula = Surv(time = start, time2 = end, event = event, type = "mstate") ~ policy + sex, data = transitions_with_covariates, id = id, istate = current_state))
check$flag  # for data entries that would produce an error fitting survival object


fit1 <- survfit(formula = Surv(time = start, time2 = end, event = event, type = "mstate") ~ policy + sex, data = transitions_with_covariates, id = id, istate = current_state)
plot(fit1, col = c(1,1,2,2,4,4), pch = "abcdef")
summary(fit1)

broom::tidy(fit1) %>%
  ggplot(data = ., aes(x = time, y = estimate, color = strata)) +
  geom_step() +
  facet_wrap(~state)

## cox model
cfit1 <- coxph(Surv(time = start, time2 = end, event = event, type = "mstate") ~ policy + sex, data = transitions_with_covariates, id = id, istate = current_state)
summary(cfit1)

## check proportional hazards assumption
cox.zph(cfit1)
# results in error...
# Error in solve.default(imat, u) : 
#   system is computationally singular: reciprocal condition number = 1.19407e-16


## creating predicted curves from cox model
dummy <- expand.grid(policy = c(TRUE,FALSE), sex = c("F","M"))
predicted1 <- survfit(cfit1, newdata = dummy)  # from here on you can plot both the original fit and the predicted fit and compare!


# example of same-day discharges ------------------------------------------

# There is an issue with the admissions data: some people are discharged on the
# same day as they are admitted. This would be less of an issue if we knew the
# time as well as the date, as we could compute actual durations in that case.
# In the absence of actual times, there are several solutions:

# 3-day study period example
dt_start <- ymd("2020-01-01")
dt_end <- ymd("2020-01-03")

transitions_3 <- 
  bind_rows(
    tibble(
      id = 1L,
      start = c(ymd("2020-01-01"),ymd("2020-01-02"),ymd("2020-01-03")),
      end = c(c(ymd("2020-01-01"),ymd("2020-01-02"),ymd("2020-01-03")))
    ),
    tibble(
      id = 2L,
      start = c(ymd("2020-01-01"),ymd("2020-01-03")),
      end = c(ymd("2020-01-03"),ymd("2020-01-03")),
    )
  ) %>%
  ## using fake start and end times to generate 12-hour intervals
  mutate(
    start = ymd_h(paste0(start, " 06")),
    end = ymd_h(paste0(end, " 18")),
    interval = iv(start, end)
  ) %>%
  group_by(id) %>%
  reframe(
    interval = iv_set_difference(iv(ymd_hm("2020-01-01 06:00"), ymd_hm("2020-01-04 18:00")), iv(start, end)),
    start_home = iv_start(interval),
    end_home = iv_end(interval)
  ) %>%
  ## convert to time since start of study
  mutate(across(c(start_home,end_home), ~(.x - ymd_hm("2020-01-01 06:00"))/ddays(1)))





# working through mstate example ------------------------------------------



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
