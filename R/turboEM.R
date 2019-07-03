library(turboEM)

N <- dataset %>%
  select(-success) %>%
  left_join(Y %>% select(inst, inst_success = success), by = 'inst')

make_pseudodata2 <- function(strength) {
  data.frame(journal = 1:J,
             ntrials = strength,
             success = strength / 2,
             inst = 0,
             inst0 = 1L, stringsAsFactors = FALSE)
}

expect_max <- function(param = runif(J), data = N, strength) {
  # Pre-allocate
  working_df <- mutate(data, odds = NA, success = NA) %>%
    bind_rows(make_pseudodata2(strength = strength))

  impute <- function(param) {
    param <- setNames(param, sort(as.character(seq_along(param))))

    # Impute values into the working data frame
    working_df %>%
      mutate(odds = exp(param[as.character(journal)])) %>%
      group_by(inst) %>%
      mutate(success = ifelse(inst0, success,
                              BiasedUrn::meanMFNCHypergeo(
                                m = ntrials,
                                n = first(inst_success),
                                odds = odds,
                                precision = 0.1)),
             success_frac = success / ntrials)
  }

  maximization <- function(data) {
    suppressWarnings(
      glm(success_frac ~ -1 + inst0 + journal,
        family = binomial,
        weights = ntrials,
        data = data)
      )
  }

  fixptfn <- function(param) {
    imputed <- impute(param)
    glmodel <- maximize(imputed)
    parnames <- gsub('journal', '', names(coef(glmodel)))[-1]
    coef(glmodel)[-1][as.numeric(parnames)]
  }

  objfn <- function(param) {
    logLik(maximization(impute(param)))
  }

  turboem(param, fixptfn = fixptfn, objfn = objfn, method = c('em', 'squarem'))
}

## Test it out!
system.time(x <- EM(5, get_odds_from_model(model), N))
y <- expect_max(strength = 5)

profvis::profvis({
  y <- expect_max(strength = 5)
})


# Profiling
