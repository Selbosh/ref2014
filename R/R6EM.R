#' Expectation-maximization algorithm for ecological REF data
#'
#' Based on an R6 class.
#'
#' @importFrom R6 R6Class
#' @import dplyr
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#'
#' @section Methods:
#' \describe{
#'   \item{\code{example_method(parameter_1 = 3)}}{This method uses \code{parameter_1} to...}
#' }
#'
#' @examples
#' # See strength.Rmd to generate the dataset.
#' \dontrun{
#' trials <- select(dataset, inst, journal, ntrials) %>% mutate_if(is.factor, as.character)
#' totals <- dataset %>% group_by(inst) %>% summarise(total = sum(success))
#'
#' set.seed(1)
#' test <- EM$new(trials, totals, 2)
#' test$run(100)
#' test$iterations
#' }
#'
#' @export
EM <- R6::R6Class('EM',
                  public = list(
                    observed = NULL,
                    pseudodata = NULL,
                    strength = NULL,
                    latent = NULL,
                    model = NULL,
                    converged = FALSE,
                    iterations = 0,

                    initialize = function(trials, totals, strength, init = NULL) {
                      if (any(duplicated(totals$inst)))
                        stop('totals should have one entry per institution')

                      if (any(duplicated(trials[, c('inst', 'journal')])))
                        stop('trials should have one entry per inst:journal')

                      self$strength <- strength
                      self$observed <-
                        left_join(totals, trials, by = 'inst') %>%
                        mutate_if(is.factor, as.character)
                      if (is.null(init))
                        init <- data.frame(
                          journal = unique(trials$journal),
                          logit = rnorm(n_distinct(trials$journal)),
                          stringsAsFactors = FALSE
                        )
                      pseudodata <- private$make_pseudodata(strength)
                      self$latent <- bind_rows(self$observed, pseudodata) %>%
                        mutate(inst0 = as.numeric(inst == 0)) %>%
                        left_join(init, by = 'journal')
                    },

                    maximize = function(data = self$latent) {
                      suppressWarnings(
                        glm(success/ntrials ~ 0 + inst0 + journal,
                            weights = ntrials, family = binomial, data = data)
                      )
                    },

                    impute = function(data = self$latent) {
                      data %>%
                        group_by(inst) %>%
                        mutate(odds = exp(logit),
                               success = ifelse(
                                 inst0, success,
                                 BiasedUrn::meanMFNCHypergeo(
                                   m = ntrials,
                                   n = first(total),
                                   odds = odds,
                                   precision = 0.1))) %>%
                        ungroup()
                    },

                    iterate = function(verbose = FALSE) {
                      self$latent <- self$impute()
                      self$model <- self$maximize()
                      if (verbose) message(logLik(self$model))
                      self$latent$logit <- private$get_logits_from_model()
                      self$iterations <- self$iterations + 1
                      invisible(self)
                    },

                    run = function(maxit, tolerance = 1e-9, verbose = TRUE) {
                      loglik_prev <- -Inf
                      while (!self$converged) {
                        if (self$iterations >= maxit) {
                          warning('Failed to converge\n')
                          break
                        }
                        self$iterate(verbose)
                        is_inst0 <- as.logical(self$latent$inst0)
                        loglik <- #logLik(self$model) # exclude prior component of loglikelihood
                          self$loglik(subset(self$latent$success, !is_inst0),
                                      subset(self$latent$ntrials, !is_inst0),
                                      logits = subset(predict(self$model), !is_inst0))
                        self$converged <- loglik - loglik_prev < tolerance
                        loglik_prev <- loglik
                      }
                      invisible(self)
                    },

                    loglik = function(success = self$latent$success,
                                      ntrials = self$latent$ntrials,
                                      logits = predict(self$model)) {
                      if (length(success) != length(ntrials) |
                          length(success) != length(logits))
                        stop('mismatch in length of arguments passed to loglik')
                      probs <- plogis(logits, log.p = FALSE)
                      binomial()$aic(
                             y = success / ntrials,
                             n = 1,
                             mu = probs,
                             wt = ntrials
                           ) / -2
                    },

                    loglik2 = function(success = self$latent$success,
                                       ntrials = self$latent$ntrials,
                                       logits = predict(self$model)) {
                      # Alternative implementation to loglik() method
                      if (length(success) != length(ntrials) |
                          length(success) != length(logits))
                        stop('mismatch in length of arguments passed to loglik')
                      probs <- plogis(logits, log.p = FALSE)
                      sum(dbinom(round(success),
                                 round(ntrials),
                                 prob = probs, log = TRUE))
                    },

                    pred_loglik = function(newdata) {
                      # newdata: a data.frame comprising
                      # - journal
                      # - ntrials
                      # - success
                      # for the unseen dataset
                      newdata2 <- self$latent %>%
                        filter(!inst0) %>%
                        select(journal, logit) %>%
                        distinct(journal, .keep_all = TRUE) %>%
                        right_join(newdata, by = 'journal')

                      self$loglik(success = newdata2$success,
                                  ntrials = newdata2$ntrials,
                                  logits = newdata2$logit)
                    }

                  ),
                  private = list(
                    make_pseudodata = function(strength = self$strength) {
                      data.frame(journal = unique(self$observed$journal),
                                 ntrials = strength,
                                 success = strength / 2,
                                 inst = 0, inst0 = 1L,
                                 stringsAsFactors = FALSE)
                    },
                    get_logits_from_model = function(model = self$model) {
                      terms <- as.data.frame(predict(model, type = 'terms'))
                      terms$journal
                    }
                  )
)
