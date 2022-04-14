#' Functions to enable genomic prediction using environmental data
#'
#'
#'
#'
#'



# A function to tidy the output of the mmer function
tidy.mmer <- function(object) {

  require(dplyr)
  require(purrr)

  # Get the model estimates
  # Fixed effects
  fixed_eff <- coef(object) %>%
    select(term = Effect, estimate = Estimate) %>%
    mutate(std_error = sqrt(c(object$VarBeta))) %>%
    column_to_rownames("term") %>%
    as.matrix()

  ## Random effects
  rand_eff <- map(randef(object), "value")
  # PEV
  pev <- lapply(X = map(object$PevU, "value"), FUN = diag)
  # combine
  rand_eff <- map2(rand_eff, pev, ~cbind(u = .x, pev = .y))


  ## Vector of new column names for separation, if necessary
  separation_col_names <- str_extract(string = attr(terms(object$call$random), "term.labels"), pattern = "[a-z_]{1,}:[a-z]{1,}") %>%
    str_subset(., ":") %>%
    str_split(., ":") %>%
    unlist()


  ## Convert to a complete data.frame
  rand_eff_df <- rand_eff %>%
    map(~rownames_to_column(as.data.frame(.), "term")) %>%
    map(~rename(., pred = u)) %>%
    imap(~`names<-`(.x, c(str_remove_all(.y, "u:|[0-9]"), paste0("pred", .y), paste0("pev", .y)))) %>%
    modify_if(~str_detect(names(.x)[1], ":"),
              ~separate(.x, col = 1, into = separation_col_names, sep = ":")) %>%
    .[order(map_dbl(., ncol), decreasing = T)] %>%
    # Merge
    reduce(merge) %>%
    mutate(ranef_pred = rowSums(select(., contains("pred"))), ranef_pev = rowSums(select(., contains("pev")))) %>%
    select(-contains(":"))

  # Add fixed effects (grand mean)
  pred_df <- mutate(as_tibble(rand_eff_df), pred = ranef_pred + fixed_eff[,1])

  # Return this data.frame
  return(pred_df)

}







genomewide_prediction <- function(fixed, random, data,   x, model.list, K, E, KE) {

  # Get training and test data
  row <- x
  tr <- unique(row$trait)


  # Edit train and test by adding covariates - centered only
  train <- row$train[[1]]
  test <- row$test[[1]]

  # Extract fixed and random formulas from the list
  model_fixed_forms <- model.list$fixed
  model_rand_forms <- model.list$random


  # Residual formula
  resid_form <- ~ vs(units)




  ## Create relationship matrices
  K <- K # Genomic
  E <- E
  KE <- KE
  # Identity matrix for environments
  I <- diag(ncol(E)); dimnames(I) <- dimnames(E)
  GE <- kronecker(X = K, Y = KE, make.dimnames = TRUE)
  GI <- kronecker(X = K, Y = I, make.dimnames = TRUE)

  ## Define an expression that fits the model
  fit_mmer_exp <- expression({
    model_fit <- mmer(fixed = fixed_form, random = rand_form, rcov = resid_form,
                      data = train, date.warning = FALSE, verbose = TRUE, getPEV = TRUE)
  })



  #################
  ## Fit models and extract predictions
  #################

  prediction_out <- tibble(trait = tr, model = names(model_rand_forms)) %>%
    mutate(prediction = list(NULL))

  # Test df to merge
  test_merge <- test %>%
    select(which(names(.) %in% c("line_name", "env", "loc", "site", "year", "value"))) %>%
    mutate_if(is.factor, as.character)

  # Iterate over models
  for (m in seq(nrow(prediction_out))) {

    # Model name and formulas
    mod <- prediction_out$model[m]
    # fixed_form <- model_fixed_forms[[mod]]
    fixed_form <- model_fixed_forms[[mod]]
    rand_form <- model_rand_forms[[mod]]

    # ## Get the variance components from the full model
    # full_varcomp <- subset(prediction_out, model == mod, sigma, drop = T)[[1]]
    # # Assign values to separate objects
    # varG <- full_varcomp$`u:line_name`
    # varE <- full_varcomp$`u:env`
    # varGE <- full_varcomp$`u:;line_name:env`
    # varR <- full_varcomp$units

    # Use rrBLUP to fit model1 or model2_fr
    if (mod == "model1") {

      mf <- model.frame(value ~ line_name, train)
      y <- model.response(mf)
      Z <- model.matrix(~ -1 + line_name, mf)

      model_fit <- mixed.solve(y = y, Z = Z, K = K, SE = TRUE)

      # Fixed effects
      fixed_eff <- cbind(estimate = model_fit$beta, std_error = model_fit$beta.SE)
      row.names(fixed_eff) <- "(Intercept)"

      ## Random effects
      rand_eff <- list(`u:line_name` = cbind(u = model_fit$u, pev = model_fit$u.SE^2))

      # All other models use SOMMER
    } else {

      ## Try to fit the model; capture the output
      model_stdout <- capture.output( eval(fit_mmer_exp) )


      # # If model fit is empty, try using a smaller number of iterations; for instance find
      # # the maximum logLik and use those iterations
      # itry <- 1
      # while (is_empty(model_fit) & itry == 1) {
      #
      #   # Find the number of iterations that maximized the logLik
      #   best_iter <- model_stdout %>%
      #     subset(., str_detect(., "singular", negate = T)) %>%
      #     read_table(.) %>%
      #     subset(., LogLik == max(LogLik), iteration, drop = TRUE)
      #
      #   # Refit
      #   eval(fit_mmer_exp)
      #
      #   # Increase the counter
      #   itry = itry + 1
      #
      # }

      # If the model is still empty, create empty fixed and random effects
      if (is_empty(model_fit)) {
        fixed_eff <- matrix(as.numeric(NA), nrow = 1, ncol = 2, dimnames = list("(Intercept)", c("estimate", "std_error")))

        rand_eff <- list("u:line_name" = cbind(u = set_names(x = rep(NA, nlevels(test$line_name)), nm = levels(test$line_name)),
                                               pev = NA))

      } else {

        ## Are any main effect variance components equal to zero?
        # Find main effects
        which_main_effects <- str_count(string = names(model_fit$sigma), pattern = ":") == 1 &
          str_detect(string = names(model_fit$sigma), pattern = "units", negate = TRUE)

        ## If the variance components are zero,
        zero_vc <- apply(X = model_fit$monitor[-1,][which_main_effects,] == 0, MARGIN = 2, FUN = any)

        # If there are no zeros, continue
        if (any(zero_vc)) {

          # find the iteration that maximizes the LL with non-zero variance components
          which_iter_max_LL <- max(intersect( order(model_fit$monitor[1,], decreasing = TRUE), which(! zero_vc) ))
          # Was this the max iteration of this model?
          maxed_LL <- which_iter_max_LL == ncol(model_fit$monitor)

          # If not, refit the model
          if (!maxed_LL) {
            # Refit the model using the iterations that maximized the LL
            model_fit <- mmer(fixed = fixed_form, random = rand_form, rcov = resid_form,
                              data = train, date.warning = FALSE, verbose = TRUE, getPEV = TRUE,
                              iters = which_iter_max_LL)
          }

        }



        ## Fixed effects
        fixed_eff <- coef(model_fit) %>%
          select(term = Effect, estimate = Estimate) %>%
          mutate(std_error = sqrt(c(model_fit$VarBeta))) %>%
          column_to_rownames("term") %>%
          as.matrix()

        ## Random effects
        rand_eff <- map(randef(model_fit), "value")
        # PEV
        pev <- map(model_fit$PevU, "value") %>% map(diag)
        # combine
        rand_eff <- map2(rand_eff, pev, ~cbind(u = .x, pev = .y))

      }

    }




    ## Vector of new column names for separation, if necessary
    separation_col_names <- str_extract(string = attr(terms(rand_form), "term.labels"), pattern = "[a-z_]{1,}:[a-z]{1,}") %>%
      str_subset(., ":") %>%
      str_split(., ":") %>%
      unlist()

    ## Create an X matrix for test
    Xtest <- model.matrix(fixed_form, test)[,row.names(fixed_eff), drop = FALSE] # This seems to work
    # Calculate fixed effects by the formula Xb
    fixed_pred <- Xtest %*% fixed_eff[,1]
    fixed_pred_se <-  Xtest %*% fixed_eff[,2] ## This will not work with quantitative elements of X

    ## If model3_fr, calculate random effects
    if (mod == "model3_fr") {

      ## Create an X matrix for test
      Ztest <- test %>%
        mutate(intercept = 1) %>%
        select(environment, intercept, covariate_list$interaction) %>%
        distinct() %>%
        as.data.frame() %>%
        column_to_rownames("environment") %>%
        as.matrix() %>%
        t()

      ## Calculate random effects - main effect and environmental interaction
      rand_eff_hat <- do.call("cbind", rand_eff) %*% Ztest

      # Gather
      rand_eff_df <- rand_eff_hat %>%
        as.data.frame() %>%
        rownames_to_column("line_name") %>%
        gather(env, pred_incomplete, -line_name) %>%
        left_join(test_merge, .)



    } else {

      # ## Convert to a complete data.frame
      # rand_eff_df <- rand_eff %>%
      #   map(~tibble(term = names(.), pred = .x)) %>%
      #   imap(~`names<-`(.x, c(str_remove_all(.y, "u:|[0-9]"), paste0("pred", .y)))) %>%
      #   modify_if(~str_detect(names(.x)[1], ":"),
      #             ~separate(.x, col = 1, into = separation_col_names, sep = ":")) %>%
      #   .[order(map_dbl(., ncol), decreasing = T)] %>%
      #   map(~left_join(test_merge, .x)) %>%
      #   reduce(full_join) %>%
      #   mutate(pred_incomplete = rowSums(select(., contains("pred")))) %>%
      #   select(-contains(":"))

      ## Convert to a complete data.frame
      rand_eff_df <- rand_eff %>%
        map(~rownames_to_column(as.data.frame(.), "term")) %>%
        map(~rename(., pred = u)) %>%
        imap(~`names<-`(.x, c(str_remove_all(.y, "u:|[0-9]"), paste0("pred", .y), paste0("pev", .y)))) %>%
        modify_if(~str_detect(names(.x)[1], ":"),
                  ~separate(.x, col = 1, into = separation_col_names, sep = ":")) %>%
        .[order(map_dbl(., ncol), decreasing = T)] %>%
        map(~merge(x = test_merge, y = .x)) %>%
        reduce(merge) %>%
        mutate(pred_incomplete = rowSums(select(., contains("pred"))), pred_incomplete_pev = rowSums(select(., contains("pev")))) %>%
        select(-contains(":"))

    }




    # Add predictions to the list
    prediction_out$prediction[[m]] <- mutate(rand_eff_df, pred_complete = pred_incomplete + c(fixed_pred),
                                             pred_complete_pev = pred_incomplete_pev + c(fixed_pred_se^2))

  }

  # Return predictions
  return(list(prediction_out = prediction_out))


}
