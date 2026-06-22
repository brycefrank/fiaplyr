### Implementation Plan: User-Defined Output Naming

The goal is to implement naming of output columns in `aggregate`, `estimate`
and `estimate_ratio` endpoints. For example

handler |>
  aggregate(tree(my_vol = VOLCFNET, my_vol_2 = VOLCFGRS))

**1. Update Scope Helpers (Verification)**
* **File:** `R/scoped-helpers.R`
* **Action:** Verify that `tree()`, `cond()`, etc., using `rlang::enquos(...)` correctly preserve the `names` attribute of the returned list. (No code changes expected here, as `enquos` handles this natively).

**2. Update Aggregation Generators**
* **File:** `R/aggregate.R`
* **Functions:** `.make_tree_aggregates()`, `.make_tree_history_aggregates()`, `.make_cond_aggregates()`
* **Action:** Modify the column naming logic for the `agg_exprs` list before it is passed to `dplyr::summarise()`.
* **Implementation:**
    ```r
    target_vars <- rlang::quos(...)
    user_names <- names(target_vars)
    
    names(agg_exprs) <- purrr::imap_chr(target_vars, function(var_quo, idx) {
      # 1. Use explicit user-provided name if available
      if (!is.null(user_names) && user_names[idx] != "") {
        return(user_names[idx])
      }
      
      # 2. Fallback to auto-generated naming (e.g., macro name or bare symbol)
      expr <- rlang::quo_get_expr(var_quo)
      if (rlang::is_call(expr)) {
        return(paste0(rlang::call_name(expr), "_", rlang::as_name(expr[[2]])))
      } else {
        return(rlang::as_name(expr))
      }
    })
    ```

**3. Update Estimator Target Parsing**
* **Files:** `R/Estimator.R`, `R/PostStratifiedEstimator.R`, `R/PostStratifiedRatioEstimator.R`
* **Action:** Ensure the internal methods that resolve the target variable for the numerator (and denominator for ratios) extract and apply the `names()` of the target scope to the final output dataframe columns. This will override default output names like `estimate_n` or `estimate_d` with the user-provided strings.

**4. Add Unit Tests**
* **Files:** `tests/testthat/test-EvalHandler.R` (or similar aggregation tests) and `tests/testthat/test-PostStratifiedEstimator.R`
* **Action:** Add assertions verifying that `colnames(res)` exactly match user inputs when executing queries like `aggregate(tree(my_vol = VOLCFNET))` and `estimate(tree(my_vol = VOLCFNET))`.