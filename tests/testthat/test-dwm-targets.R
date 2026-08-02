test_that("DWM component helpers validate and name targets", {
  cwd <- .parse_target_spec(dwm(dwm_cwd(VOLCF)), "test")
  expect_identical(cwd$slot, "dwm")
  expect_identical(cwd$targets, "dwm_cwd_VOLCF")
  expect_identical(.resolve_dwm_columns(cwd$dwm_targets[[1]], FALSE), "CWD_VOLCF_UNADJ")
  expect_identical(.resolve_dwm_columns(cwd$dwm_targets[[1]], TRUE), "CWD_VOLCF_ADJ")

  named <- .parse_target_spec(dwm(dwm_cwd(volume = VOLCF)), "test")
  expect_identical(named$targets, "volume")

  all_fwd <- .parse_target_spec(dwm(dwm_fwd(CARBON, size = "all")), "test")
  expect_identical(all_fwd$targets, "dwm_fwd_all_CARBON")
  expect_identical(
    .resolve_dwm_columns(all_fwd$dwm_targets[[1]], TRUE),
    c("FWD_SM_CARBON_ADJ", "FWD_MD_CARBON_ADJ", "FWD_LG_CARBON_ADJ")
  )
})

test_that("DWM component helpers require the dwm() scoping wrapper", {
  expect_error(
    .parse_target_spec(dwm_cwd(VOLCF), "test"),
    "wrapped in `dwm"
  )
  expect_error(
    .parse_target_spec(dwm(CWD_VOLCF_UNADJ), "test"),
    "component helper"
  )
  expect_error(
    .parse_target_spec(dwm(), "test"),
    "component helper"
  )
})

test_that("all supported DWM component and attribute combinations resolve", {
  helpers <- list()
  constructors <- c(
    CWD = "dwm_cwd",
    PILE = "dwm_pile",
    FUEL = "dwm_fuel",
    DUFF = "dwm_duff",
    LITTER = "dwm_litter"
  )
  for (component in names(constructors)) {
    for (attribute in names(.dwm_support[[component]])) {
      helpers[[length(helpers) + 1]] <- rlang::eval_tidy(
        rlang::call2(
          constructors[[component]],
          rlang::sym(attribute)
        )
      )
    }
  }
  for (size in c("SM", "MD", "LG", "ALL")) {
    for (attribute in names(.dwm_support$FWD)) {
      helpers[[length(helpers) + 1]] <- rlang::eval_tidy(
        rlang::call2(
          "dwm_fwd",
          rlang::sym(attribute),
          size = size
        )
      )
    }
  }

  expect_true(all(vapply(
    helpers,
    function(helper) inherits(helper[[1]], "fiaplyr_dwm_target"),
    logical(1)
  )))
  expect_identical(
    .resolve_dwm_columns(dwm_duff(DRYBIO)[[1]], TRUE),
    "DUFF_BIOMASS"
  )
  expect_identical(
    .resolve_dwm_columns(dwm_litter(DRYBIO)[[1]], FALSE),
    "LITTER_BIOMASS"
  )
})

test_that("DWM component helpers reject unsupported inputs", {
  expect_error(dwm_cwd(HEIGHT), "Valid choices are")
  expect_error(dwm_fuel(VOLCF), "Valid choices are")
  expect_error(dwm_fwd(CARBON), "requires `size`")
  expect_error(dwm_fwd(CARBON, size = "XL"), "Valid choices are")
  expect_error(dwm_cwd(VOLCF, CARBON), "exactly one attribute")
  expect_error(dwm_cwd(log(VOLCF)), "bare names")
})
