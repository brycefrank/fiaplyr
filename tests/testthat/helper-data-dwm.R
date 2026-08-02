setup_dwm_test_db <- function() {
  con <- setup_status_test_db()

  plt_cn <- c(101, 103, 104, 104, 201, 203, 204, 204)
  condid <- c(1, 1, 1, 2, 1, 1, 1, 2)
  value <- c(10, 30, 40, 5, 20, 60, 80, 10)

  dwm <- data.frame(
    CN = seq_along(plt_cn),
    EVALID = 1001,
    PLT_CN = plt_cn,
    CONDID = condid,
    PHASE = rep(c("P2", "P3"), length.out = length(plt_cn)),
    FUEL_BIOMASS = value * 100,
    FUEL_CARBON = value * 50,
    DUFF_BIOMASS = value * 200,
    DUFF_CARBON = value * 100,
    LITTER_BIOMASS = value * 80,
    LITTER_CARBON = value * 40,
    stringsAsFactors = FALSE
  )

  add_pair <- function(base, multiplier = 1) {
    dwm[[paste0(base, "_UNADJ")]] <<- value * multiplier
    dwm[[paste0(base, "_ADJ")]] <<- value * multiplier * 2
  }

  for (attribute in c("VOLCF", "DRYBIO", "CARBON", "LPA")) {
    add_pair(paste0("CWD_", attribute))
  }
  for (size in c("SM", "MD", "LG")) {
    size_multiplier <- match(size, c("SM", "MD", "LG"))
    for (attribute in c("VOLCF", "DRYBIO", "CARBON")) {
      add_pair(paste0("FWD_", size, "_", attribute), size_multiplier)
    }
  }
  for (attribute in c("VOLCF", "DRYBIO", "CARBON")) {
    add_pair(paste0("PILE_", attribute), 4)
  }

  dwm$FWD_MD_CARBON_UNADJ[[4]] <- NA_real_
  dwm$FWD_MD_CARBON_ADJ[[4]] <- NA_real_

  DBI::dbWriteTable(con, "COND_DWM_CALC", dwm)
  con
}
