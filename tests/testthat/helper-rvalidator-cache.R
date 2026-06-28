.rvalidator_parameter_cache <- new.env(parent = emptyenv())

.get_rvalidator_parameters <- function(parameter_name) {
  key <- paste0("parameters::", parameter_name)

  if (!exists(key, envir = .rvalidator_parameter_cache, inherits = FALSE)) {
    assign(
      key,
      rvalidator::parameters(parameter_name),
      envir = .rvalidator_parameter_cache
    )
  }

  get(key, envir = .rvalidator_parameter_cache, inherits = FALSE)
}
