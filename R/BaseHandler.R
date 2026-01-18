#' Virtual Parent Class for fiaplyr
#'
#' @slot db A DBIConnection object.
#' @export
setClass("BaseHandler",
  slots = list(db = "DBIConnection"),
  contains = "VIRTUAL"
)
