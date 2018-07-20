#' JSON parser used by this package
#'
#' Currently this just renames \code{\link{fromJSON}} in the \code{rjson} package.
#' @importFrom rjson fromJSON
#' @export
#'
#' @param json_str json string to parse
#' @param file file to read json_Str from
#' @param method method used to parse json
#' @param unexpected.escape handling escape characters, one of error, skip, keep
#' @param simplify if TRUE, convert json-encoded lists to vectors
#' @return a JSON object
#'
#' @seealso \code{\link{fromJSON}}
#' @examples
#' json_parser('{"a":true, "b":false, "c":null}')

json_parser <- rjson::fromJSON

#' JSON writer used by this package
#'
#' Currently this just renames \code{\link{toJSON}} in the \code{rjson} package.
#' @importFrom rjson toJSON
#' @export
#'
#' @param x object to write to json
#' @param method method used to write json
#' @return a string with JSON encoding of object
#'
#' @seealso \code{\link{toJSON}}
#' @examples
#' json_writer(1:10)
json_writer <- rjson::toJSON

#' Random ID generator for epiviz charts
#'
#' @param prefix prefix for random ID
#' @return random ID
rand_id <- function(prefix="") {
  sprintf("%s_%d", prefix, floor(stats::runif(1, 1e8, 1e9-1)))
}