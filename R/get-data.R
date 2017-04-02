#' Accessing openFDA API device information.
#'
#' The \code{ropenfda} package provides classes and methods for
#' conveniently accessing, manipulating and visualizing data openFDA API.
#'
#' @docType package
#' @name ropenfda
NULL

#' Construct an object representing a query to openFDA repository.
#'
#' Creating an S4 object used to fetch data from openFDA API by specifying
#' a filter on the values in the openFDA database. Allows to access all
#' device openFDA databases.
#'
#' @param query     character A query request.
#' @param count_var character A count variable, if empty the raw events are
#'   returned.
#' @param db        character A name of the openFDA database. Options are:
#'   For category 'device': "event", "classification", "510k", "pma",
#'                          "registrationlisting", "recall", "enforcement",
#'                          "udi"
#' @param category  character A category of the information to be retrieved.
#'   Available options are: "device", "drug", "food".
#' @param api_key   character A user api key.
#' @param limit     numeric   A number of events to be returned. Not applicable
#'   to count data.
#' @return Return S4 object representing query but not the data.
#' @examples
#'
#' # A complex query returning raw data
#' query <- paste(
#'  '((adverse_event_flag == "N" and single_use_flag == "Y") or',
#'  '(adverse_event_flag == "Y" and single_use_flag == "N")) and',
#'  'date_received in [20130101,20130105]')
#'
#' obj1 <- openfda(query, limit = 10)
#'
#' # A simple query, returning any occurences of included phrase
#' obj2 <- openfda('source_type == "company representative"',
#'         limit = 10)
#'
#' # Serches in any field
#' obj3 <- openfda("'company representative'", limit = 10)
#'
#' # Returns exact phrase match
#' obj4 <- openfda('source_type.exact == "COMPANY REPRESENTATIVE"',
#'                     limit = 10)
#'
#' obj5 <- openfda('source_type.exact == "REPRESENTATIVE"',
#'                     limit = 10)
#'
#' # Complex count query
#' query <- paste0(
#'   '((adverse_event_flag == "N" and single_use_flag == "Y") or',
#'   '(adverse_event_flag == "Y" and single_use_flag == "N"))')
#'
#' obj6 <- openfda(query, "date_facility_aware", limit = 10)
#'
#' obj7 <- openfda(query, limit = 10, category = "drug")
#'
#' obj8 <- openfda(query, limit = 10, category = "food")
#' @export
get_query <- function(query = "",
                      count_var = NULL,
                      db = c(
                        "event",
                        "classification",
                        "510k",
                        "pma",
                        "registrationlisting",
                        "recall",
                        "enforcement",
                        "udi",
                        "label"
                      ),
                      category = c(
                        "device",
                        "drug",
                        "food"
                      ),
                      api_key = "",
                      limit = 5100) {

  db = match.arg(db)
  category = match.arg(category)

  if (is.null(count_var) || nchar(count_var) == 0) {

    obj <- new("RawQuery",
               query = query,
               db = db,
               category = category,
               api_key = api_key,
               limit = limit)
  }else{
    obj <- new("CountQuery",
               query = query,
               count_var = count_var,
               db = db,
               category = category,
               api_key = api_key)
  }
  obj
}

#' @describeIn  get_query Convenience function to retrieve data from API in a
#' one stop.
#' @param ...     Parameters to pass to \code{get_query}
#' @param json    logical if TRUE, store the raw JSON response in the object
#' @param quiet   logical if TRUE, no warning messages are generated
#' @export
openfda <- function(..., json = FALSE, quiet = FALSE){
  query <- get_query(...)
  data  <- fetch(query, json = json, quiet = quiet)
  data
}