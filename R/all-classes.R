#' @import methods
NULL

# Query ------------------------------------------------------------------------
#' Query class.
#'
#' \code{Query} is a virtual S4 class that represents a general query to the
#' openFDA API.
#' @slot query_date  POSIXct   The query creation date.
#' @slot query       character The query string provided by the user.
#' @slot query_api   noquote   The query string constructed for the API call.
#' @slot db          character openFDA database name.
#' @slot category    character openFDA category name.
#' @slot api_key     character The private key.
#' @name Query-class
#' @rdname Query-class
#' @exportClass Query
setClass("Query",
         slots = c(
           query_time  = "POSIXct",
           query       = "character",
           query_api   = "character",
           db          = "character",
           category    = "character",
           api_key     = "character"
         ),
         contains = "VIRTUAL"
)

# CountQuery -------------------------------------------------------------------
#' CountQuery class.
#'
#' \code{CountQuery} is an S4 class that represent a query for count data to the
#' open FDA API. It inherits from \code{Query} class.
#' @slot count_var  character Specific slot for \code{CountQuery} class
#' representing the name of the counting field.
#' @name CountQuery-class
#' @rdname Query-class
#' @exportClass CountQuery
setClass("CountQuery",
         slots = c(
           count_var = "character"
         ),
         contains = "Query"
)

# RawQuery ---------------------------------------------------------------------
#' RawQuery class.
#'
#' \code{RawQuery} is an S4 class that represent a query for raw data to the
#' openFDA API. It inherits from \code{Query} class.
#' @slot limit  numeric Specific slot for \code{RawQuery} class representing
#' a maximum number of rows requested from API. openFDA API is not allowing
#' returning more than 5100 records.
#' @name RawQuery-class
#' @rdname Query-class
#' @exportClass RawQuery
setClass("RawQuery",
         slots = c(
           limit = "numeric"
         ),
         contains = "Query"
)

# Data -------------------------------------------------------------------------
#' Data class.
#'
#' \code{Data} is an S4 virtual class that represents a data fetched from the
#' openFDA API.
#'
#' @slot query_time  POSIXct    The time the data was fetched.
#' @slot result      data.frame The resulting data.
#' @slot meta        data.frame The meta-information such as number of total
#' records found in the database, error messages, license, date of the last
#' update of the database, etc.
#' @slot json        character  Raw JSON response from the API. Contains values
#' only if fetched with the option \code{json = TRUE}.
#' @name Data-class
#' @rdname Data-class
#' @exportClass Data
setClass("Data",
         slots = c(
           query_time = "POSIXct",
           result     = "data.frame",
           meta       = "data.frame",
           json       = "character"
         ),
         contains = "VIRTUAL"
)

# CountData --------------------------------------------------------------------
#' CountData class.
#'
#' \code{CountData} is an S4 class that represents a data fetched from the
#' open FDA API. It inherits from \code{Data} class.
#' @slot query  CountQuery Object representing the original query.
#' @name CountData-class
#' @rdname Data-class
#' @exportClass CountData
setClass("CountData",
         slots = c(
           query = "CountQuery"
         ),
         contains = "Data"
)

# RawData ----------------------------------------------------------------------
#' RawData class.
#'
#' \code{RawData} is an S4 class that represent a data fetched from the
#' open FDA API. It inherits from \code{Data} class.
#' @slot query  RawQuery Object representing the original query.
#' @name RawData-class
#' @rdname Data-class
#' @exportClass RawData
setClass("RawData",
         slots = c(
           query = "RawQuery"
         ),
         contains = "Data"
)

# Device UDI -------------------------------------------------------------------
#' \code{RawDeviceUdi} A specialized subclass representing fetched data from API.
#' @rdname Data-class
setClass("RawDeviceUdi", contains = "RawData")

#' \code{CountDeviceUdi} A specialized subclass representing fetched data
#' from API.
#' @rdname Data-class
setClass("CountDeviceUdi", contains = "CountData")

# Device Event -----------------------------------------------------------------
#' \code{RawDeviceEvent} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("RawDeviceEvent", contains = "RawData")

#' \code{CountDeviceEvent} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("CountDeviceEvent", contains = "CountData")

# Device Classification --------------------------------------------------------
#' \code{RawDeviceClassification} A specialized subclass representing fetched
#' data from API.
#' @rdname Data-class
setClass("RawDeviceClassification", contains = "RawData")

#' \code{CountDeviceClassification} A specialized subclass representing fetched
#' data from API.
#' @rdname Data-class
setClass("CountDeviceClassification", contains = "CountData")

# Device 510k ------------------------------------------------------------------
#' \code{RawDevice510k} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("RawDevice510k", contains = "RawData")

#' \code{CountDevice510k} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("CountD evice510k", contains = "CountData")

# Device PMA -------------------------------------------------------------------
#' \code{RawDevicePma} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("RawDevicePma", contains = "RawData")

#' \code{CountDevicePma} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("CountDevicePma", contains = "CountData")

# Device Registrationlisting ---------------------------------------------------
#' \code{RawDeviceRegistrationlisting} A specialized subclass representing
#' fetched data from API.
#' @rdname Data-class
setClass("RawDeviceRegistrationlisting", contains = "RawData")

#' \code{CountDeviceRegistrationlisting} A specialized subclass representing
#' fetched data from API.
#' @rdname Data-class
setClass("CountDeviceRegistrationlisting", contains = "CountData")

# Device Recall ----------------------------------------------------------------
#' \code{RawDeviceRecall} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("RawDeviceRecall", contains = "RawData")

#' \code{CountDeviceRecall} A specialized subclass representing fetched data
#' from API.
#' @rdname Data-class
setClass("CountDeviceRecall", contains = "CountData")

# Device Enforcement -----------------------------------------------------------
#' \code{RawDeviceEnforcement} A specialized subclass representing fetched data
#' from API.
#' @rdname Data-class
setClass("RawDeviceEnforcement", contains = "RawData")

#' \code{CountDeviceEnforcement} A specialized subclass representing fetched
#' data from API.
#' @rdname Data-class
setClass("CountDeviceEnforcement", contains = "CountData")

# Drug Event -------------------------------------------------------------------
#' \code{RawDrugEvent} A specialized subclass representing fetched data from API.
#' @rdname Data-class
setClass("RawDrugEvent", contains = "RawData")

#' \code{CountDrugEvent} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("CountDrugEvent", contains = "CountData")

# Drug Label -------------------------------------------------------------------
#' \code{RawDrugLabel} A specialized subclass representing fetched data from API.
#' @rdname Data-class
setClass("RawDrugLabel", contains = "RawData")

#' \code{CountDrugLabel} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("CountDrugLabel", contains = "CountData")

# Drug Enforcement -------------------------------------------------------------
#' \code{RawDrugEnforcement} A specialized subclass representing fetched data
#' from API.
#' @rdname Data-class
setClass("RawDrugEnforcement", contains = "RawData")

#' \code{CountDrugEnforcement} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("CountDrugEnforcement", contains = "CountData")

# Food Enforcement -------------------------------------------------------------
#' \code{RawFoodEnforcement} A specialized subclass representing fetched data
#' from API.
#' @rdname Data-class
setClass("RawFoodEnforcement", contains = "RawData")

#' \code{CountFoodEnforcement} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("CountFoodEnforcement", contains = "CountData")

# Food Event -------------------------------------------------------------------
#' \code{RawFoodEvent} A specialized subclass representing fetched data from API.
#' @rdname Data-class
setClass("RawFoodEvent", contains = "RawData")

#' \code{CountFoodEvent} A specialized subclass representing fetched data from
#' API.
#' @rdname Data-class
setClass("CountFoodEvent", contains = "CountData")

