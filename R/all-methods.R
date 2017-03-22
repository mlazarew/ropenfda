#' @include help-functions.R

# initialize Query -------------------------------------------------------------
setMethod("initialize", "Query",
          function(.Object, ...) {
            .Object@query_time <- Sys.time()
            callNextMethod(.Object, ...)
          }
)

# initialize Data --------------------------------------------------------------
setMethod("initialize", "Data",
          function(.Object, ...) {
            .Object@query_time <- Sys.time()
            callNextMethod(.Object, ...)
          }
)

# initialize RawQuery ----------------------------------------------------------
setMethod("initialize", "RawQuery",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@query_api  <-
              create_api_call(query     = .Object@query,
                              api_key   = .Object@api_key,
                              category  = .Object@category,
                              db        = .Object@db,
                              limit     = .Object@limit)
            .Object
          }
)

# initialize CountQuery --------------------------------------------------------
setMethod("initialize", "CountQuery",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@query_api  <-
              create_api_call(query     = .Object@query,
                               api_key   = .Object@api_key,
                               category  = .Object@category,
                               db        = .Object@db,
                               count_var = .Object@count_var)
            .Object
          }
)

# show Query -------------------------------------------------------------------
#' Show method for class \code{Query}.
#' @param object \code{Query} object.
#' @export
setMethod("show",
          signature(object = "Query"),
          function(object) {
            cat("object:", class(object)[1], fill = TRUE)
            cat("================================================", fill = TRUE)
            cat("query time stamp:", as.character(object@query_time), fill = TRUE)
            cat("fda category:", object@category, fill = TRUE)
            cat("fda database:", object@db, fill = TRUE)
            cat("query:", object@query, fill = TRUE)
            cat("query api:", object@query_api, fill = TRUE)
          }
)

# show Data --------------------------------------------------------------------
#' Show method for class \code{Data}.
#' @param object \code{Data} object.
#' @export
setMethod("show",
          signature(object = "Data"),
          function(object) {
            show(object@result)
          }
)

# show RawQuery ----------------------------------------------------------------
#' Show method for class \code{RawQuery}.
#' @param object \code{RawQuery} object.
#' @export
setMethod("show",
          signature(object = "RawQuery"),
          function(object) {
            callNextMethod()
            cat("limit:", object@limit, fill = TRUE)
          }
)
# show CountQuery --------------------------------------------------------------
#' Show method for class \code{CountQuery}.
#' @param object \code{CountQuery} object.
#' @export
setMethod("show",
          signature(object = "CountQuery"),
          function(object) {
            callNextMethod()
            cat("count variable:", object@count_var, fill = TRUE)
          }
)

# names Data -------------------------------------------------------------------
#' names method for class \code{Data}.
#' @param x \code{Data} object.
#' @export
setMethod("names",
          signature(x = "Data"),
          function(x) {
            names(x@result)
          }
)

#' Generic as_tibble
#' @param x \code{Data} object.
#' @rdname  as_tibble-method
#' @inherit as_tibble
setGeneric("as_tibble", function(x) standardGeneric("as_tibble"))

# as_tibble Data ----------------------------------------------------------------
#' @rdname  as_tibble-method
#' @export
setMethod("as_tibble", "Data", function(x) x@result)

setGeneric("[")

# [i,] Data --------------------------------------------------------------------
#' Subsetting \code{Data} class.
#' @inheritDotParams base::`[`
#' @inheritParams base::`[`
#' @export
setMethod("[", signature(x = "Data", i = "numeric", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop = TRUE) {
            if (anyNA(i) || (any(i < 0) && any(i > 0)))
              stop("problem with NA or mixtures of signs in 'i'")
            x@result <- x@result[i,]
            x
          }
)

# [, j] Data --------------------------------------------------------------
#' Subsetting \code{Data} class.
#' @inheritDotParams base::`[`
#' @inheritParams base::`[`
#' @export
setMethod("[", signature(x = "Data", i = "missing", j = "numeric", drop = "missing"),
          function(x, i, j, ..., drop = TRUE) {
            if (anyNA(j) || (any(j < 0) && any(j > 0)))
              stop("problem with NA or mixtures of signs in 'j'")
            x@result <- x@result[, j]
            x
          }
)

# [, "name"] Data --------------------------------------------------------------
#' Subsetting \code{Data} class.
#' @inheritDotParams base::`[`
#' @inheritParams base::`[`
#' @export
setMethod("[", signature(x = "Data", i = "missing", j = "character", drop = "missing"),
          function(x, i, j, ..., drop = TRUE) {
            x@result <- x@result[, j]
            x
          }
)

# [i, j] Data --------------------------------------------------------------
#' Subsetting \code{Data} class.
#' @inheritDotParams base::`[`
#' @inheritParams base::`[`
#' @export
setMethod("[", signature(x = "Data", i = "numeric", j = "numeric", drop = "missing"),
          function(x, i, j, ..., drop = TRUE) {
            if (anyNA(i) || (any(i < 0) && any(i > 0)))
              stop("problem with NA or mixtures of signs in 'i'")
            if (anyNA(j) || (any(j < 0) && any(j > 0)))
              stop("problem with NA or mixtures of signs in 'j'")
            x@result <- x@result[i, j]
            x
          }
)

# [i,"name"] Data --------------------------------------------------------------
#' Subsetting \code{Data} class.
#' @inheritDotParams base::`[`
#' @inheritParams base::`[`
#' @export
setMethod("[", signature(x = "Data", i = "numeric", j = "character", drop = "missing"),
          function(x, i, j, ..., drop = TRUE) {
            if (anyNA(i) || (any(i < 0) && any(i > 0)))
              stop("problem with NA or mixtures of signs in 'i'")
            x@result <- x@result[i, j]
            x
          }
)

# as.data.frame Data -----------------------------------------------------------
#' Casting \code{Data} object to data.frame.
#' @inheritParams base::as.data.frame
#' @export
setMethod("as.data.frame",
          signature(x = "Data"),
          function(x, ...)
            as.data.frame(x@result, ...)
)

# head Data --------------------------------------------------------------------
#' \code{head} of the \code{Data} class.
#' @inheritParams utils::head
#' @export
setMethod("head",
          signature(x = "Data"),
          function(x, ...) {
            x@result <- head(x@result, ...)
            x
          }
)

# trail Data -------------------------------------------------------------------
#' \code{tail} of the \code{Data} class.
#' @inheritParams utils::head
#' @export
setMethod("tail",
          signature(x = "Data"),
          function(x, ...) {
            x@result <- tail(x@result, ...)
            x
          }
)

# dim Data ---------------------------------------------------------------------
#' \code{dim} method for the \code{Data} class.
#' @inheritParams base::dim
#' @export
setMethod("dim",
          signature(x = "Data"),
          function(x) {
            dim(x@result)
          }
)

# nrow Data --------------------------------------------------------------------
#' \code{nrow} of the \code{Data} class.
#' @param x \code{Data} object.
#' @export
setMethod("nrow",
          signature(x = "Data"),
          function(x) {
            nrow(x@result)
          }
)

# ncol Data --------------------------------------------------------------------
#' \code{ncol} of the \code{Data} class.
#' @param x \code{Data} object.
#' @export
setMethod("ncol",
          signature(x = "Data"),
          function(x) {
            ncol(x@result)
          }
)

# plot CountData ---------------------------------------------------------------
#' \code{plot} of the \code{CountData} class.
#' @param x \code{CountData} object.
#' @export
setMethod("plot",
          signature(x = "CountData"),
          function(x) {
            p <- NULL
            x_names <- names(x@result)
            if ('term' %in% x_names) {
              p <- ggplot2::ggplot(data = x@result,
                                   ggplot2::aes_string("term", "count")) +
                ggplot2::geom_bar(stat = 'identity')
            }

            if ('time' %in% x_names) {
              p <- ggplot2::ggplot(data = x@result,
                                   ggplot2::aes_string("time", "count")) +
                ggplot2::geom_point()
            }
            p
          }
)

# plot RawData -----------------------------------------------------------------
#' \code{plot} of the \code{RawData} class.
#' @param x \code{RawData} object.
#' @param i numeric of character specifing index for what to plot on the x-axis
#' @param j numeric of character specifing index for what to plot on the x-axis
#' @param ... not used
#' @param y not used
#' @export
setMethod("plot", c("RawData", "missing"),
          function(x, y, i=1, j=2, ...) {

            if (is.numeric(i))
              x_name <- names(x)[i]
            else
              x_name <- i

            if (is.numeric(j))
              y_name <- names(x)[j]
            else
              y_name <- j

            if (is.list(x@result[[x_name]]) || is.list(x@result[[y_name]]))
              stop("plot method does not know how to deal with plotting lists")

            ggplot2::ggplot(data = x@result, ggplot2::aes_string(x_name, y_name)) +
              ggplot2::geom_point()
          }
)

#' Function to fetch JSON form API.
#'
#' Function to fetch JSON form API
#' @param query_api character Representing the query to API.
fetch_json <- function(query_api) {
    httr::content(httr::GET(query_api), "text")
}

# convert_fields_to_date -------------------------------------------------------
#' Internal method to convert fields to dates
#' @param obj \code{RawData} object
#' @param word character Key word suggesting date format
convert_fields_to_date <- function(obj, word) {
  # Which field names contain word
  date_fields <- names(obj)[grepl(word, names(obj))]
  # Convert them to dates
  date_val <- purrr::map(date_fields,
                         function(x_name) lubridate::ymd(obj@result[[x_name]],
                                                         quiet = TRUE))
  obj@result[, date_fields] <- date_val
  obj
}

# fetch generic ----------------------------------------------------------------
#' Generic function for fetching data from the API based on the \code{Query}
#' object.
#'
#' @return An object representing the fetching results and including the original
#' \code{Query} object. The class returned is specific to the combination of
#' the category and database interrogated.
#' @param .Object An instance of the \code{Query} subclass.
#' @param ...     additional parameters like json, see openfda.
#' @param json    logical if TRUE, store the raw JSON response in the object.
#' @param quiet   logical if TRUE, no warning messages are generated.
#' @rdname fetch-methods
#' @export
setGeneric("fetch", function(.Object, ...) standardGeneric("fetch"))

# fetch CountQuery -------------------------------------------------------------
#' @rdname fetch-methods
#' @export
setMethod("fetch", "CountQuery",
          function(.Object, json = FALSE, quiet = FALSE) {
            ret <- new(paste0("Count",
                              firstup(.Object@category),
                              firstup(.Object@db)),
                       query = .Object
            )

            # Execute the query against the API
            json_result <- fetch_json(.Object@query_api)

            # Store json response
            if (json) ret@json <- json_result

            flat_result <- jsonlite::fromJSON(json_result, flatten = TRUE)

            # Check for errors
            if (!is.null(flat_result[["meta"]])) {
              ret@result  <- tibble::as_tibble(flat_result$results)
              ret@meta    <- as.data.frame(flat_result$meta,
                                           stringsAsFactors = FALSE)
            } else  if (flat_result$error$code == "NOT_FOUND") {
              ret@result  <- tibble::tibble() # Return empty tibble
              ret@meta    <- as.data.frame(flat_result$error,
                                           stringsAsFactors = FALSE)
            }
            else stop(flat_result$error$message)

            # Convert date fields to Date
            ret <- convert_fields_to_date(ret, "time")

            ret
          })

# fetch RawQuery ---------------------------------------------------------------
#' @rdname fetch-methods
#' @export
setMethod("fetch", "RawQuery",
          function(.Object, json = FALSE, quiet = FALSE) {
            ret <- new(paste0("Raw",
                              firstup(.Object@category),
                              firstup(.Object@db)),
                       query = .Object
            )

            if (.Object@limit <= 0) .Object@limit = 5100

            tmpData <- list()
            n       <- 0
            skip    <- 0
            total   <- 0

            repeat {
              # Print the progress report
              if (n %% 1000 == 0 && n > 0)
                print(paste("Retrieved so far", n, "records"))

              # Prepare the final query
              query <- create_api_call(query     = .Object@query,
                                       api_key   = .Object@api_key,
                                       category  = .Object@category,
                                       db        = .Object@db,
                                       count_var = "",
                                       limit     = .Object@limit - n,
                                       skip      = skip)

              # Execute the query against the API
              json_result <- fetch_json(query)

              # Store json response
              if (json) ret@json <- json_result

              flat_result <- jsonlite::fromJSON(json_result, flatten = TRUE)

              # Append the current results to the previous results
              if (is.null(flat_result[["error"]])) { # Everything is going well
                total        <- flat_result$meta$results$total
                skip         <- skip + 1
                tmpData[[skip]] <- flat_result$results
                n            <- n + nrow(tmpData[[skip]])
                ret@meta <- as.data.frame(flat_result$meta,
                                          stringsAsFactors = FALSE)
              } else {# No results or error
                # Check for errors
                # If error is other than NOT_FOUND that means that there is
                # some serious problem
                if (flat_result$error$code != "NOT_FOUND")
                  stop(flat_result$error$message)

                if (n == 0) # No results for the initial query
                  ret@meta <- as.data.frame(flat_result$error,
                                            stringsAsFactors = FALSE)
                break
              }

              # Fix BUG in the openfda UDI sterilization.sterilization_methods
              # field sometimes is of tpye 'list' and sometimes is a
              # 'character string'
              # Convert the list into the string
              if (.Object@db == 'udi' && .Object@category == 'device')
                if (class(tmpData[[skip]]$sterilization.sterilization_methods) == "list")
                  tmpData[[skip]] <-
                transform(tmpData[[skip]],
                          sterilization.sterilization_methods =
                            sapply(tmpData[[skip]]$sterilization.sterilization_methods,
                                   function(x) paste(x, collapse = ";")),
                          stringsAsFactors = FALSE)

              # Do not exeed the specified limit of rows
              if (n >= .Object@limit) break

              # Do not request more then 5100 records
              if (skip == 51) break
            }

            # Assemply result from the pieces
            tmpData <- dplyr::bind_rows(tmpData)

            n <- nrow(tmpData)

            if (!quiet && total > nrow(tmpData))
              warning(paste("Only", nrow(tmpData),
                            "records retrieved from the total of", total, "\n"),
                      call. = FALSE)

            ret@result <- tibble::as_tibble(tmpData)

            # Convert date fields to Date
            ret <- convert_fields_to_date(ret, "date")
            ret
          }
)

# unfold generic ---------------------------------------------------------------
#' Method to unfold the nested raw data inside the results from the API.
#'
#' These methods work only on the raw data. These data structures might be
#' nested data.frames. Count data is never nested. \code{unfold} methods are
#' customized for individual openFDA databases. These functions make assumptions
#' what might be important to the user how to unfold these nested data
#' structures. For instance the device event database results contain records
#' that may contain multiple patients and multiple devices per one row.
#' \code{unfold} method would create a cartesian product of patients and devices
#' with the rest of the columns copied.
#'
#' @param .Object An instance of the class representing the outcome from the API
#' from a specific category and database combination.
#' @rdname unfold-methods
#' @export unfold
setGeneric("unfold", function(.Object) standardGeneric("unfold"))

# unfold RawDeviceEvent --------------------------------------------------------
#' @rdname unfold-methods
#' @export
setMethod("unfold",
          signature(.Object = "RawDeviceEvent"),
          function(.Object) {

            tmp <- .Object@result

            if ("remedial_action" %in% names(tmp))
              tmp$remedial_action <-
                sapply(tmp$remedial_action,
                       function(x) paste0(x, collapse = ";"))

            if ("source_type" %in% names(tmp))
              tmp$source_type     <-
                sapply(tmp$source_type,
                       function(x) paste0(x, collapse = ";"))

            if ("type_of_report" %in% names(tmp))
              tmp$type_of_report  <-
                sapply(tmp$type_of_report,
                       function(x) paste0(x, collapse = ";"))

            d1 <- tidyr::unnest_(tmp, "mdr_text",
                                 .sep = ".")
            d2 <- tidyr::unnest_(tmp[c("report_number", "patient")],
                                 "patient",
                                 .sep = ".")
            d3 <- tidyr::unnest_(tmp[c("device", "report_number")],
                                 "device",
                                 .sep = ".")
            tmp2 <- merge(d1,
                          d2,
                          by.x = c("report_number", "mdr_text.patient_sequence_number"),
                          by.y = c("report_number", "patient.patient_sequence_number"),
                          all.x = TRUE,
                          all.y = FALSE)

            tmp2 <- merge(tmp2,
                          d3,
                          by = c("report_number"),
                          all = TRUE)

            .Object@result <- tmp2
            convert_fields_to_date(.Object, "date")
          })

# unfold RawDeviceClassification -----------------------------------------------
#' @rdname unfold-methods
#' @export
setMethod("unfold",
          signature(.Object = "RawDeviceClassification"),
          function(.Object) {

            tmp <- .Object@result

            if ("openfda.registration_number" %in% names(tmp))
              tmp$openfda.registration_number <-
                sapply(tmp$openfda.registration_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.k_number" %in% names(tmp))
              tmp$openfda.k_number <-
                sapply(tmp$openfda.k_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.pma_number" %in% names(tmp))
              tmp$openfda.pma_number <-
                sapply(tmp$openfda.pma_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.fei_number" %in% names(tmp))
              tmp$openfda.fei_number <-
                sapply(tmp$openfda.fei_number,
                       function(x) paste0(x, collapse = ";"))

            .Object@result <- tmp
            convert_fields_to_date(.Object, "date")
          })

# unfold RawDevice510k ---------------------------------------------------------
#' @rdname unfold-methods
#' @export
setMethod("unfold",
          signature(.Object = "RawDevice510k"),

          function(.Object) {

            tmp <- .Object@result

            if ("openfda.registration_number" %in% names(tmp))
              tmp$openfda.registration_number <-
                sapply(tmp$openfda.registration_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.k_number" %in% names(tmp))
              tmp$openfda.k_number <-
                sapply(tmp$openfda.k_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.pma_number" %in% names(tmp))
              tmp$openfda.pma_number <-
                sapply(tmp$openfda.pma_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.fei_number" %in% names(tmp))
              tmp$openfda.fei_number <-
                sapply(tmp$openfda.fei_number,
                       function(x) paste0(x, collapse = ";"))

            .Object@result <- tmp
            convert_fields_to_date(.Object, "date")
          })

# unfold RawDevicePma ----------------------------------------------------------
#' @rdname unfold-methods
#' @export
setMethod("unfold",
          signature(.Object = "RawDevicePma"),

          function(.Object) {
            tmp <- .Object@result

            if ("openfda.registration_number" %in% names(tmp))
              tmp$openfda.registration_number <-
                sapply(tmp$openfda.registration_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.k_number" %in% names(tmp))
              tmp$openfda.k_number <-
                sapply(tmp$openfda.k_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.pma_number" %in% names(tmp))
              tmp$openfda.pma_number <-
                sapply(tmp$openfda.pma_number,
                       function(x) paste0(x, collapse = ";"))

            if ("openfda.fei_number" %in% names(tmp))
              tmp$openfda.fei_number <-
                sapply(tmp$openfda.fei_number,
                       function(x) paste0(x, collapse = ";"))

            .Object@result <- tmp
            convert_fields_to_date(.Object, "date")
          })

# unfold RawDeviceRegistrationlisting ------------------------------------------
#' @rdname unfold-methods
#' @export
setMethod("unfold",
          signature(.Object = "RawDeviceRegistrationlisting"),
          function(.Object) {

            tmp <- .Object@result

            if ("establishment_type" %in% names(tmp))
              tmp$establishment_type <-
                sapply(tmp$establishment_type,
                       function(x) paste0(x, collapse = ";"))

            if ("proprietary_name" %in% names(tmp))
              tmp$proprietary_name     <-
                sapply(tmp$proprietary_name,
                       function(x) paste0(x, collapse = ";"))

            .Object@result <- tidyr::unnest_(tmp, "products",
                                             .sep = ".")
            convert_fields_to_date(.Object, "date")
          })

# unfold RawDeviceRecall -------------------------------------------------------
#' @rdname unfold-methods
#' @export
setMethod("unfold",
          signature(.Object = "RawDeviceRecall"),
          function(.Object) {

            tmp <- .Object@result

            if ("k_numbers" %in% names(tmp))
              tmp$k_numbers <-
                sapply(tmp$k_numbers,
                       function(x) paste0(x, collapse = ";"))

            if ("pma_numbers" %in% names(tmp))
              tmp$pma_numbers     <-
                sapply(tmp$pma_numbers,
                       function(x) paste0(x, collapse = ";"))

            .Object@result <- tmp
            convert_fields_to_date(.Object, "date")
          })

# unfold RawDeviceEnforcement --------------------------------------------------
#' @rdname unfold-methods
#' @export
setMethod("unfold",
          signature(.Object = "RawDeviceEnforcement"),
          function(.Object) {
            convert_fields_to_date(.Object, "date")
          })

# unfold RawDeviceUdi ----------------------------------------------------------
#' @rdname unfold-methods
#' @export
setMethod("unfold",
          signature(.Object = "RawDeviceUdi"),
          function(.Object) {

            stop("This function is not yet implemented in this version
                 of the package.")
            convert_fields_to_date(.Object, "date")
          })