#' This is an internal function for parsing the query to the proper API grammar.
#' @param s character Input string.
str2api <- function(s){

  # Equal
  s <- gsub("==",        ":",     s, fixed = TRUE)
  s <- gsub("=",         ":",     s, fixed = TRUE)

  # Logical OR
  s <- gsub("\\bor\\b",  "+",     s, ignore.case = TRUE)

  # Logical AND
  s <- gsub("\\band\\b", "+AND+", s, ignore.case = TRUE)

  # Range assigment
  s <- gsub("\\bin\\b",  ":",     s, ignore.case = TRUE)

  # Range join
  s <- gsub(",",         "+TO+",  s, fixed = TRUE)

  # Spaces in the values needs to be converted into `+`
  repl_space <- function(x)
    gsub('(?<!["\'])[ ]+(?![\'"])', "+", x, perl = TRUE)

  s <- gsubfn::gsubfn('".*?"',   repl_space, s)
  s <- gsubfn::gsubfn('\'.*?\'', repl_space, s)

  # Remove spaces
  s <- gsub(" ", "", s, fixed = TRUE)

  # Convert single quotes to double quotes
  s <- gsub("'",        "\"",     s, fixed = TRUE)
  s
}

#' This is an internal function to create the API query
#' @param query     character Query string.
#' @param api_key   character Private api key.
#' @param category  character Category like 'device'.
#' @param db        character Database like 'event'.
#' @param count_var character Name of counting field, if raw keep "".
#' @param limit     integer   For raw, set limit of records to be fetch <5100.
#' @param skip      integer   Skip * 100 records.
create_api_call <- function(query, api_key, category, db, count_var = "", limit = 0, skip = 0) {

  # Converting to the server grammar
  s_query <- paste0('&search=', str2api(query))

  s_key   <- ifelse(nchar(api_key) > 0,
                    paste0('api_key=', api_key, '&'),
                    "")

  s_count <- ifelse(nchar(count_var) > 0,
                    paste0('&count=', count_var),
                    "")

  s_limit <- ifelse(limit > 0,
                    paste0('&limit=', min(limit, 100)),
                    "")

  s_skip <- ifelse(skip > 0,
                    paste0('&skip=', skip * 100),
                    "")
  # Return query
  paste0('https://api.fda.gov/', category,
         '/', db, '.json?', s_key, s_query, s_count, s_limit, s_skip)
}

#' This is an internal function to capitalize the first letter.
#' @param x Input string.
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}