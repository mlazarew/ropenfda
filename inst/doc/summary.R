## ---- echo=FALSE, message = FALSE----------------------------------------
opt <- options()
options(tibble.max_extra_cols = 2)
library(hash)

## ------------------------------------------------------------------------
library(ropenfda)

## ------------------------------------------------------------------------
res <- openfda(query = "", count_var = "date_facility_aware")
head(res, 3)
class(res)

## ---- fig.width=7--------------------------------------------------------
plot(res)

## ------------------------------------------------------------------------
res <- openfda(query = "", category = "drug", count_var = "companynumb")
head(res, 3)
class(res)

res <- openfda(query = "", category = "food", count_var = "date_created")
head(res, 3)
class(res)

## ------------------------------------------------------------------------
res <- openfda(query = "", limit = 100)
res[1:4, c(3,4,10,13)]

## ---- message=FALSE, warning=FALSE, fig.width=7--------------------------
plot(res, i = "date_of_event", j = "event_type")

## ------------------------------------------------------------------------
res <- openfda(query = "", db = "recall", limit = 10)
head(res)

## ------------------------------------------------------------------------
cat("dim:", dim(res), "nrow:", nrow(res), "ncol:", ncol(res))
names(res)

## ------------------------------------------------------------------------
head(as_tibble(res), 3)
class(as_tibble(res))

## ------------------------------------------------------------------------
res <- openfda(query = "", count_var = "date_facility_aware")
x <- as.data.frame(res)
class(x)
head(x)

## ------------------------------------------------------------------------
query_string <- '((adverse_event_flag == "N" and single_use_flag == "Y") or (adverse_event_flag == Y+N+MP+P and single_use_flag == N+Y)) and date_received in [20130101,20130105]'
res1 <- openfda(query_string, limit = 10)
dim(res1)

## ------------------------------------------------------------------------
res2 <- openfda(query = 'manufacturer_g1_name == "MEDTRONIC NEUROMODULATION"', 
                limit = 1)

## ------------------------------------------------------------------------
res2 <- openfda(query = 'manufacturer_g1_name == "MEDTRONIC"', limit = 1)

## ------------------------------------------------------------------------
res2 <- openfda(query = 'manufacturer_g1_name == "MEDTRONIC"+"NEUROMODULATION"', 
                limit = 1)

## ------------------------------------------------------------------------
res3 <- openfda(query = 'manufacturer_g1_name.exact == "MEDTRONIC NEUROMODULATION"', 
                limit = 1)

## ------------------------------------------------------------------------
query <- get_query(query = 'manufacturer_g1_name == "MEDTRONIC"', limit = 1)
query

## ------------------------------------------------------------------------
data <- fetch(query)
data

## ------------------------------------------------------------------------
class(data)

## ------------------------------------------------------------------------
res <- openfda(query = "", limit = 100)
res[1:3, c("patient", "device", "mdr_text")]

## ------------------------------------------------------------------------
res_unfold <- unfold(res)
data_unfold <- as_tibble(res_unfold)
dim(res)
dim(data_unfold)
head(setdiff(names(data_unfold), names(res)))

## ---- echo=FALSE---------------------------------------------------------
options(opt)

