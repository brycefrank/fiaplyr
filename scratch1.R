library(httr)  #Tools for Working with URLs and HTTP

# this function will help format retrieved estimates into a more readable data frame
format_estimate = function(respList) {

  return(as.data.frame(do.call(rbind, respList)))

}

# this function will accept a FIADB-API fullreport URL and return dataframes for the estimates as well as subtotals, and totals where available.
fiadb_api_GET = function(url){

  # make request
  resp <- httr::GET(url=url)
  # parse response from JSON to R list
  respObj <- httr::content(resp, "parsed", encoding = "ISO-8859-1")

  # create empty output list
  outputList = list()

  # add estimates data frame to output list
  if (! is.null(respObj$estimates)) {
      outputList$estimates <- format_estimate(respObj$estimates)
  } else  {
      print("Problem with URL or API. No estimate returned.")
      return(list())
  }


  #Use lapply to break apart subtotal list, then call sapply to format with names
  if (! is.null(respObj$subtotals))
        outputList$subtotals <- sapply(lapply(respObj$subtotals, "["),
                                       format_estimate, simplify = F, USE.NAMES = T)

  # totals data frame
  if (! is.null(respObj$totals))
    outputList$totals <- format_estimate(respObj$totals)


  # add estimate metadata, doesn't need to be reformatted
  if (! is.null(respObj$metadata)) outputList$metadata <- respObj$metadata

  return(outputList)
}

# example of usage
url <- "https://apps.fs.usda.gov/fiadb-api/fullreport?rselected=Land%20Use%20-%20Major&cselected=Land%20use&snum=79&wc=102020&outputFormat=NJSON"
getData <- fiadb_api_GET(url=url)

# estimate data frame
getData$estimates

# list of subtotal data frames
getData$subtotals

# totals data frame
getData$totals

# list of estimate metadata elements
getData$metadata

# access SQL used the generate estimates
getData$metadata$sql
