
# regular expression to parse the MATLAB file downloaded from CEIC
MATREX = stringr::str_glue(
  "timeseries\\((?P<timeseries>.*?)\\);(?:.|\\n)+?'seriesId','(?P<seriesId>.*?)',\\
  'srCode','(?P<srCode>.*?)','country','(?P<country>.*?)','seriesName','(?P<seriesName>.*?)',\\
  'nameLocal','(?P<nameLocal>.*?)','Unit','(?P<unit>.*?)','frequency','(?P<frequency>.*?)',\\
  'source','(?P<source>.*?)','firstObsDate','(?P<firstObsDate>.*?)','LastObsDate',\\
  '(?P<LastObsDate>.*?)','multiplierCode','(?P<multiplierCode>.*?)','tLastUpdTime',\\
  '(?P<tLastUpdTime>.*?)','seriesStatus','(?P<seriesStatus>.*?)','remarks',\\
  (?P<remarks>.*?),'functionInformation','(?P<functionInformation>.*?)'\\);"
)

#' Convert MATLAB time series into dataframe
#' @importFrom purrr %||%
#' @noRd
compileTS = function(x, name=NULL, json=TRUE) {
  if (isTRUE(json)) {
    y = jsonlite::fromJSON(x)
    s = y[['value']]
    t = y[['date']]
  } else {
    s = stringr::str_split(stringr::str_match(x, "\\[(.*)\\]")[2], ";")[[1]]
    t = stringr::str_split(stringr::str_match(x, "\\{(.*)\\}")[2], ",")[[1]]
  }
  df = data.frame(lubridate::mdy(t), as.numeric(s))
  names(df) = c("date", name %||% "value")
  return(df)
}

#' Convert time series encoded in MATLAB format into JSON format
#' @noRd
compileJSON = function(x) {
  s = stringr::str_split(stringr::str_match(x, "\\[(.*)\\]")[2], ";")[[1]]
  t = stringr::str_split(stringr::str_match(x, "\\{(.*)\\}")[2], ",")[[1]]
  t = stringr::str_replace_all(t, "'", '"')
  json = stringr::str_glue(
    '{{"date":[{stringr::str_flatten(t, collapse=",")}],\\
    "value":[{stringr::str_flatten(s, collapse=",")}]}}')
  return(json)
}

#' Parse CEIC file into data.frame
#'
#' Parse the MATLAB data downloaded from CEIC into a dataframe
#'
#' @param path path to the MATLAB file
#' @param jsonData stores the time series in JSON format
#'
#' @return a dataframe contains the description as well as the JSON-encoded
#' data for each series in the data file
#'
#' @importFrom rlang .data
#' @export
parse2DF = function(path, jsonData=TRUE) {
  text = readr::read_file(path)
  match = re2::re2_match_all(text, MATREX)
  df = tibble::as_tibble(unclass(match[[1]])) %>%
    # recompile time series into JSON format for easier future access
    dplyr::mutate(data = purrr::map_chr(.data$timeseries, compileJSON)) %>%
    dplyr::select("seriesId":"functionInformation", "data")
  return(df)
}


#' Compile Time Series Data
#'
#' Compile the information contained in the dataframe returned from [parse2DF()]
#' into operational time series data (as `data.frame`, `ts`, or `zoo`).
#'
#' It is strongly recommended that the dataframe only contains time series of
#' the same frequency. Though mixed frequencies are also supported.
#' Only monthly/quarterly/yearly series are supported.
#' Unless the output object is `ts`, it is not guaranteed that the returned
#' series are regular.
#'
#' @param df the dataframe returned from [parse2DF()]
#' @param freq only grab a certain frequency
#' @param out the output object type
#'
#' @return an object of the specified type
#' @export
#'
#' @examples
#' \dontrun{
#' df = parse2DF("data-raw/testseries.m")
#' df_q = dplyr::filter(frequency == "Quarterly")
#' df_q.ts = grabTS(df_q, out = "ts")
#' }
grabTS = function(df, freq=c("mixed", "y", "q", "m"), out=c("df", "ts", "zoo")) {
  freq = match.arg(freq)
  df = switch (freq,
    "y" = df[df$frequency == "Yearly", ],
    "q" = df[df$frequency == "Quarterly", ],
    "m" = df[df$frequency == "Monthly", ],
    df
  )
  code = df[['srCode']]
  freqs = df[['frequency']]
  series = df[['data']]
  if (length(setdiff(freqs, c("Yearly", "Quarterly", "Monthly")))>0) {
    stop(paste("Unsupported frequencies in the dataset",
               "(only support monthly/quarterly/yearly series)"))
  }
  df = purrr::map2(series, code, ~compileTS(.x,.y)) %>%
    purrr::reduce(~dplyr::full_join(.x,.y, by="date")) %>%
    dplyr::arrange(date)
  out = match.arg(out)
  if (out == 'df') df
  else {
    maxFreq = ifelse('Monthly' %in% freqs, 12, ifelse('Quarterly' %in% freqs, 4, 1))
    t = if (maxFreq == 12) zoo::as.yearmon(df[['date']])
    else if (maxFreq == 4) zoo::as.yearqtr(df[['date']])
    else lubridate::year(df[['date']])
    z = zoo::zoo(as.matrix(df[,-1]), order.by = t)
    if (out == 'ts') stats::as.ts(z)
    else z
  }
}


#' Parse CEIC file into time series data
#'
#' Parse the MATLAB data downloaded from CEIC into the specified time series
#' object (`data.frame`, `ts`, or `zoo`). See [grabTS()] for details.
#'
#' @param path path to the MATLAB file
#' @param out the output object type
#'
#' @return an object of the specified type
#' @export
parse2TS = function(path, out=c("df", "ts", "zoo")) {
  df = parse2DF(path)
  grabTS(df, out=out)
}

