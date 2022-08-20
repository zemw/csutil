#' Abbreviate Strings
#'
#' @param x input vector of strings.
#' @param method abbreviation method. `'c'` stands for keeping capital letters;
#' `'d'` stands for digits; `'s'` stands for suffix including 'ytd', 'YoY', etc.
#' @param rm characters represented by regular expression to be removed.
#' @param keep characters represented by regular expression to be kept.
#' @param sub a 2-element vector containing the starting and ending position
#' of the sub-string to be extracted.
#' @param unique if `TRUE`, each abbreviated strings will be guaranteed unique.
#' Duplications will be suffixed by dots.
#'
#' @return a vector of abbreviated strings
#' @export
abbr = function(x,
                method = c("cds", "cd", "cs", "c"),
                rm = NULL,
                keep = NULL,
                sub = c(1L, 100000L),
                unique = TRUE) {

  stopifnot(is.character(x))
  method = match.arg(method)

  s =  "ytd|YoY|QoQ|MoM|SA"
  c = "[A-Z]+"
  d = "[0-9]+"

  regex = switch (
    method,
    "cds" = paste(s, c, d, sep = '|'),
    "cd" = paste(c, d, sep = '|'),
    "cs" = paste(c, s, sep = '|'),
    "c" = c
  )

  # keep or remove certain character
  if (!is.null(rm) && is.character(rm)) {
    x = gsub(rm, '', x)
  }
  if (!is.null(keep) && is.character(keep)) {
    regex = paste(keep, regex, sep = '|')
  }

  if (sub[1] == 1L) {
    # skip CN: prefix by default
    x = substring(x, 5L)
  } else {
    x = substring(x, sub[1], sub[2])
  }

  # final output
  y = stringr::str_extract_all(x, regex) %>%
    purrr::map_chr( ~ paste(., collapse = ''))

  # remove duplication
  while (any(duplicated(y))) {
    y = purrr::map2_chr(
      y, duplicated(y),
      ~ifelse(isTRUE(.y), paste0(.x, '.'), .x)
    )
  }
  return(y)
}
