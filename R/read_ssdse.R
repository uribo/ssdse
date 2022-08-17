#' Read specific SSDSE data file
#'
#' @param path path to file
#' @param lang column name language
#' @param ... not use
#' @export
#' @rdname read_ssdse
read_ssdse_a <- function(path, lang, ...) {
  lang <-
    rlang::arg_match(lang,
                     c("en", "ja"))
  d <-
    tibble::as_tibble(
      utils::read.csv(path,
               skip = 0,
               fileEncoding = "Shift_JIS"))
  # # assertr::verify(d, dim(d) == c(1743, 127))
  # year
  d <-
    d[-1, ]
  d <-
    d[-1, ]
  if (lang == "ja") {
    d <-
      d %>%
      convert_ssdse_colname(id = "A", lang = "ja")
  } else if (lang == "en") {
    d <-
      d %>%
      convert_ssdse_colname(id = "A", lang = "en")
  }
  d
}

#' @export
#' @rdname read_ssdse
read_ssdse_b <- function(path, lang, ...) {
  lang <-
    rlang::arg_match(lang,
                     c("en", "ja"))
  d <-
    tibble::as_tibble(
      utils::read.csv(path,
               skip = 0,
               fileEncoding = "Shift_JIS"))
  # assertr::verify(d, dim(d) == c(565, 110)) # B
  d <-
    d[-1, ]
  if (lang == "ja") {
    d <-
      d %>%
      convert_ssdse_colname(id = "A", lang = "ja")
  } else if (lang == "en") {
    d <-
      d %>%
      convert_ssdse_colname(id = "A", lang = "en")
  }
  d
}

convert_ssdse_colname <- function(data, id, lang) {
  ja <- en <- NULL
  ssdse_colnames <- NULL
  lang <-
    rlang::arg_match(lang,
                     c("en", "ja"))
  id <-
    rlang::arg_match(id,
                     c("A", "B", "C", "D", "E"))
  df_col_ssdse <-
    col_ssdse %>%
    purrr::pluck(id)
  vars <-
    colnames(data)
  if (lang == "ja") {
    ssdse_colnames <-
      df_col_ssdse %>%
      dplyr::filter(en %in% vars) %>%
      dplyr::select(ja, en) %>%
      tibble::deframe()
  } else if (lang == "en") {
    ssdse_colnames <-
      df_col_ssdse %>%
      dplyr::filter(ja %in% vars) %>%
      dplyr::select(en, ja) %>%
      tibble::deframe()
  }
  dplyr::rename(data, !!!ssdse_colnames)
}
