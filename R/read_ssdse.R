#' Read specific SSDSE data file
#'
#' @param path path to file
#' @param lang column name language
#' @param pack packing for common variables
#' @param ... not use
#' @import rlang
#' @export
#' @rdname read_ssdse
read_ssdse_a <- function(path, lang, pack = TRUE, ...) {
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

  d <-
    d %>%
    readr::type_convert(col_types = readr::cols(
      .default = readr::col_double(),
      SSDSE.A.2022 = readr::col_character(),
      prefecture = readr::col_character(),
      municipality = readr::col_character()))
  tweak_ssdse_out(d, id = "A", lang, pack)
}

#' @export
#' @rdname read_ssdse
read_ssdse_b <- function(path, lang, pack = TRUE, ...) {
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

  d <-
    d %>%
    readr::type_convert(col_types = readr::cols(
      .default = readr::col_double(),
      Code = readr::col_character(),
      Prefecture = readr::col_character()))
  tweak_ssdse_out(d, id = "B", lang, pack)
}

#' @export
#' @rdname read_ssdse
read_ssdse_c <- function(path, lang, ...) {
  lang <-
    rlang::arg_match(lang,
                     c("en", "ja"))
  d <-
    tibble::as_tibble(
      utils::read.csv(path,
                      skip = 0,
                      fileEncoding = "Shift_JIS"))
  d <-
    d[-1, ]
  d <-
    d %>%
    readr::type_convert(
      col_types = readr::cols(
        .default = readr::col_double(),
        SSDSE.C.2022 = readr::col_character(),
        Prefecture = readr::col_character(),
        City = readr::col_character()))
  tweak_ssdse_out(d, id = "C", lang, pack = FALSE)
}

tweak_ssdse_out <- function(data, id, lang, pack) {
  if (lang == "ja") {
    d <-
      data %>%
      convert_ssdse_colname(id = id, lang = "ja")
  } else if (lang == "en") {
    d <-
      data %>%
      convert_ssdse_colname(id = id, lang = "en")
  }
  if (pack == TRUE) {
    d <-
      pack_ssdse_vars(d)
  }
  d
}

#' @param version file version
#' @export
#' @rdname read_ssdse
read_ssdse_e <- function(path, version = 2, lang, ...) {
  lang <-
    rlang::arg_match(lang,
                     c("en", "ja"))
  d <-
    tibble::as_tibble(
      utils::read.csv(path,
                      skip = 0,
                      fileEncoding = "Shift_JIS"))

  d <-
    d[-c(1, 2), ]
  d <-
    d %>%
    readr::type_convert(
      col_types = readr::cols(
        .default = readr::col_integer(),
        SSDSE.E.2022v2 = readr::col_character(),
        prefecture = readr::col_character(),
        A4103 = readr::col_double(),
        H2130 = readr::col_double(),
        H5614 = readr::col_double()))
  tweak_ssdse_out(d, id = "E", lang, pack = FALSE) %>%
    prefix_ssdse_e_year()
}

pack_ssdse_vars <- function(data) {
  data %>%
    tidyr::pack(
      A = tidyselect::matches("^A[0-9]{1,}$"),
      B = tidyselect::matches("^B[0-9]{1,}$"),
      C = tidyselect::matches("^C[0-9]{1,}$"),
      D = tidyselect::matches("^D[0-9]{1,}$"),
      E = tidyselect::matches("^E[0-9]{1,}$"),
      `F` = tidyselect::matches("^F[0-9]{1,}$"),
      G = tidyselect::matches("^G[0-9]{1,}$"),
      H = tidyselect::matches("^H[0-9]{1,}$"),
      I = tidyselect::matches("^I[0-9]{1,}$"),
      J = tidyselect::matches("^J[0-9]{1,}$"),
      K = tidyselect::matches("^K[0-9]{1,}$"),
      L = tidyselect::matches("^L[0-9]{1,}$"),
      !!intToUtf8(c(20154L, 21475L, 12539L, 19990L, 24111L)) := tidyselect::starts_with(
        intToUtf8(c(20154L, 21475L, 12539L, 19990L, 24111L))),
      !!intToUtf8(c(33258L, 28982L, 29872L, 22659L)) := tidyselect::starts_with(
        intToUtf8(c(33258L, 28982L, 29872L, 22659L))),
      !!intToUtf8(c(32076L, 28168L, 22522L, 30436L)) := tidyselect::starts_with(
        intToUtf8(c(32076L, 28168L, 22522L, 30436L))),
      !!intToUtf8(c(25945L, 32946L)) := tidyselect::starts_with(
        intToUtf8(c(25945L, 32946L))),
      !!intToUtf8(c(21172L, 20685L)) := tidyselect::starts_with(
        intToUtf8(c(21172L, 20685L))),
      !!intToUtf8(c(25991L, 21270L, 12539L, 12473L, 12509L, 12540L, 12484L)) := tidyselect::starts_with(
        intToUtf8(c(25991L, 21270L, 12539L, 12473L, 12509L, 12540L, 12484L))),
      !!intToUtf8(c(23621L, 20303L)) := tidyselect::starts_with(
        intToUtf8(c(23621L, 20303L))),
      !!intToUtf8(c(20581L, 24247L, 12539L, 21307L, 30274L)) := tidyselect::starts_with(
        intToUtf8(c(20581L, 24247L, 12539L, 21307L, 30274L))),
      !!intToUtf8(c(31119L, 31049L, 12539L, 31038L, 20250L, 20445L, 38556L)) := tidyselect::starts_with(
        intToUtf8(c(31119L, 31049L, 12539L, 31038L, 20250L, 20445L, 38556L))),
      !!intToUtf8(c(23478L, 35336L)) := tidyselect::starts_with(
        intToUtf8(c(23478L, 35336L))),
      .names_sep = "_") %>%
    purrr::discard(
      ~ identical(ncol(.x), 0L))
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

prefix_ssdse_e_year <- function(data) {
  purrr::set_names(data,
                   c(colnames(data)[1:2],
                     paste(colnames(data)[-c(1,2)],
                           c(rep("2020", 19),
                             rep("2017", 3),
                             rep("2016", 16),
                             rep("2019", 5),
                             rep("2020", 12),
                             rep("2018", 5),
                             "2016",
                             "2018",
                             rep("2020", 3),
                             rep("2018", 5),
                             rep("2019", 5),
                             rep("2016", 3),
                             rep("2019", 3),
                             rep("2018", 3),
                             rep("2019", 2),
                             rep("2020", 4)),
                           sep = "_")))
}
