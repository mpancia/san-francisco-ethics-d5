#' Extract the filers in a collection of raw SF ethics data
#'
#' @param raw_data
#'
#' @return
extract_filers <- function(raw_data) {
  raw_data %>%
    dplyr::distinct(filer_id, filer_naml) %>%
    rename(
      name = "filer_naml",
      id = "filer_id"
    ) %>%
    mutate(name = str_to_upper(name))
}


#' Extract the occupations in a collection of raw SF ethics data
#'
#' @param raw_data
#'
#' @return
extract_occupations <- function(raw_data) {
  raw_data %>%
    dplyr::distinct(tran_occ) %>%
    transmute(name = tran_occ) %>%
    mutate(name = str_to_upper(name))
}


#' Extract the donors from the raw ethics data
#'
#' Assumes that first/last names are unique.
#'
#' @param raw_data
#'
#' @return
extract_donors <- function(raw_data) {
  raw_data %>%
    mutate(
      first_name = str_to_upper(tran_namf),
      last_name = str_to_upper(tran_naml),
      name = paste(first_name, last_name)
    ) %>%
    distinct(name, first_name, last_name)
}


#' Extract the donations from the raw ethics data
#'
#' @param raw_data
#'
#' @return
extract_donations <- function(raw_data) {
  raw_data %>%
    transmute(
      id = tran_id,
      date = lubridate::date(tran_date),
      amount = tran_amt1
    )
}


#' Extract the employers from the raw ethics data
#'
#' @param raw_data
extract_employers <- function(raw_data) {
  raw_data %>%
    transmute(
      name = str_to_upper(tran_emp)
    ) %>%
    unique()
}


#' Extract employee / employee relationsips
#'
#' @param raw_data
extract_employment <- function(raw_data) {
  raw_data %>%
    mutate(
      first_name = str_to_upper(tran_namf),
      last_name = str_to_upper(tran_naml),
      donor_name = paste(first_name, last_name),
      employer_name = str_to_upper(tran_emp)
    ) %>%
    unique()
}


#' Extract employee / employee relationsips
#'
#' @param raw_data
extract_donation_donors <- function(raw_data) {
  raw_data %>%
    transmute(
      first_name = str_to_upper(tran_namf),
      last_name = str_to_upper(tran_naml),
      donor_name = paste(first_name, last_name),
      id = tran_id,
      date = lubridate::date(tran_date)
    ) %>%
    unique()
}


#' Extract donation / filer relationshi;s
#'
#' @param raw_data
extract_donation_filers <- function(raw_data) {
  raw_data %>%
    transmute(
      donation_id = tran_id,
      date = lubridate::date(tran_date),
      filer_id = filer_id
    ) %>%
    unique()
}

#' Extract donation / occupation relationships
#'
#' @param raw_data
extract_donor_occupations <- function(raw_data) {
  raw_data %>%
    transmute(
      first_name = str_to_upper(tran_namf),
      last_name = str_to_upper(tran_naml),
      donor_name = paste(first_name, last_name),
      occupation_name = str_to_upper(tran_occ)
    ) %>%
    unique()
}


#' Extract zip codes
#'
#' @param raw_data
extract_donor_zips <- function(raw_data) {
  raw_data %>%
    transmute(
      first_name = str_to_upper(tran_namf),
      last_name = str_to_upper(tran_naml),
      name = paste(first_name, last_name),
      city = str_to_upper(tran_city),
      state = str_to_upper(tran_state),
      zip_code = str_split_fixed(tran_zip4, "-", 2)[, 1]
    ) %>%
    filter(str_length(zip_code) == 5) %>%
    unique()
}
