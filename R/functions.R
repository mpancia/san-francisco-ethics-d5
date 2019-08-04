
#' Extract the filers in a collection of raw SF ethics data
#'
#' @param raw_data
#'
#' @return
extract_filers <- function(raw_data){
  raw_data %>%
    dplyr::distinct(filer_id, filer_naml) %>%
    rename(name = "filer_naml",
           id = "filer_id") %>%
    mutate(name = str_to_upper(name))
}


load_filers <- function(con, file_location){
  filers_on_load <- '
  MERGE (n: Filer {id: row.id})
  ON CREATE SET
  n = row,
  n.name = row.name,
  n.id = row.id;
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = filers_on_load)
}

#' Extract the donors from the raw ethics data
#'
#' Assumes that first/last names are unique.
#'
#' @param raw_data
#'
#' @return
extract_donors <- function(raw_data){
  raw_data %>%
    mutate(first_name = str_to_upper(tran_namf),
           last_name = str_to_upper(tran_naml),
           name = paste(first_name, last_name)) %>%
    distinct(name, first_name, last_name)
}


load_donors <- function(con, file_location){
  donors_on_load <- '
  MERGE (n: Donor {name: row.name})
  ON CREATE SET
  n = row,
  n.name = row.name,
  n.first_name = row.first_name,
  n.last_name = row.last_name;
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = donors_on_load)
}

#' Extract the donations from the raw ethics data
#'
#' @param raw_data
#'
#' @return
extract_donations <- function(raw_data){
  raw_data %>%
    transmute(
      id = tran_id,
      date = lubridate::date(tran_date),
      amount = tran_amt1
    )
}

load_donations <- function(con, file_location){
  donations_on_load <- '
  MERGE (n: Donation {id: row.id, date: date(row.date), amount: toInt(row.amount)})
  ON CREATE SET
  n = row,
  n.id = row.id,
  n.date = date(row.date),
  n.amount = toInt(row.amount);
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = donations_on_load)
}

#' Extract the employers from the raw ethics data
#'
#' @param raw_data
extract_employers <- function(raw_data){
  raw_data %>%
    transmute(
      name = str_to_upper(tran_emp)
    ) %>%
    unique()
}

load_employers <- function(con, file_location){
  employers_on_load <- '
  MERGE (n: Employer {name: row.name})
  ON CREATE SET
  n = row,
  n.name = row.name;
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = employers_on_load)
}

#' Extract employee / employee relationsips
#'
#' @param raw_data
extract_employment <- function(raw_data){
  raw_data %>%
    mutate(first_name = str_to_upper(tran_namf),
           last_name = str_to_upper(tran_naml),
           donor_name = paste(first_name, last_name),
           employer_name = str_to_upper(tran_emp)) %>%
    unique()
}

load_employment <- function(con, file_location){
  employment_on_load <- '
  MATCH (employer: Employer {name: row.employer_name}), (donor: Donor {name: row.donor_name})
  MERGE (employer)-[:EMPLOYED]->(donor)-[:WORKED_AT]->(employer);
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = employment_on_load)
}

#' Extract employee / employee relationsips
#'
#' @param raw_data
extract_donation_donors <- function(raw_data){
  raw_data %>%
    transmute(first_name = str_to_upper(tran_namf),
           last_name = str_to_upper(tran_naml),
           donor_name = paste(first_name, last_name),
            id = tran_id,
            date = lubridate::date(tran_date)) %>%
    unique()
}

load_donation_donors <- function(con, file_location){
  employment_on_load <- '
  MATCH (donation: Donation {id: row.id, date: date(row.date)}), (donor: Donor {name: row.donor_name})
  MERGE (donor)-[:MADE_DONATION]->(donation)-[:MADE_BY]->(donor);
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = employment_on_load)
}

#' Extract donation / filer relationshi;s
#'
#' @param raw_data
extract_donation_filers <- function(raw_data){
  raw_data %>%
    transmute(donation_id = tran_id,
              date = lubridate::date(tran_date),
              filer_id = filer_id) %>%
    unique()
}

load_donation_filers <- function(con, file_location){
  filers_on_load <- '
  MATCH (donation: Donation {id: row.donation_id, date: date(row.date)}), (filer: Filer {id: row.filer_id})
  MERGE (donation)-[:MADE_TO]->(filer)-[:HAS_DONATION]->(donation);
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = filers_on_load)
}


#' Extract zip codes
#'
#' @param raw_data
extract_donor_zips <- function(raw_data){
  raw_data %>%
    transmute(first_name = str_to_upper(tran_namf),
              last_name = str_to_upper(tran_naml),
              name = paste(first_name, last_name),
              city = str_to_upper(tran_city),
              state = str_to_upper(tran_state),
              zip_code = str_split_fixed(tran_zip4, "-", 2)[,1]) %>%
    filter(str_length(zip_code) == 5) %>%
    unique()
}

load_donor_zips <- function(con, file_location){
  zips_on_load <- '
  MATCH (n: Donor {name: row.name})
  MERGE (zipcode: ZipCode {zip_code: row.zip_code})
  MERGE (state: State {abbreviation: row.state})
  MERGE (n)-[:RESIDES_IN]->(zipcode)-[:HAS_RESIDENT]->(n)
  MERGE (city: City {name: row.city})-[:IS_IN]->(state)-[:CONTAINS]->(city)
  MERGE (zipcode)-[:IS_IN]->(city)-[:CONTAINS]->(zipcode)
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = zips_on_load)
}

load_schema <- function(con) {
  send_cypher(path = here("data/schema.cypher"), con=con)
}

load_industry_taxonomy <- function(con, file_location){
  taxonomy_on_load <- '
  MERGE (industry: Industry {name: row.name})
  '
  load_csv(url = paste0("file:///", file_location), con=con, header=TRUE, as="row", on_load = taxonomy_on_load)
}
