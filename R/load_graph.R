load_filers <- function(con, file_location) {
  filers_on_load <- "
  MERGE (n: Filer {id: row.id})
  ON CREATE SET
  n = row,
  n.name = row.name,
  n.id = row.id;
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = filers_on_load
  )
}

load_occupations <- function(con, file_location) {
  occupations_on_load <- "
  MERGE (n: Occupation {name: row.name})
  RETURN n.name
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = occupations_on_load
  )
}

load_donors <- function(con, file_location) {
  donors_on_load <- "
  MERGE (n: Donor {name: row.name})
  ON CREATE SET
  n = row,
  n.name = row.name,
  n.first_name = row.first_name,
  n.last_name = row.last_name;
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = donors_on_load
  )
}
load_donations <- function(con, file_location) {
  donations_on_load <- "
  MERGE (n: Donation {id: row.id, date: date(row.date), amount: toInt(row.amount)})
  ON CREATE SET
  n = row,
  n.id = row.id,
  n.date = date(row.date),
  n.amount = toInt(row.amount);
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = donations_on_load
  )
}
load_employers <- function(con, file_location) {
  employers_on_load <- "
  MERGE (n: Employer {name: row.name})
  ON CREATE SET
  n = row,
  n.name = row.name;
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = employers_on_load
  )
}
load_employment <- function(con, file_location) {
  employment_on_load <- "
  MATCH (employer: Employer {name: row.employer_name}), (donor: Donor {name: row.donor_name})
  MERGE (employer)-[:EMPLOYED]->(donor)-[:WORKED_AT]->(employer);
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = employment_on_load
  )
}
load_donation_donors <- function(con, file_location) {
  employment_on_load <- "
  MATCH (donation: Donation {id: row.id, date: date(row.date)}), (donor: Donor {name: row.donor_name})
  MERGE (donor)-[:MADE_DONATION]->(donation)-[:MADE_BY]->(donor);
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = employment_on_load
  )
}

load_donation_filers <- function(con, file_location) {
  filers_on_load <- "
  MATCH (donation: Donation {id: row.donation_id, date: date(row.date)}), (filer: Filer {id: row.filer_id})
  MERGE (donation)-[:MADE_TO]->(filer)-[:HAS_DONATION]->(donation);
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = filers_on_load
  )
}

load_donor_occupations <- function(con, file_location) {
  occupations_on_load <- "
  MATCH (occupation: Occupation {name: row.occupation_name}), (donor: Donor {name: row.donor_name})
  MERGE (donor)-[:WORKED_AS]->(occupation)-[:HAD_WORKER]->(donor);
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = occupations_on_load
  )
}

load_donor_zips <- function(con, file_location) {
  zips_on_load <- "
  MATCH (donor: Donor {name: row.name})
  MERGE (zipcode: ZipCode {zip_code: row.zip_code})
  MERGE (state: State {abbreviation: row.state})
  MERGE (city: City {name: row.city})
  MERGE (donor)-[:RESIDES_IN]->(zipcode)-[:HAS_RESIDENT]->(donor)
  MERGE (donor)-[:RESIDES_IN]->(city)-[:HAS_RESIDENT]->(donor)
  MERGE (donor)-[:RESIDES_IN]->(state)-[:HAS_RESIDENT]->(donor)
  "
  load_csv(url = paste0("file:///", file_location), con = con, header = TRUE, as = "row", on_load = zips_on_load)
}

load_schema <- function(con) {
  send_cypher(path = here("data/schema.cypher"), con = con)
}

load_industry_taxonomy <- function(con, file_location) {
  taxonomy_on_load <- "
  MERGE (industry: Industry {name: row.name})
  "
  load_csv(url = paste0("file:///", file_location), con = con, header = TRUE, as = "row", on_load = taxonomy_on_load)
}

load_occupation_class_taxonomy <- function(con, file_location) {
  taxonomy_on_load <- "
  MERGE (oc: OccupationClass {name: row.name})
  "
  load_csv(url = paste0("file:///", file_location), con = con, header = TRUE, as = "row", on_load = taxonomy_on_load)
}
