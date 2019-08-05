# This gets the raw data from the SF OpenData Socrata portal for:
#   Campaign contributions (form 460): https://data.sfgov.org/City-Management-and-Ethics/Campaign-Finance-FPPC-Form-460-Schedule-A-Monetary/q66q-d2tr
#   Data key: https://data.sfgov.org/City-Management-and-Ethics/Campaign-Finance-Data-Key/wygs-cc76
# The contributions are filtered to contain only individual donors from 2018 onwards for supervisor races.
# We also write the outputs to CSVs for archival.

DB_URL <- Sys.getenv("GRAPHENEDB_URL") %>% url_parse()
DB_URL <- paste0("http://", DB_URL$domain, ":", DB_URL$port)
DB_USER <- Sys.getenv("GRAPHENEDB_BOLT_USER")
DB_PASSWORD <- Sys.getenv("GRAPHENEDB_BOLT_PASSWORD")

con <- neo4j_api$new(url = DB_URL, user = DB_USER, password = DB_PASSWORD)

raw_data_plan <- drake_plan(
  data_key = RSocrata::read.socrata("https://data.sfgov.org/resource/wygs-cc76.json"),
  data_key_csv = write_csv(data_key, file_out("data/data_key.csv")),
  raw_data = RSocrata::read.socrata(paste("https://data.sfgov.org/resource/q66q-d2tr.json?$where=",
                                          "date_extract_y(Thru_date) >= 2018 AND",
                                          "Entity_Cd = 'IND' AND",
                                          "contains(lower(Filer_NamL), 'supervisor')")),
  raw_data_csv = write_csv(raw_data, file_out("data/raw_data.csv"))
)

extract_entities_plan <- drake_plan(
  filers_df = extract_filers(raw_data),
  filers_csv = write_csv(filers_df, file_out("data/filers.csv")),
  donors_df = extract_donors(raw_data),
  donors_csv = write_csv(donors_df, file_out("data/donors.csv")),
  donations_df = extract_donations(raw_data),
  donations_csv = write_csv(donations_df, file_out("data/donations.csv")),
  employers_df = extract_employers(raw_data),
  employers_csv = write_csv(employers_df, file_out("data/employers.csv")),
)

extract_relationships_plan <- drake_plan(
  employment_df = extract_employment(raw_data),
  employment_csv = write_csv(employment_df, file_out("data/employment.csv")),
  donation_donors_df = extract_donation_donors(raw_data),
  donation_donors_csv = write_csv(donation_donors_df, file_out("data/donation_donors.csv")),
  donation_filers_df = extract_donation_filers(raw_data),
  donation_filers_csv = write_csv(donation_filers_df, file_out("data/donation_filers.csv")),
  donor_zips_df = extract_donor_zips(raw_data),
  donation_zips_csv = write_csv(donor_zips_df, file_out("data/donor_zips.csv")),
)

load_plan <- drake_plan(
  loaded_schema = load_schema(con),
  loaded_taxonomy = target(command = load_industry_taxonomy(con, here(file_in("data/industry_taxonomy.csv"))),
                           trigger = trigger(condition = TRUE)),
  loaded_donors = load_donors(con, here(file_in("data/donors.csv"))),
  loaded_donations = load_donations(con, here(file_in("data/donations.csv"))),
  loaded_filers = load_filers(con, here(file_in("data/filers.csv"))),
  loaded_employers = load_employers(con, here(file_in("data/employers.csv"))),
  loaded_employment = load_employment(con, here(file_in("data/employment.csv"))),
  loaded_donation_filers= load_donation_filers(con, here(file_in("data/donation_filers.csv"))),
  loaded_donation_donors= load_donation_donors(con, here(file_in("data/donation_donors.csv"))),
  loaded_donor_zips = load_donor_zips(con, here(file_in("data/donor_zips.csv"))),
)

correct_plan <- drake_plan(
  employer_pattern_df = read_csv(here(file_in("data/employer_replacements.csv"))),
  employer_pattern_replacements_df = simplify_companies(raw_data, employer_pattern_df),
  employer_pattern_replacements_csv = write_csv(employer_pattern_replacements_df, file_out("data/employer_pattern_replacements.csv")),
  loaded_employer_pattern_replacements = merge_companies(con, here(file_in("data/employer_pattern_replacements.csv")))
)

plan <- bind_rows(raw_data_plan, extract_entities_plan, extract_relationships_plan, load_plan, correct_plan)
