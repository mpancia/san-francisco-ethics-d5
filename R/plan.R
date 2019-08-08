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
  data_key_csv = write_csv(data_key, here(file_out("data/data_key.csv"))),
  raw_data = RSocrata::read.socrata(paste("https://data.sfgov.org/resource/q66q-d2tr.json?$where=",
                                          "date_extract_y(Thru_date) >= 2018 AND",
                                          "Entity_Cd = 'IND' AND",
                                          "(Filer_Id = '1407918' OR",
                                          "Filer_Id = '1408942')")),
  raw_data_csv = write_csv(raw_data, here(file_out("data/raw_data.csv"))),
)

extract_entities_plan <- drake_plan(
  filers_df = extract_filers(raw_data),
  filers_csv = write_csv(filers_df, here(file_out("data/filers.csv"))),
  donors_df = extract_donors(raw_data),
  donors_csv = write_csv(donors_df, here(file_out("data/donors.csv"))),
  donations_df = extract_donations(raw_data),
  donations_csv = write_csv(donations_df, here(file_out("data/donations.csv"))),
  employers_df = extract_employers(raw_data),
  employers_csv = write_csv(employers_df, here(file_out("data/employers.csv"))),
)

extract_relationships_plan <- drake_plan(
  employment_df = extract_employment(raw_data),
  employment_csv = write_csv(employment_df, here(file_out("data/employment.csv"))),
  donation_donors_df = extract_donation_donors(raw_data),
  donation_donors_csv = write_csv(donation_donors_df, here(file_out("data/donation_donors.csv"))),
  donation_filers_df = extract_donation_filers(raw_data),
  donation_filers_csv = write_csv(donation_filers_df, here(file_out("data/donation_filers.csv"))),
  donor_zips_df = extract_donor_zips(raw_data),
  donation_zips_csv = write_csv(donor_zips_df, here(file_out("data/donor_zips.csv"))),
)

load_plan <- drake_plan(
  loaded_schema = load_schema(con),
  loaded_taxonomy = target(command = load_industry_taxonomy(con, here(file_in("data/industry_taxonomy.csv"))),
                           trigger = trigger(condition = TRUE)),
  loaded_donors = target(load_donors(con, here(file_in("data/donors.csv"))),
                         trigger = trigger(change = loaded_taxonomy)),
  loaded_donations = target(load_donations(con, here(file_in("data/donations.csv"))),
                            trigger = trigger(change = loaded_donors)),
  loaded_filers = target(load_filers(con, here(file_in("data/filers.csv"))),
                         trigger = trigger(change = loaded_schema)),
  loaded_employers = target(load_employers(con, here(file_in("data/employers.csv"))),
                            trigger = trigger(change = loaded_schema)),
  loaded_employment = target(load_employment(con, here(file_in("data/employment.csv"))),
                             trigger = trigger(change = c(loaded_employment, loaded_donors))),
  loaded_donation_filers = target(load_donation_filers(con, here(file_in("data/donation_filers.csv"))),
                                  trigger = trigger(change = c(loaded_donations, loaded_filers))),
  loaded_donation_donors = target(load_donation_donors(con, here(file_in("data/donation_donors.csv"))),
                                  trigger = trigger(change = c(loaded_donors, loaded_donations))),
  loaded_donor_zips = load_donor_zips(con, here(file_in("data/donor_zips.csv"))),
)

correct_plan <- drake_plan(
  employer_pattern_df = read_csv(here(file_in("data/employer_replacements.csv"))),
  employer_pattern_replacements_df = simplify_companies(raw_data, employer_pattern_df),
  employer_pattern_replacements_csv = write_csv(employer_pattern_replacements_df, here(file_out("data/employer_pattern_replacements.csv"))),
  loaded_employer_pattern_replacements = merge_companies(con, here(file_in("data/employer_pattern_replacements.csv"))),
  corrected_employer_names = target(neo4r::call_neo4j("MATCH (employer: Employer) RETURN DISTINCT employer.name", con = con, type = "row")$employer.name %>% transmute(employer_name = value), trigger = trigger(change=loaded_employer_pattern_replacements)),
  industry_taxonomy_df = read_csv(here(file_in("data/industry_taxonomy.csv"))),
  industry_pattern_df = read_csv(here(file_in("data/industry_mapping_patterns.csv")), col_types = list(col_character(), col_factor(industry_taxonomy_df$name), col_character())),
  industry_pattern_replacements = label_companies(industry_pattern_df, corrected_employer_names),
  industry_pattern_replacements_csv = write_csv(industry_pattern_replacements, here(file_out("data/industry_pattern_replacements.csv"))),
  unlabeled_companies_df = get_unlabeled_companies(con, corrected_employer_names, industry_pattern_replacements),
  unlabeled_companies_csv = write_csv(corrected_employer_names, here(file_out("data/unlabeled_companies.csv"))),
)

export_plan <- drake_plan(

)

plan <- bind_rows(raw_data_plan,
                  extract_entities_plan,
                  extract_relationships_plan,
                  load_plan,
                  correct_plan,
                  export_plan)