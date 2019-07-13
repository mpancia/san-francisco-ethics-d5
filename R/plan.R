# This gets the raw data from the SF OpenData Socrata portal for:
#   Campaign contributions (form 460): https://data.sfgov.org/City-Management-and-Ethics/Campaign-Finance-FPPC-Form-460-Schedule-A-Monetary/q66q-d2tr
#   Data key: https://data.sfgov.org/City-Management-and-Ethics/Campaign-Finance-Data-Key/wygs-cc76
# The contributions are filtered to contain only individual donors from 2018 onwards for supervisor races.
# We also write the outputs to CSVs for archival.

raw_data_plan <- drake_plan(
  data_key = RSocrata::read.socrata("https://data.sfgov.org/resource/wygs-cc76.json"),
  data_key_csv = write_csv(data_key, file_out("data/data_key.csv")),
  raw_data = RSocrata::read.socrata(paste("https://data.sfgov.org/resource/q66q-d2tr.json?$where=",
                                          "date_extract_y(Thru_date) >= 2018 AND",
                                          "Entity_Cd = 'IND' AND",
                                          "contains(lower(Filer_NamL), 'supervisor')")),
  raw_data_csv = write_csv(raw_data, file_out("data/raw_data.csv"))
)

plan <- bind_rows(raw_data_plan)
