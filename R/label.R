#' A function to automate labeling companies with industries
label_companies <-
  function(industry_pattern_df,
             corrected_employer_names) {
    output_df <- corrected_employer_names %>%
      mutate(
        industry_name = NA,
        label_info = as.character(NA)
      ) %>%
      apply_labels_to_dataframe(
        industry_pattern_df,
        "employer_name_pattern",
        "industry_value",
        .,
        "employer_name",
        "industry_name"
      ) %>%
      apply_labels_to_dataframe(
        industry_pattern_df,
        "employer_name_pattern",
        "info",
        .,
        "employer_name",
        "label_info"
      ) %>%
      dplyr::filter(!is.na(industry_name)) %>%
      mutate(industry_name = factor(
        industry_name,
        levels = levels(industry_pattern_df$industry_value)
      ))
  }

load_company_labels <- function(con, file_location) {
  merge_on_load <- "
  MATCH (employer: Employer {name: row.employer_name})
  MATCH (industry: Industry {name: row.industry_name})
  MERGE (employer)-[:IS_MEMBER_OF {info: row.label_info}]->(industry)-[:HAS_MEMBER {info: row.label_info}]->(employer)
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = merge_on_load
  )
}

get_unlabeled_companies <- function(con, corrected_employer_names, industry_pattern_replacements) {
  unlabeled_names <- corrected_employer_names %>%
    dplyr::filter(!employer_name %in% industry_pattern_replacements$employer_name)
  escaped_names <- unlabeled_names$employer_name %>% str_replace_all("'", "\\\\'")
  name_list <- paste0("[", paste0(paste0("\"", escaped_names, "\""), collapse = ","), "]")
  query <- glue("
  MATCH (e: Employer)-[]->(d: Donor)-[:MADE_DONATION]->(don)
  WHERE e.name IN {name_list}
  RETURN e.name, count(d) as total_employees, sum(don.amount) as total_amount
  ORDER BY total_amount DESC
  ")
  neo4r::call_neo4j(query = query, con = con, type = "row") %>%
    data.frame() %>%
    transmute(employer_name = value, total_employees = value.1, total_amount = value.2)
}

#' A function to automate labeling occupations with industries
label_occupations <-
  function(industry_pattern_df,
             corrected_occupation_names) {
    output_df <- corrected_occupation_names %>%
      mutate(
        industry_name = NA,
        label_info = as.character(NA)
      ) %>%
      apply_labels_to_dataframe(
        industry_pattern_df,
        "occupation_name_pattern",
        "industry_value",
        .,
        "occupation_name",
        "industry_name"
      ) %>%
      apply_labels_to_dataframe(
        industry_pattern_df,
        "occupation_name_pattern",
        "info",
        .,
        "occupation_name",
        "label_info"
      ) %>%
      dplyr::filter(!is.na(industry_name)) %>%
      mutate(industry_name = factor(
        industry_name,
        levels = levels(industry_pattern_df$industry_value)
      ))
  }

get_unlabeled_occupations <- function(con) {
  query <- "
  MATCH (donation: Donation)-[:MADE_BY]->(d:Donor)-[:WORKED_AS]->(o:Occupation), (d)-[:WORKED_AT]->(e: Employer)
  WHERE NOT (e)-[:IS_MEMBER_OF]->()
  AND NOT (o)-[:IS_MEMBER_OF]->()
  RETURN count(d) as total_donors, o.name as occupation_name
  ORDER BY total_donors DESC"
  neo4r::call_neo4j(query = query, con = con, type = "row") %>%
    as.data.frame() %>%
    set_names(c("total_donors", "occupation_name"))
}

load_occupation_labels <- function(con, file_location) {
  merge_on_load <- "
  MATCH (occupation: Occupation {name: row.occupation_name})
  MATCH (industry: Industry {name: row.industry_name})
  MERGE (occupation)-[:IS_MEMBER_OF {info: row.label_info}]->(industry)-[:HAS_MEMBER {info: row.label_info}]->(occupation)
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = merge_on_load
  )
}
