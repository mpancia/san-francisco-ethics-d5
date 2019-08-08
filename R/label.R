#' A function to automate labeling companies with industries
label_companies <- function(industry_pattern_df, corrected_employer_names) {
  output_df <- corrected_employer_names %>%
    mutate(
      industry_name = NA,
      label_info = as.character(NA)
    )
  label_company <- function(employer_name_pattern, industry_value, info, ...) {
    output_df <<- output_df %>% mutate(
      industry_name = ifelse(str_detect(employer_name, employer_name_pattern), as.character(industry_value), industry_name),
      label_info = ifelse(str_detect(employer_name, employer_name_pattern), info, label_info)
    )
    return(NA)
  }
  effects <- pmap(industry_pattern_df, label_company)
  output_df %<>%
    dplyr::filter(!is.na(industry_name)) %>%
    mutate(industry_name = factor(industry_name, levels = levels(industry_pattern_df$industry_value)))
}

get_unlabeled_companies <- function(con, corrected_employer_names, industry_pattern_replacements) {
  unlabeled_names <- corrected_employer_names %>%
    dplyr::filter(!employer_name %in% industry_pattern_replacements$employer_name)
  escaped_names <- unlabeled_names$employer_name %>% str_replace_all("'", "\\\\'")
  name_list <- paste0("[", paste0(paste0("\"", escaped_names, "\""), collapse = ","), "]")
  query <- glue("
  MATCH (e: Employer)-[]->(d: Donor)
  WHERE e.name IN {name_list}
  RETURN e.name, count(d) as total_employees
  ")
  neo4r::call_neo4j(query = query, con = con, type = "row") %>%
    data.frame() %>%
    transmute(employer_name = value, total_employees = value.1)
}
