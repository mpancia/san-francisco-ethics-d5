#' A function to automate labeling companies with industries
label_companies <- function(industry_pattern_df, corrected_employer_names) {
  output_df <- corrected_employer_names %>%
    mutate(industry_name = NA,
           label_info = as.character(NA))
  label_company <- function(employer_name_pattern, industry_value, info, ...){
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

