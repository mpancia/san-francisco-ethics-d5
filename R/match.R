# Functions to do data matching

#' A function to automate merging company names that represent the same company,
#' putting them in standard form.
simplify_companies <- function(raw_data, employer_pattern_df) {
  output_df <-
    raw_data %>%
    transmute(original_name = str_to_upper(tran_emp)) %>%
    distinct() %>%
    mutate(new_name = str_remove_all(original_name, ","), # remove commas
           new_name = str_remove_all(new_name, "LLC"), # remove LLC
           new_name = str_remove_all(new_name, "LLP"), # remove LLP
           new_name = str_remove_all(new_name, "INC"), # remove INC
           new_name = str_replace_all(new_name, "&", "AND"), # replace amperands
           new_name = str_replace_all(new_name, "\\s+", " "), # replace extra whitespace
           new_name = str_remove_all(new_name, "\\(.*\\)"), # remove stuff in parens
           new_name = str_remove_all(new_name, " \\.$"), # remove periods at the end
           new_name = str_trim(new_name)
    ) %>%
    filter(new_name != original_name)
  replace_pattern <- function(pattern, replacement, ...){
    output_df <<- output_df %>% mutate(
      new_name = str_replace_all(new_name, pattern, replacement)
    )
    return(NA)
  }
  effects <- pmap(employer_pattern_df, replace_pattern)
  output_df %<>%
    dplyr::filter(new_name != original_name) %>% mutate(
           new_name = str_replace_all(new_name, "\\s+", " "),
           new_name = str_remove_all(new_name, "\\(.*\\)"),
           new_name = str_remove_all(new_name, " \\.$"),
    )
}

merge_companies <- function(con, file_location){

}
