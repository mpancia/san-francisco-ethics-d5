# Functions to do data matching

#' A function to automate merging company names that represent the same company,
#' putting them in standard form.
simplify_companies <- function(raw_data, employer_pattern_df) {
  output_df <-
    raw_data %>%
    transmute(original_name = str_to_upper(tran_emp)) %>%
    distinct() %>%
    mutate(
      new_name = str_remove_all(original_name, ","), # remove commas
      new_name = str_remove_all(new_name, "LLC"), # remove LLC
      new_name = str_remove_all(new_name, "LLP"), # remove LLP
      new_name = str_remove_all(new_name, "INC"), # remove INC
      new_name = str_replace_all(new_name, "&", "AND"), # replace amperands
      new_name = str_replace_all(new_name, "\\s+", " "), # replace extra whitespace
      new_name = str_remove_all(new_name, "\\(.*\\)"), # remove stuff in parens
      new_name = str_remove_all(new_name, " \\.$"), # remove periods at the end
      new_name = str_trim(new_name)
    )
  replace_pattern <- function(pattern, replacement, ...) {
    output_df <<- output_df %>% mutate(
      new_name = str_replace_all(new_name, pattern, replacement)
    )
    return(NA)
  }
  effects <- pmap(employer_pattern_df, replace_pattern)
  output_df %<>% mutate(
    new_name = str_replace_all(new_name, "\\s+", " "),
    new_name = str_remove_all(new_name, "\\(.*\\)"),
    new_name = str_remove_all(new_name, " \\.$"),
  ) %>%
    distinct() %>%
    dplyr::filter(new_name != original_name)
}


merge_companies <- function(con, file_location) {
  merge_on_load <- "
  MATCH (orig: Employer {name: row.original_name})
  MERGE (new: Employer {name: row.new_name})
  WITH orig, new, new.name AS new_name, row
    call apoc.refactor.mergeNodes([new, orig], {properties: 'discard', mergeRels:true}) YIELD node
    SET node.name = row.new_name
    SET node.aliases = [row.new_name]
  RETURN node
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = merge_on_load
  )
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = "
           MATCH (new: Employer)
           WHERE row.new_name = new.name
           SET new.aliases = new.aliases + [row.original_name]
           RETURN new
           "
  )
}


#' A function to automate merging occupation names that represent the same occupation,
#' putting them in standard form.
simplify_occupations <- function(raw_data, occupation_pattern_df) {
  output_df <-
    raw_data %>%
    transmute(original_name = str_to_upper(tran_occ)) %>%
    distinct() %>%
    mutate(
      new_name = str_remove_all(original_name, ","), # remove commas
      new_name = str_replace_all(new_name, "\\s+", " "), # replace extra whitespace
      new_name = str_remove_all(new_name, "\\(.*\\)"), # remove stuff in parens
      new_name = str_remove_all(new_name, " \\.$"), # remove periods at the end
      new_name = str_trim(new_name)
    )
  replace_pattern <- function(pattern, replacement, ...) {
    output_df <<- output_df %>% mutate(
      new_name = str_replace_all(new_name, pattern, replacement)
    )
    return(NA)
  }
  effects <- pmap(occupation_pattern_df, replace_pattern)
  output_df %<>% mutate(
    new_name = str_replace_all(new_name, "\\s+", " "),
    new_name = str_remove_all(new_name, "\\(.*\\)"),
    new_name = str_remove_all(new_name, " \\.$"),
  ) %>%
    distinct() %>%
    dplyr::filter(new_name != original_name)
}

merge_occupations <- function(con, file_location) {
  merge_on_load <- "
  MATCH (orig: Occupation {name: row.original_name})
  MERGE (new: Occupation {name: row.new_name})
  WITH orig, new, new.name AS new_name, row
    call apoc.refactor.mergeNodes([new, orig], {properties: 'discard', mergeRels:true}) YIELD node
    SET node.name = row.new_name
    SET node.aliases = [row.new_name]
  RETURN node
  "
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = merge_on_load
  )
  load_csv(
    url = paste0("file:///", file_location),
    con = con,
    header = TRUE,
    as = "row",
    on_load = "
           MATCH (new: Occupation)
           WHERE row.new_name = new.name
           SET new.aliases = new.aliases + [row.original_name]
           RETURN new
           "
  )
}
