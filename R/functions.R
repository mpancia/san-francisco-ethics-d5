#' Apply labels to a dataframe using a dataframe of patterns/labels.
#'
#' @param pattern_dataframe An input dataframe containing patterns
#' @param pattern_column_name The name of the column in the pattern dataframe containing the patterns
#' @param pattern_label_column_name The name of the column in the pattern dataframe containing the labels
#' @param match_dataframe An input dataframe containing a character column to pattern match on
#' @param match_column_name The name of the column to match the patterns on
#' @param match_label_column_name The name of the resulting label column, to be populated on a successful match
#'
#' @return A dataframe that looks like \code{match dataframe}, but with \code{match_label_column_name} filled in with labels on a successful match.
#' @export
#'
#' @examples
#' pattern_df <- tribble(
#'   ~pattern, ~label,
#'   ".*cat.*", "A_CAT",
#'   ".*dog.*", "A_DOG"
#' )
#'
#' match_df <- tribble(
#'   ~pet_description, ~pet_label,
#'   "this is a cat", NA,
#'   "this is a dog", NA,
#'   "this is a wombat", NA
#' )
#'
#' apply_labels_to_dataframe(pattern_df, "pattern", "label", match_df, "pet_description", "pet_label")
apply_labels_to_dataframe <-
  function(pattern_dataframe,
             pattern_column_name,
             pattern_label_column_name,
             match_dataframe,
             match_column_name,
             match_label_column_name) {
    pattern_column <- sym(pattern_column_name)
    pattern_label_column <- sym(pattern_label_column_name)
    match_column <- sym(match_column_name)
    match_label_column <- sym(match_label_column_name)

    apply_row_label <- function(current_state, row) {
      with(row, {
        pattern <- eval(pattern_column)
        label <- as.character(eval(pattern_label_column))
        mutate_expr <- quo(ifelse(
          str_detect(!!match_column, !!pattern), !!label,
          !!match_label_column
        ))
        current_state %>% mutate(!!match_label_column := !!mutate_expr)
      })
    }

    reduce(pmap(pattern_dataframe, list), apply_row_label, .init = match_dataframe)
  }
