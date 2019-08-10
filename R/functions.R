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
    # Convert the column names to symbols
    c(
      pattern_column,
      pattern_label_column,
      match_column,
      match_label_column
    ) %<-% map(list(
      pattern_column_name,
      pattern_label_column_name,
      match_column_name,
      match_label_column_name
    ), sym)

    apply_row_label <- function(current_state, row) {
      # Evaluate the pattern and label for the current row, converting the label to a character if necessary
      c(pattern, label) %<-% map(
        list(pattern_column, pattern_label_column),
        function(x) as.character(eval(x, envir = row))
      )
      # Make an expression that matches on the label and replaces it
      mutate_expr <- quo(ifelse(
        str_detect(!!match_column, !!pattern), !!label,
        !!match_label_column
      ))
      # Apply the expression to create a new column
      current_state %>%
        mutate(!!match_label_column := !!mutate_expr)
    }
    # Convert the dataframe to a list of rows, then reduce, starting with the original input match dataframe.
    reduce(pmap(pattern_dataframe, list), apply_row_label, .init = match_dataframe)
  }
