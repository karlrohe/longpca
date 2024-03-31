#' @importFrom utils globalVariables
utils::globalVariables(c(".data", "formula"))




#' make_interaction_model
#'
#' This generates an `interaction_model` object.  If you are comfortable thinking about matrices, you can think of this as a matrix-like-object.
#'
#'
#'
#' @param .data a tibble that contains the variables in the formula. The only exception is that the left-hand-side can be 1 and this does not need to be in .data.
#' @param formula a formula, like outcome ~ row_id * (measurement_type & context).
#' @param duplicates
#' @param parse_text
#' @param dropNA
#' @param data_prefix
#' @param ... additional arguments passed to `tidytext::unnest_tokens`
#'
#' @return a list with four elements.  First, the interaction_tibble, akin to a sparse matrix in triplet form. Second, row_universe which is akin to the row names of A, but in a tidy form.  Thir, column_universe which is like row_universe. Fourth, some settings.
#' @export
#'
#' @examples
#' library(nycflights13)
#' im = make_interaction_model(flights,~(month & day)*dest)
#' names(im)
#' im$row_universe
#' im$column_universe
#' im$interaction_tibble
#' im$settings
#' # you can extract the sparse Matrix:
#' A = longpca:::get_Matrix(im,  import_names = TRUE)
#' str(A)
#' im = make_interaction_model(all_packages, ~Package*Imports, parse_text = TRUE)
#' names(im)
#' im$row_universe
#' im$column_universe
#' im$interaction_tibble
#' im$settings
#' # with text, there is often a great number of weakly connected words (words that appear once).
#' # you can remove these words that appear less than 10 times (and documents that have less than 10 words) via:
#' core(im, core_threshold = 10)
#' # core retains the k-core of the "largest connected component" in the bipartite graph between rows and columns.
#'
#' library(dplyr)
#' # You can provide more than two text columns to make_interaction_model:
#' im_text = make_interaction_model(top_packages, ~Package*(Title & Description), parse_text= TRUE)
#' im_text$column_universe |> arrange(desc(n))
#' # remove stop words by removing them from the column_universe,
#' #  then use the function subset_im to renumber the columns/rows and remove any lines from interaction_tibble
#' im_text$column_universe = im_text$column_universe |> anti_join(tidytext::stop_words, by = c("token"="word"))
#' im_text = im_text |> subset_im()
#' im_text$column_universe |> arrange(desc(n))
make_interaction_model <- function(.data, formula, duplicates = "add", parse_text = FALSE, dropNA = TRUE, data_prefix = NULL, ...) {
  if (parse_text & (duplicates != "add")) {
    duplicates <- "add"
    warning("duplicates set to `add` because this is text and that's what we do.")
  }

  allowed_duplicate_values <- c("add", "average", "none")
  if (!duplicates %in% allowed_duplicate_values) {
    stop("Invalid value for 'duplicates'. Choose either 'add' or 'average' or 'none'.")
  }

  dumb_formula <- ~1
  boolean_first_symbol_is_tilde <- formula[[1]] == dumb_formula[[1]]

  # If no left-hand side in formula, then make it a 1
  if (length(formula) == 2 & boolean_first_symbol_is_tilde) {
    formula <- stats::as.formula(paste("1", deparse(formula)))
  }

  vars <- parse_formula(formula, .data)
  outcome_column <- vars[[1]]
  row_column <- vars[[2]]
  column_column <- vars[[3]]

  if (parse_text) {
    .data <- .data |>
      dplyr::select(dplyr::all_of(row_column), tidyselect::all_of(column_column)) |>
      tidyr::pivot_longer(-dplyr::all_of(row_column), names_to = "from_text", values_to = "text") |>
      tidytext::unnest_tokens("token", text, ...) |>
      dplyr::mutate(outcome_unweighted_1 = 1)
    column_column <- c("from_text", "token")
    data_prefix <- "text"
  }

  im <- make_interaction_model_from_variables(
    tib = .data,
    row_column = row_column,
    column_column = column_column,
    outcome_column = outcome_column,
    vars = NULL,
    dropNA = dropNA,
    duplicates = duplicates
  )

  outcome_aggregation <- duplicates
  if (outcome_column == "outcome_unweighted_1") {
    outcome_aggregation <- "count"
  }

  im$settings <- list(
    fo = formula,
    data_prefix = data_prefix,
    outcome_aggregation = outcome_aggregation,
    outcome_variables = outcome_column,
    row_variables = row_column,
    column_variables = column_column
  )

  class(im) <- "interaction_model"

  return(im)
}




#' Custom completion function for make_interaction_model
#'
#' @param .data The tibble passed to make_interaction_model
#' @param formula The formula passed to make_interaction_model
#'
#' @keywords internal
#' @export
.make_interaction_model_completion <- function(.data, formula) {
  # Extract variable names from .data
  variables <- names(.data)

  # Create a completion list
  completion_list <- c(
    paste0(variables, " & "),
    paste0(variables, " * "),
    # paste0(variables, " = "),
    paste0(variables, " ~ "),
    # paste0(variables, " + "),
    # paste0(variables, " | "),


  )

  # Return the completion list
  return(completion_list)
}



#' print interaction model
#'
#' @param im
#'
#' @return
#' @export
#'
#' @examples
print.interaction_model = function(im){
  print(im[-c(1,4)])
}



#' summary interaction model
#'
#' @param im
#'
#' @return
#' @export
#'
#' @examples
summary.interaction_model = function(im){
  diagnose(im,make_plot = FALSE)
}


#' make_interaction_model_from_variables (internal to make_interaction_tibble and text2sparse)
#'
#' @param tib
#' @param row_column
#' @param column_column
#' @param outcome_column
#'
#' @return
#' @export
#' @importFrom dplyr mutate count arrange select distinct row_number left_join group_by ungroup summarize n
#' @importFrom stats sd
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#'
#' @examples
make_interaction_model_from_variables = function(tib, row_column, column_column, outcome_column, vars=NULL, dropNA, duplicates){
  if(is.null(vars)){ vars = list(outcome_column, row_column, column_column)}
  row_index = tib |>
    select(all_of(row_column)) |>
    drop_na() |>
    count(across(all_of(row_column))) |>
    arrange(desc(n)) |>
    mutate(row_num = row_number())
  col_index = tib |>
    select(all_of(column_column)) |>
    drop_na() |>
    count(across(all_of(column_column))) |>
    arrange(desc(n)) |>
    mutate(col_num = row_number())

  # if a weighted graph...
  if(outcome_column != "outcome_unweighted_1"){
    edge_list_tib = tib |> select(all_of(unlist(vars)))

    interaction_tibble = edge_list_tib |>
      left_join(row_index, by = row_column) |>
      left_join(col_index, by = column_column) |>
      mutate(outcome= .data[[outcome_column]]) |>
      select(row_num, col_num, outcome)
  }else{
    # if an un-weighted graph...
    edge_list_tib = tib |> select(all_of(unlist(vars[-1])))

    interaction_tibble = edge_list_tib |>
      left_join(row_index, by = row_column) |>
      left_join(col_index, by = column_column) |>
      mutate(outcome= 1) |>
      select(row_num, col_num, outcome)
  }
  if(dropNA) interaction_tibble = interaction_tibble |> tidyr::drop_na()
  if(duplicates == "none"){
    return(
      list(interaction_tibble = interaction_tibble,
           row_universe = row_index,
           column_universe = col_index)
    )
  }
  if(duplicates == "add"){
    interaction_tibble = interaction_tibble |>
      group_by(row_num,col_num) |>
      summarize(outcome = sum(outcome)) |>
      ungroup()
  }
  if(duplicates == "average"){
    interaction_tibble = interaction_tibble |>
      group_by(row_num,col_num) |>
      summarize(n = n(),
                sd = if_else(n>1, sd(outcome, na.rm=TRUE),0),
                min = min(outcome, na.rm=TRUE),
                max = max(outcome, na.rm=TRUE),
                outcome = mean(outcome, na.rm=TRUE)) |>
      ungroup()
  }

  list(interaction_tibble = interaction_tibble,
       row_universe = row_index,
       column_universe = col_index)
}












#' old_make_interaction_model
#'
#' This generates an `interaction_model` object.  If you are comfortable thinking about matrices, you can think of this as a matrix-like-object.
#'
#' @param fo a formula, like outcome ~ (row_nums & context) * measurement_type.
#' @param tib a tibble that contains the variables in the formula. The only exception is that the left-hand-side can be 1 and this does not need to be in tib.
#' @param dropNA recommended.  This drops rows of tib if there are any NA's among the essential variables.
#' @param parse_text if set to TRUE, then the right side of * (i.e. the measurement_type) will be parsed as a sequence of tokens via tidytext::unnest_tokens.  Additional arguments in ... will be passed to unnest_tokens.  For example, adding to_lower = FALSE will ensure case is kept. Additionally, you could set token to be something other than words (ngrams or skip_ngrams and then additionally specify n = ).  See unnest_token for more arguments.
#'
#' @return a list with four elements.  First, the interaction_tibble, akin to a sparse matrix in triplet form. Second, row_universe which is akin to the row names of A, but in a tidy form.  Thir, column_universe which is like row_universe. Fourth, some settings.
#' @export
#'
#' @examples
#' library(nycflights13)
#' im = old_make_interaction_model(~(month & day)*dest, flights)
#' names(im)
#' im$row_universe
#' im$column_universe
#' im$interaction_tibble
#' im$settings
#' # you can extract the sparse Matrix:
#' A = longpca:::get_Matrix(im,  import_names = TRUE)
#' str(A)
#' im = old_make_interaction_model(~Package*Imports, all_packages, parse_text = TRUE)
#' names(im)
#' im$row_universe
#' im$column_universe
#' im$interaction_tibble
#' im$settings
#' @importFrom magrittr %>%
old_make_interaction_model = function(fo, tib, duplicates = "add", parse_text= FALSE, dropNA = TRUE, data_prefix = NULL,...){
  # This returns a list with elements
  #  interaction_tibble: sparse matrix for formula fo on tibble tib.
  #  row_universe: a tibble of distinct row-variable values, with the row_num (i.e. corresponding row number in A)
  #  column_universe: a tibble of distinct column-variable values, with the col_num (i.e. corresponding column number in A)
  #  settings: a bunch of stuff.

  if(parse_text & (duplicates != "add")){
    duplicates = "add"
    warning("duplicates set to `add` because this is text and that's what we do.  ")
  }

  allowed_duplicate_values <- c("add", "average","none")
  if (!duplicates %in% allowed_duplicate_values) {
    stop("Invalid value for 'duplicates'. Choose either 'add' or 'average' or 'none'.")
  }


  dumb_formula = ~1
  boolean_first_symbol_is_tilde = fo[[1]] == dumb_formula[[1]]
  # if no left hand side in formula, then make it a 1
  if(length(fo)==2 & boolean_first_symbol_is_tilde){
    # fo = update_lhs_to_1(fo, quiet = FALSE)
    fo = stats::as.formula(paste("1", deparse(fo)))
  }


  vars = parse_formula(fo, tib)
  outcome_column = vars[[1]]
  row_column = vars[[2]]
  column_column = vars[[3]]

  # print(column_column)
  if(parse_text){

    tib = tib |>
      dplyr::select(all_of(row_column), tidyselect::all_of(column_column)) |>
      pivot_longer(-all_of(row_column), names_to = "from_text", values_to = "text") |>
      tidytext::unnest_tokens("token", text,...) |>
      dplyr::mutate(outcome_unweighted_1 = 1)
    column_column=c("from_text", "token")


    data_prefix= "text"
  }

  im =make_interaction_model_from_variables(tib=tib,
                                            row_column=row_column,
                                            column_column=column_column,
                                            outcome_column=outcome_column,
                                            vars=NULL,
                                            dropNA=dropNA,
                                            duplicates=duplicates)

  outcome_aggregation = duplicates
  if(outcome_column == "outcome_unweighted_1") outcome_aggregation = "count"

  im$settings = list(fo = fo,
                     data_prefix = data_prefix,
                     outcome_aggregation = outcome_aggregation,
                     outcome_variables = outcome_column,
                     row_variables = row_column,
                     column_variables = column_column)

  class(im) = "interaction_model"
  return(im)
}

