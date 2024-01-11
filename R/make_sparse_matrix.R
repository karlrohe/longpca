#' extract_interaction (internal)
#'
#' @param str
#'
#' @return a vector of terms inside the interaction
#'
#' @examples
#'
extract_interaction <- function(str) {

  pattern <- "\\(([^)]+)\\)|<([^>]+)>|\\b([a-zA-Z0-9_]+)\\b"
  #   This line defines a regular expression pattern to match specific parts of the input string.
  #   The pattern is composed of three parts, separated by the | (OR) operator, meaning it will match any one of these three patterns:
  #     \\(([^)]+)\\): Matches anything inside parentheses.
  # \\( and \\): Escaped parentheses to match literal parentheses in the string.
  # ([^)]+): A capturing group that matches any character except a closing parenthesis ()) one or more times (+).
  # <([^>]+)>: Matches anything inside angle brackets (< and >).
  # < and >: Matches literal angle brackets.
  # ([^>]+): A capturing group that matches any character except a closing angle bracket (>) one or more times.
  # \\b([a-zA-Z0-9_]+)\\b: Matches a standalone variable name.
  # \\b: Word boundary, ensuring we match whole words only.
  # [a-zA-Z0-9_]+: A capturing group that matches one or more alphanumeric characters or underscores.

  matches <- regmatches(str, gregexpr(pattern, str))
  # gregexpr(pattern, str): This function searches for all occurrences of the pattern in the input string str. It returns a list of match positions.
  # regmatches(str, ...): This function extracts the actual matched strings based on the positions found by gregexpr. The ... represents the list of match positions.
  return(matches[[1]])
}

#' extract_variables (internal)
#'
#' @param str
#'
#' @return
#'
#' @examples
extract_variables <- function(str) {
  # Remove parentheses if present
  str <- gsub("[()]", "", str)

  # Split the string by '|' or '*' and trim whitespace
  variables <- strsplit(str, "\\s*&\\s*")

  return(variables)
}


#' parse_variables (internal)
#'
#' @param fo
#' @param tib
#'
#' @return
#'
#' @examples
#' @importFrom magrittr %>%
parse_variables <- function(fo, tib) {
  # this makes a list of length three. each element is a character vector of variable names.
  #      first element is outcome variable.  second element is row variables.  third element is column variables.

  # fo should be like this:
  # fo = val~(var1&var2) * (var4&var5 )
  # or
  # fo = val~ v1 * var2
  # tib should be a tibble the contains all the variable names.

  # first, a simple check to make sure fo is a formula  check to make sure for is of the right form...

  if(!rlang::is_formula(fo)){
    stop("first argument must be a formula; outcome ~ user_id * product_id")
  }

  # "left side" is the outcome...

  left_var <- all.vars(fo[[2]])
  # if it is an unweighted graph, then should
  #  should have 1 on left hand side... 1 ~ userid*productid
  #  this makes all.vars(fo[[2]]) as character(0)
  #  also, we need to make a column in tib for it...
  if(length(left_var)==0){
    left_var="1"
    if("outcome_unweighted_1" %in% colnames(tib)){
      stop("outcome_unweighted_1 is 'reserved' column name for parse_variables with '1~' formulas.
            this code is removing your column outcome_unweighted_1 and defining it as all 1s. this might cause an error later.")
    }
    tib = tib %>% dplyr::mutate("outcome_unweighted_1" = 1)
    left_var = "outcome_unweighted_1"
  }

  # right side are the variables that define the features.


  right_expr <- fo[3]
  rhs = deparse(right_expr[[1]])
  rhs_terms = extract_interaction(rhs)
  if(length(rhs_terms)!=2){
    stop("Your input formula \n\n    ",
         deparse(fo),
         "\n\nhas been parsed to have these terms on the right hand side:\n\n    ",
         paste0(rhs_terms,sep="\n    "),
         "\nCurrently, this function only handles one interaction term on the right hand side of the formula.
for example, these are valid:

    y ~ v1*v3
    y ~ (v1 & v2) * v3
    y ~ (v1 & v2) * (v3 & v4 & v5)
    y ~ v1 * (v3 & v4).

However, these are not yet accepted:

    y ~ v1*v3 + v4
    y ~ v1*v3 + v2*v4
    y ~ v1*v2*v3")
  }
  right_vars = extract_variables(rhs_terms)


  if(left_var=="1") tib= mutate(tib, "")

  # Combine left and right variables
  vars <- c(left_var, right_vars)

  # Check if the variables exist in the tibble

  missing_vars <- setdiff(unlist(vars), names(tib))
  if (length(missing_vars) > 0) {
    stop("The following variables are not in the tibble: ", paste(missing_vars, collapse = ", "))
  }

  return(vars)

}



#' make_edge_tib
#'
#' @param fo a formula, like outcome ~ (row_ids & context) * measurement_type.
#' @param tib a tibble that contains the variables in the formula. The only exception is that the left-hand-side can be 1 and this does not need to be in tib.
#'
#' @return a list that contains the edge-list tibble, the row_index, and column_index.
#' @export
#'
#' @examples
make_edge_tib = function(fo, tib){
  vars = parse_variables(fo, tib)
  # Select the columns from the tibble


  outcome_column = vars[[1]]
  row_column = vars[[2]]
  column_column = vars[[3]]

  get_edge_tib_from_variables(tib, row_column, column_column, outcome_column)

}



#' make_sparse_text_matrix_raw
#'
#' @param fo
#' @param tib
#' @param dropNA
#' @param ...
#'
#' @return
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select distinct row_number left_join
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#' @importFrom tidytext unnest_tokens
#' @export
#' @examples
make_sparse_text_matrix_raw = function(fo, tib, dropNA = TRUE, ...){

  fo = update_lhs_to_1(fo)
  vars=parse_variables(fo, tib)
  outcome_column = vars[[1]]
  row_column = vars[[2]]
  column_column = vars[[3]]

  edge_list_text = tib %>%
    dplyr::select(all_of(row_column), text = all_of(column_column)) %>%
    unnest_tokens(word, text,...) %>%
    # tidytext::unnest_tokens(word, text) %>%
    mutate(outcome_unweighted_1 = 1)


  edge_tib_list = get_edge_tib_from_variables(edge_list_text, row_column, "word", outcome_column)


  edge_tib = edge_tib_list$edge_tib
  if(dropNA) edge_tib = edge_tib %>% drop_na()
  row_universe = edge_tib_list$row_index
  column_universe = edge_tib_list$col_index
  A = Matrix::sparseMatrix(
    i = edge_tib$row_id,
    j = edge_tib$col_id,
    x = edge_tib$outcome,
    dims = c(nrow(row_universe), nrow(column_universe)))


  sp_A_dat = list(A=A,
                  row_universe=row_universe,
                  column_universe = column_universe)
  return(sp_A_dat)

}



#' get_edge_tib_from_variables (internal to make_edge_tib and make_sparse_text_matrix_raw)
#'
#' @param tib
#' @param row_column
#' @param column_column
#' @param outcome_column
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select distinct row_number left_join
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#'
#' @examples
get_edge_tib_from_variables = function(tib, row_column, column_column, outcome_column, vars=NULL){
  if(is.null(vars)){ vars = list(outcome_column, row_column, column_column)}
  row_index = tib %>%
    select(all_of(row_column)) %>%
    drop_na() %>%
    distinct() %>%
    mutate(row_id = row_number())
  col_index = tib %>%
    select(all_of(column_column)) %>%
    drop_na() %>%
    distinct() %>%
    mutate(col_id = row_number())

  # if a weighted graph...
  if(outcome_column != "outcome_unweighted_1"){
    edge_list_tib = tib %>% select(all_of(unlist(vars)))

    edge_tib = edge_list_tib %>%
      left_join(row_index, by = row_column) %>%
      left_join(col_index, by = column_column) %>%
      mutate(outcome= .data[[outcome_column]])
  }else{
    # if an un-weighted graph...
    edge_list_tib = tib %>% select(all_of(unlist(vars[-1])))

    edge_tib = edge_list_tib %>%
      left_join(row_index, by = row_column) %>%
      left_join(col_index, by = column_column) %>%
      mutate(outcome= 1)
  }
  list(edge_tib = edge_tib %>% drop_na(),
       row_index = row_index,
       col_index = col_index)
}


#' make_sparse_matrix_raw
#'
#' @param fo a formula, like outcome ~ (row_ids & context) * measurement_type.
#' @param tib a tibble that contains the variables in the formula. The only exception is that the left-hand-side can be 1 and this does not need to be in tib.
#' @param dropNA recommended.  This drops rows of tib if there are any NA's among the essential variables.
#'
#' @return a list with three elements.  First, the sparse Matrix A. Second, row_universe which is akin to the row names of A, but in a tidy form.  Thir, column_universe which is like row_universe.
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
make_sparse_matrix_raw = function(fo, tib, dropNA = TRUE){
  # This returns a list with elements
  #  A: sparse matrix for formula fo on tibble tib.
  #  row_universe: a tibble of distinct row-variable values, with the row_id (i.e. corresponding row number in A)
  #  col_universe: a tibble of distinct column-variable values, with the col_id (i.e. corresponding column number in A)

  vars = parse_variables(fo, tib)
  outcome_column = vars[[1]]
  row_column = vars[[2]]
  column_column = vars[[3]]

  edge_tib_list = make_edge_tib(fo,tib)

  edge_tib = edge_tib_list$edge_tib
  if(dropNA) edge_tib = edge_tib %>% tidyr::drop_na()
  row_universe = edge_tib_list$row_index
  column_universe = edge_tib_list$col_index
  A = Matrix::sparseMatrix(
    i = edge_tib$row_id,
    j = edge_tib$col_id,
    x = edge_tib$outcome,
    dims = c(nrow(row_universe), nrow(column_universe)))


  sp_A_dat = list(A=A,
                  row_universe=row_universe,
                  column_universe = column_universe)
  return(sp_A_dat)
}

#' make_incomplete_matrix_raw
#'
#' This is like make_sparse_matrix_raw, but for softImpute.
#'
#' @param fo
#' @param tib
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize left_join mutate select distinct
make_incomplete_matrix_raw = function(fo, tib){

  vars = parse_variables(fo, tib)
  outcome_column = vars[[1]]
  row_column = vars[[2]]
  column_column = vars[[3]]

  edge_tib_list = make_edge_tib(fo,tib)

  edge_tib = edge_tib_list$edge_tib
  mean_outcome = edge_tib %>%
    group_by(row_id, col_id) %>%
    summarize(outcome = mean(outcome, na.rm=T)) %>% ungroup

  column_means = mean_outcome %>%
    group_by(col_id) %>%
    summarize(col_mean = mean(outcome, na.rm=T)) %>% ungroup

  edge_tib2 = mean_outcome %>%
    left_join(column_means) %>%
    mutate(outcome = outcome - col_mean) %>%
    select(-col_mean) %>%
    left_join(
      edge_tib %>%
        select(-outcome) %>%
        select(all_of(c(row_column, column_column)),
               row_id, col_id) %>%
        distinct()
    )

  row_universe = edge_tib_list$row_index
  column_universe = edge_tib_list$col_index %>%
    left_join(column_means) %>%
    tidyr::drop_na()

  Imat = softImpute::Incomplete(i = edge_tib$row_id,
                                j = edge_tib$col_id,
                                x = edge_tib$outcome)

  incomplete_A_dat = list(A=Imat,
                          row_universe=row_universe,
                          column_universe = column_universe)
  return(incomplete_A_dat)
}
