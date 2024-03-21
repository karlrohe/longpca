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
parse_formula <- function(fo, tib) {
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
    tib = tib |> dplyr::mutate("outcome_unweighted_1" = 1)
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

