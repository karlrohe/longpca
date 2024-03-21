
#' make_leverage (internal for localization)
#'
#' @param pcs
#'
#' @return
#' @importFrom dplyr select bind_cols tibble
#' @importFrom tidyselect starts_with
#'
#' @examples
make_leverage = function(pcs){
  # types_of_modes = pcs |> distinct(type) |> pull(type)
  # this_type = types_of_modes[1]
  lev_list=list()

  row_pc_mat = pcs$row_features  |>
    select(starts_with(pcs$settings$prefix_for_dimensions)) |>
    as.matrix()

  row_leverage = rowSums(row_pc_mat^2)

  lev_list[[1]] = pcs$row_features  |>
    select(-starts_with(pcs$settings$prefix_for_dimensions)) |>
    bind_cols(tibble(leverage = row_leverage))

  col_pc_mat = pcs$column_features  |>
    select(starts_with(pcs$settings$prefix_for_dimensions)) |>
    as.matrix()

  col_leverage = rowSums(col_pc_mat^2)

  lev_list[[2]] = pcs$column_features  |>
    select(-starts_with(pcs$settings$prefix_for_dimensions)) |>
    bind_cols(tibble(leverage = col_leverage))


  names(lev_list) = names(pcs)[1:2]
  lev_list

}
#' make_deg_lev_resid (Internal)
#'
#' @param lev_tib
#'
#' @return
#' @importFrom dplyr select bind_cols tibble
#'
#' @examples
make_deg_lev_resid = function(lev_tib){
  fit = stats::lm(I(log(leverage+.00000001))~I(log(degree+1)),  data = lev_tib)
  bind_cols(lev_tib , tibble(residuals = fit$resid))
}
#' localization
#'
#' Plots degree vs leverage; More specifically, the residual of log(leverage)~log(degree) against log(degree).
#'
#' @importFrom dplyr select bind_cols tibble mutate bind_rows
#' @importFrom ggplot2 ggplot aes facet_wrap scale_x_log10 geom_point geom_smooth
#'
#' @param pcs
#'
#' @return
#' @export
#'
#' @examples
localization = function(pcs){
  # plot degree vs leverage...
  #   actually, the residual of log(leverage)~log(degree)
  #   against log(degree).


  leverages_data = make_leverage(pcs)


  # make mode labels
  my_row_names = pcs$settings$row_variables |> paste(collapse  = " & ")
  my_col_names = pcs$settings$column_variables |> paste(collapse  = " & ")


  row_dat = leverages_data[[1]] |>
    select(degree,leverage) |>
    mutate(mode = my_row_names) |>
    make_deg_lev_resid()

  col_dat = leverages_data[[2]] |>
    select(degree,leverage) |>
    mutate(mode = my_col_names) |>
    make_deg_lev_resid()

  deg_lev_resid = bind_rows(row_dat, col_dat)


  deg_lev_resid |>
    ggplot(aes(x=degree+1, y= residuals)) +
    geom_point()+
    # ggtitle(paste("   ", round(fit$coefficients[1],2),  " x deg ^", round(fit$coefficients[2],2)))+
    geom_smooth(se = F, method = "gam", data=subset(deg_lev_resid, degree >=2))+
    facet_wrap(~mode, scales = "free")+
    scale_x_log10()
}


# these next two functions help the function streaks
#' Internal
#'
#' @param u
#' @param n
#'
#' @return
#'
#' @examples
pair = function(u, n = 1000){
  if(nrow(u)>1000){
    lev = rowSums(u^2)
    samp = sample(nrow(u), n, prob = lev)
  }else{samp= 1:nrow(u)}
  graphics::par(bty="n")
  pairs(u[samp,], panel =my_panel)
  graphics::par(bty="o")
}
my_panel = function(x,y){

  graphics::points(x,y,pch ='.')
  graphics::abline(h=0, col='red'); graphics::abline(v=0, col='red')
  graphics::points(0,0, pch = "O", col = "red")

}
#' streaks
#'
#' pairs plot.  Set type_mode either "rows" or "cols".  plot_columns to pick which columns.
#'
#' @param pcs
#' @param mode
#' @param plot_columns
#'
#' @return
#' @export
#'
#' @importFrom dplyr select
#' @importFrom tidyselect starts_with
#'
#' @examples
streaks = function(pcs, mode = "rows",plot_columns= NULL){
  #  pairs plot
  #  type_mode plots the
  # make plot to look for radial streaks
  # if(is.null(type_mode)) type_mode = default_mode(pcs)
  #   if(!is_tibble(pc)){
  #     stop(
  #       "error: you need to give it a tibble.
  # this would happen if you gave it an pc object
  # Instead, you need to pick either $row_features or $column_features.
  # ")
  #   }
  if(mode %in% c("row","r","rows")){
    pc = pcs$row_features |> select(starts_with(pcs$settings$prefix_for_dimensions))
  }else{
    pc = pcs$column_features |> select(starts_with(pcs$settings$prefix_for_dimensions))
  }

  pc_mat = pc |>
    as.matrix()
  # scale(center=F)

  k = ncol(pc_mat)

  # identify which columns to plot if plot_columns is NULL
  if(is.null(plot_columns)){
    if(k<=10){plot_columns = 1:k}else{
      plot_columns = c(1:5, (k-4):k)}
  }
  pc_mat = pc_mat[,plot_columns]
  nn = nrow(pc_mat)
  if(nn>1000){
    levs = rowSums(pc_mat^2)
    samp = sample(nrow(pc_mat), size = 1000,prob = levs)
  }else{samp = 1:nn}

  pair(pc_mat[samp,])
}



#' plot.pc
#'
#' This creates five diagnostic plots:
#' 1) Screeplot: The top `k` singular values of `L`.
#' 2) Better screeplot: its singular values `2:k` (because the first one is usually dominant and difficult to see an elbow past it).
#' 3) A "localization plot" which is very similar (maybe exact?) to [this stuff](https://github.com/karlrohe/LocalizationDiagnostic); for each row (and column) compute its degree and its leverage score.  Take the log of both. Fit a linear model `log(leverage)~log(degree)` and plot the residuals against `log(degree)`.  If there is localization, I suspect that there will be a big curl on the right side.
#' 4) Pairs plot of `row_features`. This is the plot emphasized in the varimax paper. In these example plots below, we do not see very clear radial streaks.
#' 5) A pairs plot for `column_features`.  In both pairs plots, if there are more than 1000 points, then the code samples 1000 points with probability proportional to their leverage scores.  It will plot up to `k=10` dimensions.  If `k` is larger, then it plots the first 5 and the last 5.
#'
#' @param pcs
#'
#' @return
#' @export
#'
#' @examples
#' library(nycflights13)
#' pcs = pca_count(1 ~ (month & day)*(dest), flights, k = 6)
#' plot(pcs)
plot.pc = function(pcs){
  B = as.matrix(get_middle_matrix(pcs))
  k = nrow(B)
  graphics::par(mfrow = c(1,1))
  plot(diag(B), main = "Singular values of L (biased)")
  # , ylim = c(0, max(diag(B))))
  # cutoff1 = (sqrt(1/mean(pcs$row_features$degree)) + sqrt(1/mean(pcs$column_features$degree)))/2
  # cutoff2 = (sqrt(1/mean(pcs$row_features$weighted_degree)) + sqrt(1/mean(pcs$column_features$weighted_degree)))/2
  # lines(x = c(1,max(dim(B))), y = cutoff1*c(1,1))
  # lines(x = c(1,max(dim(B))), y = cutoff2*c(1,1))
  readline(prompt="Press [Enter] to continue to the next plot...")

  skip_first_singular_values = diag(B)[-1]
  plot(2:k, skip_first_singular_values, main = "2:k singular values of L (biased)")
  # , ylim = c(0, max(diag(B)[-1])))
  readline(prompt="Press [Enter] to continue to the next plot...")


  localization_plot = localization(pcs)
  print(localization_plot)

  readline(prompt="Press [Enter] to continue to the next plot...")

  streaks(pcs)
  readline(prompt="Press [Enter] to continue to the next plot...")

  streaks(pcs, "cols")
  # readline(prompt="Press [Enter] to continue to the next plot...")
  #
  # image(B, main = "middle B matrix")
}
