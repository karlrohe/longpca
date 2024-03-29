% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{core}
\alias{core}
\title{core
Given an interaction_model, this will return another interaction_model that corresponds to the "k-core" of the input.  This function is recommended when \code{diagnose(im)} shows that the majority of rows/columns have 1, 2, or 3 connections.  In this case, the data is potentially too sparse for pca.  If you simply throwing away the rows/columns that are weakly connected, then you will reduce the connections of those that remain.  The k-core is what you get if you keep on iterating.  In particular, it will find the largest subset of rows and columns from the interaction_model such that every row and column has at least \code{core_threshold} number of connections or "data points" in interaction_tibble.  This is exactly the k-core if the row and columns correspond to unique elements (non-overlapping).  If the elements in the rows match some elements in the columns, then those elements are represented twice... once for the row and once for the column.  It is possible that only one of those is retained.}
\usage{
core(im_input, core_threshold)
}
\arguments{
\item{core_threshold}{}
}
\description{
core
Given an interaction_model, this will return another interaction_model that corresponds to the "k-core" of the input.  This function is recommended when \code{diagnose(im)} shows that the majority of rows/columns have 1, 2, or 3 connections.  In this case, the data is potentially too sparse for pca.  If you simply throwing away the rows/columns that are weakly connected, then you will reduce the connections of those that remain.  The k-core is what you get if you keep on iterating.  In particular, it will find the largest subset of rows and columns from the interaction_model such that every row and column has at least \code{core_threshold} number of connections or "data points" in interaction_tibble.  This is exactly the k-core if the row and columns correspond to unique elements (non-overlapping).  If the elements in the rows match some elements in the columns, then those elements are represented twice... once for the row and once for the column.  It is possible that only one of those is retained.
}
