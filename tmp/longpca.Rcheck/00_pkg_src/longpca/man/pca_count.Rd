% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.R
\name{pca_count}
\alias{pca_count}
\title{pca_count}
\usage{
pca_count(fo, tib, k)
}
\arguments{
\item{fo}{a formula, like  ~ (row_ids & context) * measurement_type. The left hand side should be left empty.  If it isn't, it will be converted to 1. Either way, the elements of the sparse matrix will just be counts.}

\item{tib}{a tibble that contains the variables in the formula. The only exception is that the left-hand-side can be 1 and this does not need to be in tib.}

\item{k}{number of dimensions for the pca}
}
\value{
the output is a list of: (1) row_features for whichever term is first on the right hand side of the formula, (2) column_features for whichever term is second on the right hand side of the formula, (3) middle_B which is a tibble corresponding to a diagonal matrix in sparse triplet form, (4) settings which is a list of details.
}
\description{
this is the first user function to generate pcs.
}
