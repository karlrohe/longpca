## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dplyr)
library(longpca)

## -----------------------------------------------------------------------------
library(nycflights13)
formula =  ~ (month & day)*(dest)
im = make_interaction_model(flights, formula)
# pcs = pca(im, k = 6)

## -----------------------------------------------------------------------------
im$row_universe
im$column_universe

## -----------------------------------------------------------------------------
# you can get a fresh version here:
# all_packages = available.packages() %>% as_tibble()
all_packages |> select(Package,  Imports)

## -----------------------------------------------------------------------------
im_imports = make_interaction_model(all_packages, ~Package*Imports, parse_text= TRUE)
im_imports$row_universe
im_imports$column_universe

## -----------------------------------------------------------------------------
im_imports = make_interaction_model(all_packages, ~Package*Imports, parse_text= TRUE, to_lower = FALSE)
im_imports$column_universe

## -----------------------------------------------------------------------------
im_description = make_interaction_model(top_packages,~Package*Description,  parse_text = TRUE, to_lower= TRUE)
diagnose(im_description)
im_description$column_universe

## -----------------------------------------------------------------------------
im_imports_authors = make_interaction_model(top_packages, ~Package*(Imports&Author&Description&Title), parse_text = TRUE)
im_imports_authors$row_universe
im_imports_authors$column_universe


## -----------------------------------------------------------------------------
diagnose(im_imports_authors)

## -----------------------------------------------------------------------------
eigcv = pick_dim(im_imports_authors, num_bootstraps = 20)
plot(eigcv)
eigcv

## -----------------------------------------------------------------------------
pcs = pca(im_imports_authors, k = 10)
streaks(pcs)
streaks(pcs, "columns")

## -----------------------------------------------------------------------------
spcs = rotate(pcs)
streaks(spcs)
streaks(spcs, "columns")

## -----------------------------------------------------------------------------
top(spcs, 2,keep_how_many = 15)

