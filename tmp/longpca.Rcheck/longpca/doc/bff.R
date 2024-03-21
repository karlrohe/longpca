## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
#>r = getOption("repos")
#>r["CRAN"] = "http://cran.us.r-project.org"
#>options(repos = r)

## ----setup--------------------------------------------------------------------
library(tidyverse)
library(longpca)
# This data on CRAN packages is pre-loaded in longpca. It was downloaded in February 2024.  
all_packages

# You can download the most recent version in this fashion:
# all_packages = available.packages() |> as_tibble()
# if you download fresh data, the specific interpretations below are likely to not be sensible.  

all_packages |> select(Package, Imports)

## -----------------------------------------------------------------------------
# in make_interaction_model, the ... arguments go to tidytext::unnest_tokens for the text parsing.  In this case, we do not want to make the words lower case (because package names are case sensitive and that might be important later)
idg = make_interaction_model(all_packages,
                             ~Package*Imports,
                             parse_text = TRUE, 
                             to_lower = FALSE)
idg

## -----------------------------------------------------------------------------
diagnose(idg)

## -----------------------------------------------------------------------------
set.seed(1)
# eicv = pick_dim(idg, dimMax = 100)
# plot(eicv)
# eicv$estimated_dimension

## -----------------------------------------------------------------------------
pcs = pca(idg, k = 11)
pcs

## -----------------------------------------------------------------------------
streaks(pcs,mode = "rows",plot_columns = 1:11)
# streaks(pcs,mode = "columns",plot_columns = 1:11)

## -----------------------------------------------------------------------------
spcs = rotate(pcs)
streaks(spcs, mode = "rows", plot_columns = 1:11)
streaks(spcs, mode = "cols", plot_columns = 1:11)

## -----------------------------------------------------------------------------
top(spcs, this_dim = 1)

## -----------------------------------------------------------------------------
top(spcs, 9)

## -----------------------------------------------------------------------------
all_packages |> count(License) |> arrange(desc(n))

## -----------------------------------------------------------------------------
all_packages <- all_packages %>%
  mutate(
    GPL = ifelse(str_detect(License, "(GPL|GNU General Public License)"), TRUE, FALSE),
    MIT = ifelse(str_detect(License, "MIT"), TRUE, FALSE),
    CC = ifelse(str_detect(License, "CC BY"), TRUE, FALSE),
    Apache = ifelse(str_detect(License, "Apache"), TRUE, FALSE),
    GNU = ifelse(str_detect(License, "GNU"), TRUE, FALSE),
    # Assuming 'Other' should be TRUE if none of the above conditions are met
    Other = ifelse(!(GPL | MIT | CC | Apache | GNU), TRUE, FALSE)
  )

## -----------------------------------------------------------------------------
data_selected = all_packages |> 
  left_join(spcs$column_features |>
              rename(Package = token) , by = "Package") |>
  left_join(spcs$row_features, by = "Package") |> 
  select(response = MIT, contains("vpc_"))

logistic_model <- glm(response ~ .-1, data = data_selected, family = binomial())
broom::tidy(logistic_model)


## -----------------------------------------------------------------------------

# # bff should take two objects: (pcs, im)
# #  the rows of im should align with the rows or columns of pcs.
# text_im = make_interaction_model(~Imports * (Title & Description & License & Author),
#                                  tib = top_packages |> mutate(Imports = Package),
#                                  parse_text = TRUE)
# text_im2 = make_interaction_model(~Package * (Title & Description & License & Author),
#                                   tib = top_packages,
#                                   parse_text = TRUE)
# bb = bff(spcs, text_im) # defined below.
# bbb = bff(spcs, text_im2)
# View(bb)
# View(bbb)


