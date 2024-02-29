## install.packages("devtools")
## devtools::install_github("karlrohe/longpca")


library(tidyverse)
library(longpca)
library(nycflights13)

im = make_interaction_model(formula, flights)
im

# inspect "degree distributions" with this funciton:
#  recall that im is the interaction_model defined above.
diagnose(im)

cv_eigs = pick_dim(im, dimMax = 10,num_bootstraps = 5)
plot(cv_eigs)
cv_eigs

pcs = pca(im, k = 6)


pcs$row_features %>% sample_n(size = 10)
pcs$column_features %>% sample_n(size = 10)


pcs$row_features %>%
  mutate(date = make_date(day = day, month=month, year = 2013)) %>%
  select(date, contains("pc_")) %>%
  pivot_longer(contains("pc_"), names_to = "pc_dimension", values_to = "loadings") %>%
  ggplot(aes(x = date, y = loadings)) + geom_line() +
  facet_wrap(~pc_dimension, scales= "free") + geom_smooth()


## ---- cache=TRUE----------------------------------------------------------------
airports %>% sample_n(size = 3)

# first, get the lat and lon for the airports:
airport_dat = pcs$column_features %>%
  left_join(airports %>% select(dest=faa, lat,lon)) %>%
  select(lat, lon, contains("_col")) %>%
  pivot_longer(contains("pc_"),
               names_to = "pc_dimension", values_to = "loadings") %>%
  drop_na()


library(maps)
usa_map <- map_data("state")
p <- ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  coord_fixed(1.3, xlim = c(-125, -65), ylim = c(25, 50))
# i'm only keeping lower 48 states, dropping Anchorage and Honolulu.


p + geom_point(data = airport_dat, aes(x = lon, y = lat,
                                       size = abs(loadings), color = loadings)) +
  facet_wrap(~ pc_dimension)  +
  scale_color_gradient2(low = "red", high = "blue", mid = "white")


## ---- cache=TRUE----------------------------------------------------------------
library(nycflights13)
flights


## ---- cache=TRUE----------------------------------------------------------------
formula =  ~ (month & day)*(dest)


## ---- cache=TRUE----------------------------------------------------------------


## ---- cache=TRUE----------------------------------------------------------------

pcs = pca_count(formula, tib = flights, k = 6)
# In some settings, the verb "sum" is a more sensible than "count"... pca_sum is the identical function
# pcs = pca_sum(formula, tib = flights, k = 6)
pcs = pca(im, flights, k = 6)
spcs = rotate(pcs)

## -------------------------------------------------------------------------------
names(pcs)


## -------------------------------------------------------------------------------
sample_n(pcs$row_features, size = 3)
sample_n(pcs$column_features, size=3)


## ---- cache=TRUE----------------------------------------------------------------
plot(pcs)

