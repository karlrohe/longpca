# install.packages("devtools")
# devtools::install_github("karlrohe/longpca")

library(longpca)

# install.packages("tidyverse")
library(tidyverse)
# install.packages("nycflights13")
library(nycflights13)

flights

im = make_interaction_model(flights, 1 ~ (month & day) * dest)
diagnose(im)
pcs = pca(im, 6)

# hard to interpret these...
top(pcs,1,abs_cut_off = 1)
top(pcs,2,abs_cut_off = 1)


# General life advice for interpreting pcs... plot them in their "native domain"
# we have time/days as the units... plot it against that!
# we have location as the context... plot it against that!

pcs$row_features %>%
  mutate(date = make_date(day = day, month=month, year = 2013)) %>%
  select(date, contains("pc_")) %>%
  pivot_longer(contains("pc_"), names_to = "pc_dimension", values_to = "loadings") %>%
  ggplot(aes(x = date, y = loadings)) + geom_line() +
  facet_wrap(~pc_dimension, scales= "free") + geom_smooth()



# first, get the lat and lon for the airports:
airport_dat = pcs$column_features %>%
  left_join(airports %>% select(dest=faa, lat,lon)) %>%
  select(lat, lon, contains("_col")) %>%
  pivot_longer(contains("pc_"),
               names_to = "pc_dimension", values_to = "loadings") %>%
  drop_na()
#> Joining with `by = join_by(dest)`


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
