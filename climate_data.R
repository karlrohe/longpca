# low rank completion test case goes bonk. what's up with this'

library(longpca)

load(file = "~/data_peeks/embed2/month_dat.RData")

im = make_interaction_model(mean_val~(id&element)*(year&month),
                            month_dat,
                            duplicates = "average")
A = get_Incomplete_Matrix(im)
dim(A)
library(softImpute)
s = softImpute(A, rank.max = 5, type = "svd")
pcs = s_2_pc(im, s, "pc")
dat = pcs$column_features %>%
  arrange(year, as.numeric(month)) %>%
  filter(degree > 50) %>%
  mutate(date = lubridate::make_date(year, month))

dat %>%
  select(date, val = pc_1_columns) %>%
  ggplot(aes(x = date, y = val)) + geom_line()



# this looks better..
im_p = make_interaction_model(mean_val~(id)*(year&month),
                            month_dat %>% filter(element == "PRCP"),
                            duplicates = "average")
Ap = get_Incomplete_Matrix(im_p)
dim(Ap)
library(softImpute)
sp = softImpute(Ap, rank.max = 5, type = "svd")
pcs = s_2_pc(im_p, sp, "pc")
dat = pcs$column_features %>%
  arrange(year, as.numeric(month)) %>%
  filter(degree > 50) %>%
  mutate(date = lubridate::make_date(year, month))
dat
dat %>%
  select(date, val = pc_1_columns) %>%
  ggplot(aes(x = date, y = val)) + geom_line()



im_t = make_interaction_model(mean_val~(id)*(year&month),
                              month_dat %>% filter(element == "TMAX"),
                              duplicates = "average")
At = get_Incomplete_Matrix(im_t)
dim(At)

st = softImpute(At, rank.max = 5, type = "svd")
pcs = s_2_pc(im_t, st, "pc")
dat = pcs$column_features %>%
  arrange(year, as.numeric(month)) %>%
  filter(degree > 50) %>%
  mutate(date = lubridate::make_date(year, month))
dat
dat %>%
  select(date, val = pc_3_columns) %>%
  ggplot(aes(x = date, y = val)) + geom_line()
