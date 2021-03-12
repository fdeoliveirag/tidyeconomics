create_date <- function(x, y) seq(from = my(x), to = my(y), by = "month")

# 2T 2001 / 4T 2001 (3)
# 1T 2003 / 2T 2003 (2)
# 4T 2008 / 1T 2009 (2)
# 2T 2014 / 4T 2016 (11)
# 1T 2020 / 2T 2020 (2)

recession_dates <- c(create_date("04/2001", "12/2001"),
                     create_date("01/2003", "06/2003"),
                     create_date("10/2008", "03/2009"),
                     create_date("04/2014", "12/2016"),
                     create_date("01/2020", "06/2020"))

recession_dates <- tibble(
  starting = my(c("04/2001", "01/2003", "10/2008", "04/2014", "01/2020")),
  ending = my(c("12/2001", "06/2003", "03/2009", "12/2016", "06/2020"))
)

monitor_fgv <- monitor_fgv %>%
  mutate(codace = ifelse(date %in% recession_dates, 1, 0))

ggplot() +
  geom_point(data = monitor_fgv, aes(date, gdp), color = "darkblue") +
  geom_line(data = monitor_fgv, aes(date, gdp)) +
  geom_rect(data = recession_dates,
            aes(xmin = starting, xmax = ending),
            ymin = -Inf, ymax = Inf,
            alpha = .4)

ggplot(filter(monitor_fgv, year(date) %in% c(2019, 2020)), aes(date, gdp)) + geom_point() + geom_line()

c("date", "gdp", "ipca", "selic", "ipca_target")

meta_ipca <- c(6, 4, 3.5,         # FHC II
               4, 5.5, 4.5, 4.5,  # Lula I
               rep(4.5, 4),       # Lula II
               rep(4.5, 4),       # Dilma I
               rep(4.5, 4),       # Dilma II
               4.25, 4)           # Bolsonaro

map(meta_ipca, ~ rep(.x, 12)) %>% flatten_dbl()

z <- function(x, yr = 1) rep(x, 12 * yr)

meta_ipca <- c(z(6), z(4), z(3.5), z(4), z(5.5),
               z(4.5, yr = 14), z(4.25), z(4))
