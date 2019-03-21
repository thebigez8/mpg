library(tidyverse)
library(arsenal)

impute <- function(d)
{
  for(u in which(is.na(d$Mileage)))
  {
    d$Mileage[u] <- mean(d$Mileage[u + c(-1, 1)])
  }
  for(u in which(is.na(d$Date)))
  {
    d$Date[u] <- round(mean(d$Date[u + c(-1, 1)]))
  }
  d
}

partial.tanks <- function(d)
{
  for(u in which(d$mpg > 100))
  {
    d$mpg[u + 0:1] <- mean(d$mpg[u + 0:1])
    d$smoothed[u + 0:1] <- TRUE
  }
  d
}

dat <- "mpg.csv" %>%
  read_csv(col_types = cols(), col_names = TRUE, na = "?") %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Notes = replace(Notes, is.na(Notes), ""),
    i = 1:n()
  ) %>%
  impute() %>%
  arrange(is.na(Gallons) & (Notes %nin% c("Bought", "unk")), Date) %>%
  mutate(
    miles = if_else(is.na(Gallons), NA_real_, c(NA_real_, diff(Mileage))),
    mpg = miles / Gallons,
    ppg = Price / Gallons,
    Break = if_else(Date >= as.Date("2017-01-08"), "After", "Before"),
    smoothed = FALSE
  ) %>%
  arrange(i) %>%
  partial.tanks()

stopifnot(diff(dat$Date) >= 0)
stopifnot(diff(dat$Mileage) > 0)
stopifnot(all(dat$Gallons > 0, na.rm = TRUE))
stopifnot(all(!is.na(dat$Gallons) == dat$Notes %in% c("", "est", "unk")))
stopifnot(all(dat$Price > 0, na.rm = TRUE))

p1 <- dat %>%
  filter(!is.na(mpg)) %>%
  ggplot(aes(x = Date, y = mpg)) +
  geom_line() +
  geom_point(aes(color = Break, shape = smoothed)) +
  geom_hline(yintercept = median(dat$mpg, na.rm = TRUE)) +
  theme(legend.position = 'none') +
  scale_shape_manual(values = c("TRUE" = 15, "FALSE" = 16))

p2 <- ggplot(dat, aes(x = Date, y = Mileage)) +
  geom_line() +
  geom_point(aes(color = Break)) +
  theme(legend.position = 'none')

p3 <- dat %>%
  filter(!is.na(ppg)) %>%
  ggplot(aes(x = Date, y = ppg)) +
  geom_line() +
  geom_point()

plot(gridExtra::grid.arrange(p1, p2, p3, nrow = 2))
