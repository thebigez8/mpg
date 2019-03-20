library(tidyverse)

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

dat <- "mpg.csv" %>%
  read_csv(col_types = cols(), col_names = TRUE, na = "?") %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Notes = replace(Notes, is.na(Notes), ""),
    i = 1:n()
  ) %>%
  impute() %>%
  arrange(is.na(Gallons) + (Notes %nin% c("Bought", "unk")), Date) %>%
  mutate(
    miles = if_else(is.na(Gallons), NA_real_, c(NA_real_, diff(Mileage))),
    mpg = miles / Gallons,
    ppg = Price / Gallons
  ) %>%
  arrange(i)

stopifnot(diff(dat$Date) >= 0)
stopifnot(diff(dat$Mileage) > 0)
stopifnot(all(dat$Gallons > 0, na.rm = TRUE))
stopifnot(all(!is.na(dat$Gallons) == dat$Notes %in% c("", "est", "unk")))
stopifnot(all(dat$Price > 0, na.rm = TRUE))

p1 <- dat %>%
  filter(!is.na(mpg)) %>%
  ggplot(aes(x = Date, y = mpg)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = median(dat$mpg, na.rm = TRUE))

p2 <- ggplot(dat, aes(x = Date, y = Mileage)) +
  geom_point() +
  geom_line()

p3 <- dat %>%
  filter(!is.na(ppg)) %>%
  ggplot(aes(x = Date, y = ppg)) +
  geom_line() +
  geom_point()

plot(gridExtra::grid.arrange(p1, p2, p3, nrow = 2))
