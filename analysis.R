library(tidyverse)
library(arsenal)

impute <- function(d)
{
  for(u in which(is.na(d$Mileage)))
  {
    d$Mileage[u] <- mean(d$Mileage[u + c(-1, 1)])
  }
  needgallons <- which(is.na(d$Gallons) & d$Notes %in% "unk" & !is.na(d$Price))
  stopifnot(length(needgallons) == 2)
  for(u in needgallons)
  {
    i <- if(is.na(d$Gallons[u - 1])) -2 else -1
    d$Gallons[u] <- d$Price[u]*mean(d$Gallons[u + c(i, 1)] / d$Price[u + c(i, 1)])
  }
  for(u in which(is.na(d$Date)))
  {
    d$Date[u] <- round(mean(d$Date[u + c(-1, 1)]))
  }
  d
}

partial.tanks <- function(d)
{
  # didn't buy car with full tank
  d$mpg[d$Notes %in% c("First fill-up")] <- NA_real_

  pts <- c("2014-06-21", "2014-07-18", "2015-06-13", "2015-07-18", "2016-11-05")
  fixem <- which(d$Date %in% as.Date(pts) & d$Price %nin% 25.85)
  stopifnot(length(pts) == length(fixem))
  for(u in fixem)
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
  arrange(is.na(Gallons) & (Notes %nin% c("Bought", "unk", "unknown fill-up")), Date) %>%
  mutate(
    miles = if_else(is.na(Gallons), NA_real_, c(NA_real_, diff(Mileage))),
    mpg = miles / Gallons,
    ppg = Price / Gallons,
    Break = case_when(
      Date >= as.Date("2019-05-30") ~ "After 1",
      Date >= as.Date("2017-01-08") ~ "After",
      TRUE ~ "Before"
    ),
    smoothed = FALSE
  ) %>%
  partial.tanks() %>%
  arrange(i)

stopifnot(diff(dat$Date) >= 0)
stopifnot(diff(dat$Mileage) > 0)
stopifnot(all(dat$Gallons > 0, na.rm = TRUE))
stopifnot(all(!is.na(dat$Gallons) == dat$Notes %in% c("", "est", "unk", "First fill-up")))
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
