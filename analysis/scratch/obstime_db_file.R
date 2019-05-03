library(lubridate, mask.ok = c("as.difftime", "date", "intersect",
                               "setdiff", "union"))
library(purrr, include.only = c("map_dfc", "exec"))

obstime_seq <- c(
  # For the first month, every 3 hours
  seq(ISOdatetime(1902, 06, 01, 0, 0, 0, "UTC"),
      ISOdatetime(1902, 06, 30, 11, 59, 0, "UTC"),
      by = "3 hours"),
  # For the next two months, every day at noon
  seq(ISOdatetime(1902, 07, 01, 12, 0, 0, "UTC"),
      ISOdatetime(1902, 08, 31, 12, 0, 0, "UTC"),
      by = "1 day"),
  # Then, every 4 weeks, at noon
  seq(ISOdatetime(1902, 09, 01, 12, 0, 0, "UTC"),
      ISOdatetime(2003, 12, 30, 12, 0, 0, "UTC"),
      by = "4 weeks")
)
head(obstime_seq)
tail(obstime_seq)

funs <- list(year = year, month = month, day = mday,
             hour = hour, minute = minute, second = second)

obstime_db <- map_dfc(funs, exec, x = obstime_seq)
write.table(obstime_db, "analysis/data/retrieved/obstime_db.time",
            sep = "\t", row.names = FALSE, quote = FALSE)
