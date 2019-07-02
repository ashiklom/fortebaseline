#!/usr/bin/env Rscript
library(lubridate,
        mask.ok = c("as.difftime", "date", "intersect",
                    "setdiff", "union"),
        exclude = "here")
library(purrr, include.only = c("map_dfc", "exec"))
library(here)

obstime_seq <- c(
  # For the first month, every 3 hours
  seq(ISOdatetime(1902, 06, 01, 0, 0, 0, "UTC"),
      ISOdatetime(1902, 06, 30, 11, 59, 0, "UTC"),
      by = "3 hours"),
  # For the next two months, every day at noon
  seq(ISOdatetime(1902, 07, 01, 12, 0, 0, "UTC"),
      ISOdatetime(1902, 08, 31, 12, 0, 0, "UTC"),
      by = "1 day"),
  # Then, every 2 weeks, at noon
  seq(ISOdatetime(1902, 09, 01, 12, 0, 0, "UTC"),
      ISOdatetime(2003, 12, 30, 12, 0, 0, "UTC"),
      by = "2 weeks")
)
head(obstime_seq)
tail(obstime_seq)

funs <- list(year = year, month = month, day = mday,
             hour = hour, minute = minute, second = second)

obstime_db <- map_dfc(funs, exec, x = obstime_seq)
obstime_db_file <- here("analysis", "data", "derived-data", "obstime_db.time")
write.table(obstime_db, obstime_db_file,
            sep = "\t", row.names = FALSE, quote = FALSE)

# Upload file to pecan
## library(ssh)
## session <- ssh_connect("shik544@172.18.65.128")
## target_file <- path("/public", "shared-docker-volumes",
##                     "pecan_data", "dbfiles", "forte_obstime.time")
## upload <- scp_upload(session, obstime_db_file, "~/forte_obstime.time")
## cmd <- sprintf("echo \"%s\" | sudo -s -S mv -f %s %s",
##                mypassword, upload, target_file)
## ssh_exec_wait(session, cmd)
## ssh_disconnect(session)
