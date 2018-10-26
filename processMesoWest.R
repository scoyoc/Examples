# Setup ----
library("tidyverse")
library("lubridate")

# Data ----
my.file <- file.choose()
header <- unlist(read.table(my.file, header = F, sep = ",", nrows = 1, skip = 6, 
                            stringsAsFactors = F))
dat <- tbl_df(read.table(my.file, header = F, sep = ",", skip = 8, 
                         col.names = header, 
                         stringsAsFactors = F)) %>%
  mutate(Date_Time = mdy_hm(Date_Time), 
         Date = date(Date_Time))
dat <- dat[, !apply(is.na(dat), 2, all)]

# Summarize ----
dly.prcp <- dat %>%
  group_by(Date) %>%
  summarise(PRCP = max(precip_accum_set_1) - min(precip_accum_set_1))

qplot(x = dly.prcp$Date, y = dly.prcp$PRCP, geom = "line", 
      xlab = "Date", ylab = "PRCP", main = unique(dat$Station_ID)) +
  theme_bw()

max(dat$precip_accum_set_1) - min(dat$precip_accum_set_1)
