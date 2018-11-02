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
#-- EDGUT
dly.prcp <- dat %>%
  group_by(Date) %>%
  summarise(PRCP = round(max(precip_accum_one_hour_set_1) - 
              min(precip_accum_one_hour_set_1), 2))
max(dat$precip_accum_one_hour_set_1) - min(dat$precip_accum_one_hour_set_1)
ggplot(data = dly.prcp, aes(x = Date, y = PRCP)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(data = filter(dly.prcp, PRCP > 0), 
            aes(label = PRCP), vjust = -0.3, size = 3.5) +
  ggtitle("EDGUT") +
  theme_bw()

#-- NLPU1
dly.prcp <- dat %>%
  group_by(Date) %>%
  summarise(PRCP = round(max(precip_accum_set_1) - min(precip_accum_set_1), 2))
max(dat$precip_accum_set_1) - min(dat$precip_accum_set_1)
ggplot(data = dly.prcp, aes(x = Date, y = PRCP)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(data = filter(dly.prcp, PRCP > 0), 
            aes(label = PRCP), vjust = -0.3, size = 3.5) +
  ggtitle("NLPU1") +
  theme_bw()

