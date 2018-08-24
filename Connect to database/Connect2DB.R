#' ---
#' title: "Connect2DB.R"
#' author: "Van Scoyoc, Matthew W."
#' date: "30 August, 2016"
#' ---
#'   
#' Data:  
#' - CARE_RHAD_2.0.accdb  
#'   
#' Notes:  
#' This example shows how to use RODBC to connect to an Microsoft Access 2010
#' database.  
#----------

#+ Setup, message=FALSE ----
#' Packages
#install.packages(c("RODBC", "dplyr", "tidyr", "knitr", "ggplot2"))
library("RODBC")
library("dplyr")
library("tidyr")
library("knitr")
library("ggplot2")

#+ Data, message=FALSE ----
#' Open database connections
my.dir <- "P:/1_ResourceStewardshipScience/Natural Resource Program/1 CARE range assessment project/4 Data & Photos/CARE RHAD"
my.db <- "CARE_RHAD_2.0.accdb"
data.db <- odbcConnectAccess2007(paste(my.dir, my.db, sep = "/"))

#' Read in plot data
plots.sampd <- sqlFetch(data.db, "tblPlots_Sampled")
#' Summarise the number of plots sampled by allotment
kable(plots.sampd %>% 
        group_by(Allotment) %>%
        summarise(n())
  )

#' Read in, subset, and summarize cattle and elk scat data
scat.cow.v.elk <- tbl_df(sqlFetch(data.db, "CARE_RHA_PelletCounts")) %>%
  filter(Animal == "cattle" | Animal == "elk") %>%
  select(-SumOfQuads) %>%
  group_by(Plot_ID, Animal) %>%
  summarise(Total = sum(SumOfTally)) %>%
  left_join(select(plots.sampd, Plot_ID, Allotment)) %>%
  group_by(Allotment, Animal) %>%
  summarise(Total = sum(Total))
kable(scat.cow.v.elk)

#+ Figures ----
#' Make a bar plot of cattle and elk scat observed in each allotment.
g <- ggplot(scat.cow.v.elk, aes(x = Animal, y = Total, fill = Animal)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Allotment) +
  theme_bw()
g

#+ Close DB, eval = TRUE, echo = TRUE ----
odbcClose(data.db)
rm(data.db)

#+ EndScript ----
Sys.Date()
sessionInfo()