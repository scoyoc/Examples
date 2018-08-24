# Exploratory Data Analysis for Mulitvariate Statistics
# Data Screening, Transformation, & Standardizations
# Van Scoyoc, Matthew
# 29 September, 2014
#
# Data files: 
#
# Associated files: biostats.r
# 
# Associated directories: 
#
# References: Numerical Ecology with R, Chapter 2: Exploratory Data Analysis
#             C:\R\help\biostats\FSH 560 Multivariate Stats\...
#                  ...2_DataScreening_2010.pdf
#                  ...R_document_2_DataScreening.pdf
#
# Notes:
# Due to the large figure sizes, this R file is best run in the R Gui, not in RStudio.
#
# This example uses the source file 'biostats.R' developed by Kevin McGarigal at the University of 
# Massachusetts.
#
# This example will use the 'varespec' and 'varechem' data sets in the vegan package.
############################################################

#----- Packages -----
library(vegan) # ecological community analysis
library(plyr) # splitting, applying and combining data

#----- Additional functions -----
source(file.choose("")) # biostats.R

# Examine properties of a data set
data.props = function (data){
  d.class = class(data)
  d.dim = dim(data)
  d.rows = rownames(data)
  d.cols = colnames(data)
  row.na = apply(data, 1, function(x) sum(is.na(x)))
  col.na = apply(data, 2, function(x) sum(is.na(x)))
  look = head(data)
  
  z = list(d.class, d.dim, d.rows, d.cols, row.na, col.na, look)
  names(z) = c('Class', "Dimensions", "RowNames", "ColumnNames", "Rows.NA", 
               "Cols.NA", "First.6.Rows")
  
  return(z)
}

# Puts histograms on the diagonal panels in the 'pairs()' funcion
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Put (absolute) correlations on the upper panels in the 'pairs()' funcion,
# with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#----- Data -----
# Cover values of 44 plant species
data(varespec)
# Soil characteristics for each site
data(varechem)

#----- Exploratory Data Analysis -----
#----- Examination of species data -----
data.props(varespec)
summary(varespec)

#--- Examine the frequency of occurance and abundance
# Produces 10 plots, press 'Enter' in the console to advanse to the next plot
# See biostats_help.pdf for more info
# Plot 1: Empirical distribution of species occurrence
# Plot 2: Empirical distribution of species relative occurrence
# Plot 3: Histogram of species occurrence
# Plot 4: Histogram of log-transformed species occurrence
# Plot 5: Empirical distribution of species mean abundance
# Plot 6: Frequency of occurrence versus mean abundance
# Plot 7: Frequency of occurrence versus log of mean abundance
# Plot 8: Empirical distribution of species per plot
# Plot 9: Empirical distribution of total plot abundance
# Plot 10: Plot richness versus plot total abundance
foa.plots(varespec)

#--- Examine the distributional properties of each species
# Produces 4 plots in a 2X2 frame to examine the distribution of each species
# See biostats_help.pdf for more info
# Upper Right: Histogram
# Upper Left: Box-and-whisker plot
# Lower Right: ECDF plot
# Lower Left: Normal QQ plot
uv.plots(varespec)
# Also check out 'ecdf.plots()', 'hist.plots()', 'box.plots()', and 'qqnorm.plots()' to examine 
# these distributions individually

#--- Examine the data set for outliers
# Mulitvariate outliers
mv.outliers(varespec, method = 'euclidean', sd.limit = 3)

#----- Examination of soil data -----
data.props(varechem)
summary(varechem)

#--- Examine the distributional properties of each species
# Produces 4 plots in a 2X2 frame to examine the distribution of each species
# See biostats_help.pdf for more info
# Upper Right: Histogram
# Upper Left: Box-and-whisker plot
# Lower Right: ECDF plot
# Lower Left: Normal QQ plot
uv.plots(varechem)
# Also check out 'ecdf.plots()', 'hist.plots()', 'box.plots()', and 'qqnorm.plots()' to examine 
# these distributions individually

# Examine correlations between variables 
pairs(varechem, lower.panel = panel.smooth, diag.panel = panel.hist, upper.panel = panel.cor, 
      main = "Bivariate Plots with Histograms for Plot Attributes")

#--- Examine the data set for outliers
# Mulitvariate outliers
mv.outliers(varechem, method = 'euclidean', sd.limit = 3)

# End script