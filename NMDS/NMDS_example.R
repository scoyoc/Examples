# Nonmetric Multidimensional Scaling (NMDS) Example
# Van Scoyoc, Matthew
# 29 September, 2014
#
# Data files: varespec
# Associated files: biostats.r
#
# References: Borcard, D., Gillet, F. & Legendre, P. (2011) Numerical Ecology with R. Chaper 5.
#             R_document_8_NMDS.pdf
#             vegantutor.pdf
#
# Notes: 
# This example uses the source file 'biostats.R' developed by Kevin McGarigal at the University of 
# Massachusetts.
#
# This example will use the 'varespec' and 'varechem' data sets in the vegan package.
# I have code to make a sweet plot using ggplot if you ever want it.
#

#----- Packages -----
library("vegan") # ecological community analysis
library("cluster") # cluster analysis
library("ggplot2") # plotting
library("grid") # plotting

#----- Additional functions -----
source(file.choose("")) # biostats.R

#----- Data -----
# Cover values of 44 plant species
data(varespec)
# Soil characteristics for each site
data(varechem)

#----- Association Matrices -----
# Bray-Curtis dissimilarity for veg data
sp.bc = vegdist(varespec, method = 'bray')

#----- Nonmetric Multidimensionsl Scaling (NMDS) -----
# Run an NMDS using Bray-Curtis dissimilarity with 2 dimensions on the species data.
sp.nmds = metaMDS(varespec, distance = 'bray', k = 2, autotransform = FALSE, trymax = 100, 
                        zerodist = "add")
sp.nmds 
# Very good NMDS. 2 dimensions gave us a stress of 0.100 
# You probably won't get the same stress as I reported as NMDS is never the same due to the 
# random starts (or something like that, I actually knew at one time).

# Create species scores matrix
sp.sc = scores(sp.nmds)
# Create a goodness of fit vector
gof = goodness(sp.nmds, display = "sites")

# Fit species vectors to NMDS
spec.vec = envfit(sp.nmds, varespec, perm = 10000)
spec.vec

# Fit soil vectors to NMDS
chem.vec = envfit(sp.nmds, varechem, perm = 1000)
chem.vec

#--- Visualize the NMDS
# Examine the ordered observations next the NMDS illustrating 1) the goodness of fit for each 
# observation (site) and 2) the location each species plays in the ordination.
# This is a bit messy, but it's a good diagnostic figure.
par(mfrow = c(1, 2))
stressplot(sp.nmds, main = "Stress Plot")
plot(sp.nmds, type = "t", main = paste("Goodness of Fit \n (Strees:", round(sp.nmds$stress, 3), ")"))
points(sp.nmds, display = "sites", cex = gof * 100)
par(mfrow = c(1, 1))

# This plot helps you intrepret the axes. For example, Asis 1 is composed primarily of Ple.sch. 
# Axis 2 is composed of Cla.ste, Cla.arb, etc. Axis 2 is also correlated with AL, Fe, Mn, and Humdepth.
# Axis 1 is not strongly correlated with soil variables. You can change the axis probability sensitivity
# by changing the value of 'p.max' in the 'plot()' lines.
p = ordiplot(sp.sc, type = "n", main = paste("NMDS \n (Stress:", round(sp.nmds$stress, 3), ")"), 
             xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(sp.sc, row.names(sp.sc), pos = 1, offset = 0.25)
plot(spec.vec, p.max = 0.01, col = "blue")
plot(chem.vec, p.max = 0.01, col = 'red')

#--- Add colors from a cluster analysis to the NMDS
# Wards heirarchical cluster analysis using Bray-Curtis dissimilarity identifying 4 groups
sp.clus = hclust(sp.bc, method = 'ward.D', members = NULL)
sp.grps = cutree(sp.clus, k = 4)
grp.lev = levels(factor(sp.grps)) 
# Plot groups with NMDS
p = ordiplot(sp.sc, type = "n", main = paste("NMDS \n (Stress:", round(sp.nmds$stress, 3), ")"), 
             xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
for (i in 1:length(grp.lev))
{
  points(sp.sc[sp.grps == i, ], pch = 16, cex = 1.5, col = i + 1)
}
text(sp.sc, row.names(sp.sc), pos = 1, offset = 0.25)
plot(spec.vec, p.max = 0.01, col = "blue")
plot(chem.vec, p.max = 0.01, col = 'red')
legend('bottomright', paste('Group',c(1:length(grp.lev))), pch = 16, 
       col = 1 + c(1:length(grp.lev)), pt.cex = 2, bty = 'n')

#--- ggplot2 code
# Create plot scores dataframe
# I created an arbitrary group "manual.group" for demonstration puposes. 
sp.sc = scores(sp.nmds)
sp.sc = data.frame(sp.sc, cluster.grps = as.factor(sp.grps), manual.grps = c(rep("A", 12), rep("B", 12)))

# Format fitted vectors
# Species vectors
spp.vec = as.data.frame(scores(spec.vec, display = 'vectors'))
# Add a row of spp codes for plotting
spp.vec$label = rownames(spp.vec)
# Add p-values
spp.vec = cbind(spp.vec, pvals = spec.vec$vectors$pvals)
# Reduce to only significant vectors
spp.vec = spp.vec[spp.vec$pvals <= 0.1, ]
# Change spp names for plotting (not done on this example)
#spp.vec$env = c()
# Change label locations
spp.vec$labs.x = spp.vec$NMDS1 * 1.1
spp.vec$labs.y = spp.vec$NMDS2 * 1.1

# Soil vectors
soil.vec = as.data.frame(scores(chem.vec, display = 'vectors'))
soil.vec$label = rownames(soil.vec)
soil.vec = cbind(soil.vec, pvals = chem.vec$vectors$pvals)
soil.vec = soil.vec[soil.vec$pvals <= 0.1, ]

nmds.fig = ggplot(sp.sc, aes(x = NMDS1, y = NMDS2, color = cluster.grps, shape = manual.grps)) +
  geom_point() + 
  # Mannually assign colors to cluster groups
  scale_color_manual(values = c("green3", "yellow2", "red3", "blue")) +
  # Manually assign shapes to manual groups
  scale_shape_manual(values = c(15, 17)) +
  # Plot vectors (not working, I don't know why...)
#  coord_fixed() +
#  geom_segment(data = spp.vec, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
#               arrow = arrow(length = unit(0.25, 'cm')), color = 'blue') +
#  geom_text(data = spp.vec, aes(x = labs.x, y = labs.y, label = label), color = 'blue', size = 4) +
  labs(x = "NMDS1", y = "NMDS2", title = "Example NMDS biplot\nUsing ggplot2") +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.01))
nmds.fig

# End script