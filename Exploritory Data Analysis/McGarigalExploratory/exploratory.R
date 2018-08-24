###Analysis of Environmental Data -- Lab
###Data Exaploration, Screening & Adjustments -- R script

###1. Set up your R work session
setwd('c:/work/stats/ecodata/lab/exploratory/')
source('c:/work/r/scripts/biostats.R')
birds<-read.csv('bird.sub.csv',header=TRUE)
hab<-read.csv('hab.sub.missing.csv',header=TRUE)
str(birds)
str(hab)

###2. Summary statistics
col.summary(hab[,-c(1:3)]) #use table() for factors
sum.stats(birds,var='AMCR:YRWA',margin='column')
sum.stats(birds,var='AMCR:YRWA',margin='row')

###3. Missing data
temp<-na.omit(hab)
mean(hab$s.sidi)
mean(hab$s.sidi,na.rm=TRUE)
temp<-hab
temp[is.na(temp)]<-0
temp$s.sidi
temp<-hab
temp$s.sidi[is.na(temp$s.sidi)]<-mean(temp$s.sidi,na.rm=TRUE)
temp$s.sidi
temp<-hab
temp$s.sidi[is.na(temp$s.sidi)]<-median(temp$s.sidi,na.rm=TRUE)
temp$s.sidi
temp<-replace.missing(hab,var='sub.lat:w')
temp$s.sidi
temp<-replace.missing(hab,var='sub.lat:w',method='mean')
temp$s.sidi
temp<-drop.var(hab,var='sub.lat:w',pct.missing=5)
str(temp)

###4. Frequency of occurrence and abundance plots
foa.plots(birds,var='AMCR:YRWA')

###5. Dropping variables
temp<-drop.var(birds,var='AMCR:YRWA',min.fo=3)
temp<-drop.var(birds,var='AMCR:YRWA',max.po=95)
temp<-drop.var(birds,var='AMCR:YRWA',min.cv=5)

###6. Single variable distributions

#6.1 Empirical (cumulative) distribution functions
edf.plots(birds,var='AMCR:YRWA')
ecdf.plots(birds,var='AMCR:YRWA')
ecdf.plots(birds,var='AMCR:YRWA',by='basin')

#6.2 Histograms
hist.plots(birds,var='AMCR:YRWA')
hist.plots(birds,var='AMCR:YRWA',by='basin')

#6.3 Box-and-whisker plots
box.plots(birds,var='AMCR:YRWA')
box.plots(birds,var='AMCR:YRWA',by='basin')

#6.4 Normal quantile-quantile plots
qqnorm.plots(birds,var='AMCR:YRWA')
qqnorm.plots(birds,var='AMCR:YRWA',by='basin')

#6.5 Four-in-one plots
uv.plots(birds,var='AMCR:YRWA')

###7. Relationships between pairs of variables

#7.1 Correlations
cor(birds[,-c(1:3)])
cor(birds[,-c(1:3)],method='spearman')

#7.2 Scatterplots
plot(hab$ls,birds$BRCR)
lines(lowess(hab$ls,birds$BRCR))
birdhab<-merge(hab,birds,by=c('basin','sub'))
scatter.plots(birdhab,y='AMCR:YRWA',x='ls')

#7.3 Scatterplot matrix
pairs(hab[,9:16])
pairs(hab[,9:16],lower.panel=panel.smooth,upper.panel=panel.cor,method='spearman')

#7.4 Coplots
coplot(BRCR~ls|sub.elev,data=birdhab,panel=panel.smooth)

#7.5 Redundancy plots
redun.plot(birds[,-c(1:3)])
redun.plot(birds[,-c(1:3)],var='AMCR:YRWA')

###8. Outliers
uv.outliers(hab,id=c('basin','sub'),var='sub.lat:w')
mv.outliers(birds,var='AMCR:YRWA',method='bray')
#mv.outliers(birds,var='AMCR:YRWA',method='mahalanobis')

###9. Data transformations

#9.1 Log transformation
data.trans(birds,var='AMCR:YRWA',method='log')

#9.2 Power transformation
data.trans(birds,var='AMCR:YRWA',method='power',exp=.5)
data.trans(birds,var='AMCR:YRWA',method='power',exp=0)

#9.3 Arcsin square root transformation
data.trans(hab,var='s.sidi',method='asin')

###10. Data standardizations
data.stand(birds,var='AMCR:YRWA',method='normalize',margin='row')

###11. Dissimilarity matrices
data.dist(birds[,-c(1:3)],method='euclidean')
data.dist(birds[,-c(1:3)],method='bray')
