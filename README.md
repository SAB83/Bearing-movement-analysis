# Bearing-movement-analysis
Calculating bearing of movement of deployed motus tags on juveniles and adults of migratory birds and plotting their movements and their flight distance
data <- read.csv("/home/baniasss/164/movement/data.csv")


# Let's get rid extra "X" columns with select()

#data <- select(data, -starts_with("X"))

data$sex<-revalue(data$sex, c("M"="Adult", "F"="Adult","U"="Juveniles"))

data <- filter(data, !is.na(V)) 

##Analysis of Variance for circular data
##Analysis of Variance for circular data
##Analysis of Variance for circular data

##selecting just site 1

data <- filter(data, site==1) 
data <- filter(data, site==2) 


group<- data$sex

x<- data$V

aov.circular(x, group, kappa = NULL,
             method = c("F.test", "LRT"), F.mod = TRUE, control.circular=list())

## S3 method for class 'aov.circular'circular analysis of variance
#to test whether the means of the bearings depended on age 

print(x, digits = max(3, getOption("digits") - 3), ...)


aov.circular(x, group)

aov.circular(x, group, method="LRT")

#Mean Mean Resultant Length 

rho.circular(b$J2, na.rm = TRUE)

#mean direction

#A1: c(127.6678, 	294.15459,	184.7675, 	242.7005, 	250.8453, 194.747, 	89.81581,	84.28495,	246.1145,	67.30797)
#J1: c(251.4676,	67.30797,	251.7804,	251.9206,	250.9346,	294.15459,	89.81581,	251.9206,	251.5219,	251.8396,	251.8396,
#248.2641,246.161,	222.0518,	235.7786,	227.5044,	252.3998,	251.9219,	239.3041,	249.3744)

#A2:c(323.73419,	170.3115,	181.1295,	157.1818)
#J2:c(323.73419,	179.1788,	120.5493,	176.3291,	188.5305,	181.491,	125.9794,	180.1792,	182.5466,	164.5033,
#190.5269,	165.7049,	176.1885,	172.2055,	164.849)

angles =c(251.4676,	67.30797,	251.7804,	251.9206,	250.9346,	294.15459,	89.81581,	251.9206,	251.5219,	251.8396,	251.8396,
          248.2641,246.161,	222.0518,	235.7786,	227.5044,	252.3998,	251.9219,	239.3041,	249.3744)
anglecir =  circular(angles, type="angles", units="degrees",modulo="2pi", template='geographics')
mean(anglecir)

x<-c(127.6678, 	294.15459,	184.7675, 	242.7005, 	250.8453, 194.747, 	89.81581,	84.28495,	246.1145,	67.30797)
circ.mean(x)

#------------------------------------------------
#Performs Watson’s test for homogeneity on two samples of circular data.

library(CircStats)

#A1: c(127.6678, 	294.15459,	184.7675, 	242.7005, 	250.8453, 194.747, 	89.81581,	84.28495,	246.1145,	67.30797)
#J1: c(251.4676,	67.30797,	251.7804,	251.9206,	250.9346,	294.15459,	89.81581,	251.9206,	251.5219,	251.8396,	251.8396,
#248.2641,246.161,	222.0518,	235.7786,	227.5044,	252.3998,	251.9219,	239.3041,	249.3744)

#A2:c(323.73419,	170.3115,	181.1295,	157.1818)
#J2:c(323.73419,	179.1788,	120.5493,	176.3291,	188.5305,	181.491,	125.9794,	180.1792,	182.5466,	164.5033,
#190.5269,	165.7049,	176.1885,	172.2055,	164.849)


b <- read.csv("/home/name/164/movement/b.csv")

J<- filter(b, !is.na(J2)) 

A<- filter(b, !is.na(A2)) 

x<-c(127.6678, 	294.15459,	184.7675, 	242.7005, 	250.8453, 194.747, 	89.81581,	84.28495,	246.1145,	67.30797)
y<-c(251.4676,	67.30797,	251.7804,	251.9206,	250.9346,	294.15459,	89.81581,	251.9206,	251.5219,	251.8396,	251.8396,
     248.2641,246.161,	222.0518,	235.7786,	227.5044,	252.3998,	251.9219,	239.3041,	249.3744)

x<-b$A2

y<-b$J2

watson.two(x, y, alpha=0.05, plot=TRUE)

rho.circular(y, na.rm = FALSE)

#Site1
#Adults1

#A1: c(127.6678, 	294.15459,	184.7675, 	242.7005, 	250.8453, 194.747, 	89.81581,	84.28495,	246.1145,	67.30797)
#J1: c(251.4676,	67.30797,	251.7804,	251.9206,	250.9346,	294.15459,	89.81581,	251.9206,	251.5219,	251.8396,	251.8396,
#248.2641,246.161,	222.0518,	235.7786,	227.5044,	252.3998,	251.9219,	239.3041,	249.3744)

testlen<-c(1,	1,	1,	1,	1,1,1,1,1,1)

testpos<-c(127.6678, 	294.15459,	184.7675, 	242.7005, 	250.8453, 194.747, 	89.81581,	84.28495,	246.1145,	67.30797)

testlen<-c(1,	1,	1,	1,	1,1,1,1,1,1,1,	1,	1,	1,	1,1,1,1,1,1)
testpos<-c(251.4676,	67.30797,	251.7804,	251.9206,	250.9346,	294.15459,	89.81581,	251.9206,	251.5219,	251.8396,	251.8396,
  248.2641,246.161,	222.0518,	235.7786,	227.5044,	252.3998,	251.9219,	239.3041,	249.3744)


par(cex.axis = 1.5, font.axis = 2)

polar.plot(testlen,testpos,radial.lim=c(0,1),clockwise=TRUE,start=90,line.col=(2:7),
           boxed.radial=FALSE, show.grid.labels=0, lwd=3)


#Juveniles1

testlen<-c(1,1,1,1,1,1,1,1,1,1,1,1,8)

testpos<-c(67.30797,		250.9346,	294.15459,	89.81581,	248.2641,246.161,	222.0518,	
           235.7786,	227.5044,	252.3998,	239.3041,	249.3744,251.4676)

polar.plot(testlen,testpos,radial.lim=c(0,8),clockwise=TRUE,start=90,line.col=(0:8),
           boxed.radial=FALSE, show.grid.labels=0, lwd=3)


#------------------------------------------------------------------#
#Site2
#Adult


#A2:c(323.73419,	170.3115,	181.1295,	157.1818)
#J2:c(323.73419,	179.1788,	120.5493,	176.3291,	188.5305,	181.491,	125.9794,	180.1792,	182.5466,	164.5033,
#190.5269,	165.7049,	176.1885,	172.2055,	164.849)


testlen<-c(1,1,1,1)

testpos<-c(323.73419,	170.3115,	181.1295,	157.1818)

polar.plot(testlen,testpos,radial.lim=c(0,1),clockwise=TRUE,start=90,line.col=(1:6),
           boxed.radial=FALSE, show.grid.labels=0, lwd=3, label.pos=NULL)

#Adults2

testlen<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

testpos<-c(323.73419,	179.1788,	120.5493,	176.3291,	188.5305,	181.491,	125.9794,	180.1792,	182.5466,	164.5033,
           190.5269,	165.7049,	176.1885,	172.2055,	164.849)

polar.plot(testlen,testpos,radial.lim=c(0,1),clockwise=TRUE,start=90,line.col=(1:10),
           boxed.radial=FALSE, show.grid.labels=0, lwd=3)

