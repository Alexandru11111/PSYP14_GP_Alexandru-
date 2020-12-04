require("MASS")
library(MASS)
require(apa)
require(smacof)
library(vegan)

nations_diss <- sim2diss(Nations, method = 9, to.dist = TRUE) # Use 10 insted of 9 due to the scale ? However no value of 9 in the dataset

as.matrix(nations_diss, labels=TRUE)

(nations_mds_diss = isoMDS(nations_diss)) 
nations_mds_diss$points
nations_mds_diss$stress

"___________________________________________________________________________________"#dimens

x=nations_mds_diss$points[,1]
y=nations_mds_diss$points[,2]

"________________________________________________________________________________"#PLOT

x <- nations_mds_diss$points[,1]
y <- nations_mds_diss$points[,2]
plot(x, y, xlab = "Political ideology", ylab = "Economic status",
     xlim = range(nations_mds_diss$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(Nations), cex = 0.9)
abline(h=0, v=0, col = "gray60", lty = 2)

"________________________________________________________________________________"
nations_shep <- Shepard(nations_diss, nations_mds_diss$points) # THIS WORKS 



plot(nations_shep, pch = 20, xlab = "Dissimilarity",
     ylab = "Distance", xlim = range(nations_shep$x),
     ylim = range(nations_shep$x))

lines(nations_shep$x, nations_shep$yf, type = "S") 


"___________________________________________________________________"
# For fun 

#TEST WITH VEGAN PACKAGE 

initMDS(nations_diss)
metaMDS(nations_mds)

plot.wcmdscale(nations_mds_diss)
plot.meandist(nations_mds_diss)


ord <- decorana(Nations)


ord <- metaMDS(nations_diss) # better function 

autoplot(ord, genom = "text", legend = "none")


plot(ord, type = "n")
points(ord, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(ord, display = "spec", cex=0.7, col="blue")

