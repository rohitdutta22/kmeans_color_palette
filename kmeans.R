library(imager)
library(Rcpp)
sourceCpp("pixel.cpp")

## pixel matrix, eliminating the depth component
kmeans_manual = function(loaded_img, cluster_no){

dat.img <- as.array(((loaded_img)[,,1,]))
p = list()

temp <- data.frame(red = as.vector(dat.img[,,1]),
                   green = as.vector(dat.img[,,2]),
                   blue = as.vector(dat.img[,,3]))

## Looking at the picture choosing 10 random points as initial cluster points
set.seed(5000)
x.cor <- sample(1:(dim(dat.img))[1],cluster_no)
set.seed(6913)
y.cor <- sample(1:(dim(dat.img))[2],cluster_no)

## Initializing a data frame of cluster points
cluster.temp.data = as.data.frame(matrix(nrow = cluster_no, ncol = 3))
colnames(cluster.temp.data) = c("red","green","blue")

## Obtaining the initial k = cluster_no clusters
for(i in 1:cluster_no){
  cluster.temp.data[i,] = dat.img[x.cor[i],y.cor[i],]
}

## For faster performance C++ is used


index.col1 <- pixelC(as.matrix(cluster.temp.data), as.matrix(temp), cluster_no)

col.proportion1 <- table(index.col1)/(dim(dat.img)[1]*dim(dat.img)[2])

foo.dat1 <- data.frame(temp, index.col1)

## Calculating the final cluster means as the cluster representative 
updated.cluster1 <- (aggregate(foo.dat1[,1:3], list(foo.dat1$index.col1), mean))[-1]


foo.dat2 <- pixel_C(as.matrix(updated.cluster1),index.col1,col.proportion1,as.matrix(temp),cluster_no)

dim1 <- (dim(dat.img))[1]
dim2 <- (dim(dat.img))[2]

kmeans_data <- data.frame(floor(foo.dat2[,1:3]*255), col.proportion1 = foo.dat2[,4], x.cor, y.cor,dim1,dim2)
colnames(kmeans_data) = c("red","green","blue",
                          "cluster_proportion","x.cor","y.cor","dim1","dim2")
p[[1]] <- kmeans_data


## Obtaining the color palette corresponding to the obtained RGB clusters

individual_img_data <- kmeans_data[,1:3]
rgb.mat <- array(0, dim = c(cluster_no*100,1200,3))
iter <- c(seq(1,cluster_no*100,100),(cluster_no*100)+1)

rgb.mat[,,1] <- red_mat(as.matrix(individual_img_data), iter, cluster_no)
rgb.mat[,,2] <- green_mat(as.matrix(individual_img_data), iter, cluster_no)
rgb.mat[,,3] <- blue_mat(as.matrix(individual_img_data), iter, cluster_no)


p[[2]] <- rgb.mat

return(p)

}

##############*******************####################****************###############




######################################################################################




par(mfrow = c(1,2))
load.img <- load.image("van_gogh_img130.jpeg")

plot(load.img, axes = FALSE, main = "Original Picture", cex = 1.5)
output <- kmeans_manual(load.img, 10)
plot(as.cimg(output[[2]]), axes = FALSE, main = "Manually Made Color Palette")





## This the color palette obtained using the function from library 'colorfindr'
library(colorfindr)

dat <- get_colors("van_gogh_img130.jpeg")
make_palette(dat,n = 10, clust_method = "kmeans")
title(main = "Color Palette using 'colorfindr'")


library(rbenchmark)
benchmark(
  {
    loaded_img <- load.image("van_gogh_img130.jpeg")
    output <- kmeans_manual(loaded_img, 10)
    plot(as.cimg(output[[2]]), axes = FALSE, main = "Manually Made Color Palette")
  },
  {dat <- get_colors("van_gogh_img130.jpeg")
  make_palette(dat,n = 10, clust_method = "kmeans")
  title(main = "Color Palette using 'colorfindr'")
  }, replications = 1
)











