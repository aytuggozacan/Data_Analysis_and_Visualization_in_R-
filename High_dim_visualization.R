library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(GGally)
library(pheatmap)
library(mclust)

#Section 1

expr <- readRDS("extdata/cancer_data.rds") %>% as.data.table(keep.rownames="tumor_type")
head(expr[, 1:6])

# Plot pairwise correlations
ggcorr(expr[, -"tumor_type"])

expr_mat <- as.matrix(expr[, -"tumor_type"])
rownames(expr_mat) <- expr[, tumor_type]
pheatmap(expr_mat, cluster_rows = F, cluster_cols = F)

# obviously two data points are completely off -->  find them
expr_melt <- melt(expr, id.vars='tumor_type')
expr_melt[order(-value)]

# the expression value of the tumor type  DOHH2 for the genes FUK and UGP2 seems
# to be way too high.
# These look like outliers. Let's plot them to check
# we would like to plot the expression value of genes FUK and UGP2 # It's better to use the original data table for this
ggplot(expr, aes(FUK, UGP2)) + geom_point()

# We can substitute the outlier with NA
expr[tumor_type == "DOHH2", FUK := NA] expr[tumor_type == "DOHH2", UGP2 := NA]

# Now check the correlation again. We see that the two genes that were previously
# highly correlated do not show a high correlation after removing the outliers.
# Lesson learned: always have a look at the raw data when doing a correlation analysis 
ggcorr(expr[, !"tumor_type"])

# Now plot heatmap again
expr_mat <- as.matrix(expr[, !"tumor_type"])
rownames(expr_mat) <- expr[, tumor_type]
pheatmap(expr_mat, cluster_rows = F, cluster_cols = F)

#Section 2

# Exclude Species column
plot.data <- as.matrix(iris[, -5])
pheatmap(plot.data,cluster_rows=F, cluster_cols=F, scale='column')

pheatmap(plot.data, show_rownames=F, scale='column',
         clustering_method = "complete")

## pheatmap() returns an object with dendrograms
h_complete <- pheatmap(plot.data, show_rownames=F,
                       scale='column', clustering_method = "complete", silent=T)
# silent=T prevents heatmap to be displayed again
complete <- cutree(h_complete$tree_row, k = 3)

## label the row names to be able to annotate rows
rownames(plot.data) <- 1:nrow(plot.data)
## create a data.frame for the row annotations
row.ann <- data.table(Species = iris$Species)
row.ann[, complete:=factor(complete)] # the clusters need to be factors
## plot the heatmap with complete linkage clustering
pheatmap(plot.data, annotation_row = row.ann, show_rownames=F,
         scale='column', clustering_method = "complete")

#Section 3
# No coding needed

#Section 4

dt <- data.table(x = c(8,10,4,5,6,6,3),
                 y = c(5,7,6,8,4,9,3),
                 letter=c('A','B','C','D','E','X1','X2') )
# there are three steps to k-means
# 1. decide how many k (number of clusters) --> here two
# 2. calculate the distance of each point to all centroids
# 3. take all points belonging to one cluster and compute new centroid (mean)
# a) compare deltax1 to deltax2
# b) compute new mean
# 2. compute distances
#euclidean distance: (x-x_1)**2 + (y-y_1)**2
dt[, delta.x1:= x - dt[letter == "X1", x]]
# computes distance along x axis between every letter and X1 
dt[, delta.x2:= x - dt[letter == "X2", x]]
# computes distance along x axis between every letter and X2 
dt[, delta.y1:= y - dt[letter == "X1", y]]
# computes distance along y axis between every letter and X1 
dt[, delta.y2:= y - dt[letter == "X2", y]]
# computes distance along y axis between every letter and X2 
dt[, distance.x1 := sqrt(delta.x1**2 + delta.y1**2)]
# computes euclidean distance between each datapoint and X1 
dt[, distance.x2 := sqrt(delta.x2**2 + delta.y2**2)]
# computes euclidean distance between each datapoint and X2
# 3a. choose clostest centroid

dt[, cluster := ifelse(distance.x1 < distance.x2, "X1", "X2")] # choses closest centroid
#compute new mean
new.centroids <- dt[1:5, .(mean.x = mean(x), mean.y = mean(y)), by="cluster"]
new.centroids

# we want to cluster data into 3 clusters k=3
#kmeans: always scale your data
scaled.data <- scale(plot.data) # it is important to scale your data for k-means 
km = kmeans(scaled.data, 3)
km

# The table allows us to see in how many observations the methods coincided # for each of the 3 clusters
table(complete, km$cluster)

row.ann[, kmeans := factor(km$cluster)]
pheatmap(scaled.data, annotation_row=row.ann, show_rownames=F,
         clustering_method = "complete")

#Section 5 

#install.packages("fossil")
library(fossil)

#RI = (a+b)/ n(n-1)/2 --> number of pairs
#a --> pairs that are in the same cluster in both clustering
#b --> pairs that are in different clusters in both clusterings
rand.index(complete, complete)

## Convert Species to numeric
row.ann[, Species:= as.numeric(Species)]
## compute all pairwise rand indices
# the apply function applies a function along either the columns or the rows
# this can be specified with the margin parameter: margin=1 is rows, margin = 2
# is for columns. Here we want to comapare all columns with each other, so we choose
# margin = 2
# you could achieve the same by just iterating the over the columns and computing the
# the rand.index
rand <- apply(row.ann, 2, function(i)
  apply(row.ann, 2, function(j) rand.index(as.numeric(i), as.numeric(j))))
rand

pheatmap(rand, cluster_cols = F, cluster_rows = F)

rand_dt <- data.table(rand, keep.rownames = 'Clustering1') %>% 
  melt(id.vars='Clustering1', value.name= "rand_index", variable.name= "Clustering2")

rand_dt[rand_index<1 & Clustering1=='Species'][which.max(rand_index)]

#Section 6

## get the data
data(iris)
iris_dt <- as.data.table(iris)
pca_data <- iris_dt[Species == "setosa", -"Species"]
# we always need to scale our data for PCA analysis to make sure that the mean
# is in the origin and that our variances are on a similar scale
# good thing is that prcomp does this if you provide center=TRUE and scale.=True 
pca <- prcomp(pca_data, center=TRUE, scale.=TRUE)
pca

# we can look at the explained variance with the summary on the results
summary(pca)

# Compute projection of the data using the predict function.
proj <- as.data.table(predict(pca))
ggplot(proj, aes(PC1, PC2)) + geom_point()

# The biplot can be shown the following way:
biplot(pca)

pc_iris <- cbind(iris_dt[Species == "setosa"], proj)
pc_iris <- melt(pc_iris, id.vars = c("Species", 'PC1', 'PC2', 'PC3', 'PC4'))
ggplot(pc_iris, aes(value, PC1)) + geom_point() + facet_wrap(~variable, scales = 'free')

# First run the pca on all species
pca_data <- iris_dt[, -"Species"]
pca <- prcomp(pca_data, center=TRUE, scale.=TRUE)
pca

# Compute projection of the data using the predict function. proj <- as.data.table(predict(pca))
pc_iris <- cbind(iris_dt, proj)
ggplot(pc_iris, aes(PC1, PC2, color = Species)) + geom_point()

# In the plot above one can see that the first principal component clearly seperates the s
# The biplot can be shown the following way:
biplot(pca)

# In the biplot one can observe that the lengths and widths do not contribute with the
# same sign any longer.
pc_iris <- melt(pc_iris, id.vars = c("Species", 'PC1', 'PC2', 'PC3', 'PC4'))
ggplot(pc_iris, aes(value, PC1, color=Species)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')


