##################################### PCA #######################################################
#
#
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/#pca-data-format
#
# PCA method is particularly useful when the variables within the data set are highly correlated.
#
#************************************************************************************************

# In principal component analysis, variables are often scaled (i.e. standardized). This is particularly
# recommended when variables are measured in different scales (e.g: kilograms, kilometers, centimeters, …);
# otherwise, the PCA outputs obtained will be severely affected.
# 
# The goal is to make the variables comparable. Generally variables are scaled to have i) standard deviation
# one and ii) mean zero.

library(ggcorrplot)
library(tidyverse)
library(broom)
library(ggbiplot)
library(factoextra)

data(wine)

ggcorrplot(round(cor(wine), 2), method = "circle", hc.order = TRUE)

wine.pca <- prcomp(wine, scale. = TRUE)

ggbiplot(wine.pca, 
         obs.scale = 1, 
         var.scale = 1,
         groups = wine.class, 
         ellipse = TRUE, 
         circle = TRUE) +
    scale_color_discrete(name = '') +
    theme(legend.direction = 'horizontal', 
          legend.position = 'top')

##################################### Eigenvalues ###############################################
#
# Eigenvalues
#
#************************************************************************************************

# An eigenvalue > 1 indicates that PCs account for more variance than accounted 
# by one of the original variables in standardized data. This is commonly used 
# as a cutoff point for which PCs are retained. This holds true only when the data are standardized.

eig.val <- get_eigenvalue(wine.pca)
eig.val

fviz_eig(wine.pca, choice = "eigenvalue", addlabels = TRUE)
fviz_eig(wine.pca, choice = "variance", addlabels = TRUE) #default

##################################### IND #######################################################
#
# Results for individuals
#
#************************************************************************************************

wine.ind <- get_pca_ind(wine.pca)
wine.ind$coord          # Coordinates
wine.ind$contrib        # Contributions to the PCs
wine.ind$cos2           # Quality of representation 

fviz_pca_ind(wine.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(wine.pca, 
             pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(wine.pca, 
             col.ind = "cos2", 
             pointsize = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_cos2(wine.pca, choice = "ind")

# Individuals on dimensions 2 and 3
fviz_pca_ind(wine.pca, 
             axes = c(2, 3),
             col.ind = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Total contribution on PC1 and PC2
fviz_contrib(wine.pca, choice = "ind", axes = 1:2)

set.seed(123)
# k-means on all 13 dimensions
res.km.ind <- kmeans(wine.ind$coord, centers = 3, nstart = 25)
groups <- as.factor(res.km.ind$cluster)
fviz_pca_ind(wine.pca,
             geom = "point",   # points w/o labels
             col.ind = groups, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",  # for the centroids
             legend.title = "Groups",
             repel = TRUE
)

fviz_pca_ind(wine.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "norm",
             legend.title = "Groups",
             repel = TRUE
)

fviz_pca_ind(wine.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "convex",
             legend.title = "Groups",
             repel = TRUE
)

fviz_pca_ind(wine.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "t",
             legend.title = "Groups",
             repel = TRUE
)

fviz_pca_ind(wine.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "norm",
             ellipse.level = 0.95,
             legend.title = "Groups",
             repel = TRUE
)

fviz_pca_ind(wine.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "norm",
             ellipse.level = 0.66,
             legend.title = "Groups",
             repel = TRUE
)

fviz_pca_ind(wine.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "euclid",
             legend.title = "Groups",
             repel = TRUE
) + coord_fixed()


##################################### VAR #######################################################
#
# Results for Variables
#
#************************************************************************************************

wine.var <- get_pca_var(wine.pca)
wine.var$coord          # Coordinates
wine.var$contrib        # Contributions to the PCs
wine.var$cos2           # Quality of representation 

# Plot coordinates of variables anchored at (0,0)
fviz_pca_var(wine.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(wine.pca,
             axes = 2:3,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(wine.pca,
             axes = 12:13,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


library("corrplot")
corrplot(wine.var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
#
# A high cos2 indicates a good representation of the variable on the principal 
# component. In this case the variable is positioned close to the circumference 
# of the correlation circle.
# 
# A low cos2 indicates that the variable is not perfectly represented by the PCs.
# In this case the variable is close to the center of the circle.
#
fviz_cos2(wine.pca, choice = "var", axes = 1:2)

# The cos2 values are used to estimate the quality of the representation
# The closer a variable is to the circle of correlations, the better its representation 
# on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the center of the plot are less important for the first components.

# Color by cos2 values: quality on the factor map
fviz_pca_var(wine.pca, 
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

corrplot(wine.var$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(wine.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(wine.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC13
fviz_contrib(wine.pca, choice = "var", axes = 13, top = 10)

# The total contribution of a given variable, on explaining the variations 
# retained by two principal components, say PC1 and PC2, is calculated as 
# contrib = [(C1 * Eig1) + (C2 * Eig2)]/(Eig1 + Eig2), where
# C1 and C2 are the contributions of the variable on PC1 and PC2, respectively
# Eig1 and Eig2 are the eigenvalues of PC1 and PC2, respectively. Recall that 
# eigenvalues measure the amount of variation retained by each PC.

fviz_contrib(wine.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(wine.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
# clustering on all 13 dimensions
res.km <- kmeans(wine.var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(wine.pca, 
             col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")


##################################### IND and VAR ###############################################
#
# Individuals and vars together
#
#************************************************************************************************


fviz_pca_biplot(wine.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_biplot(wine.pca, 
                repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = groups, # color by groups
                palette = c("#00AFBB", "#E7B800", "#FC4E07")
)

fviz_pca_biplot(wine.pca, 
                col.ind = groups, 
                palette = "jco", 
                addEllipses = TRUE, 
                label = "var",       # means only variables will be labelled
                col.var = "black",
                repel = TRUE,
                legend.title = "Cluster",
                title = "PC1 and PC2") 

fviz_pca_biplot(wine.pca, 
                axes = c(2,3),
                col.ind = groups, 
                palette = "jco", 
                addEllipses = TRUE, 
                label = "var",       # means only variables will be labelled
                col.var = "black",
                repel = TRUE,
                legend.title = "Cluster",
                title = "PC2 and PC3") 

fviz_pca_biplot(wine.pca, 
                # Fill individuals by groups
                geom.ind = "point",         # point w/o text label
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = groups,
                col.ind = "black",
                # Color variable by groups
                col.var = grp,
                legend.title = list(fill = "Ind cluster", 
                                    color = "Var cluster"),
                repel = TRUE        # Avoid label overplotting
) + ggpubr::fill_palette("jco") +   # Indiviual fill color
    ggpubr::color_palette("jco")    # Variable colors # was "npg"

# More complex biplot formatting example

fviz_pca_biplot(wine.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = groups, 
                col.ind = "black",
                pointshape = 21, 
                pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", 
                col.var = "contrib",
                gradient.cols = "RdYlBu",
                legend.title = list(fill = "Ind cluster",
                                    color = "Contrib",
                                    alpha = "Contrib")
)

################################## IND and VAR clusters #########################################
#
# Individuals clustered and vars clustered together
# Understanding the properties of wine groups through their average values of metrics
# individually or in metrics clusters
#
#************************************************************************************************

# Wines in cluster 3 are characterized by the lowest values of variables in var cluster 3
# Likewise wines from cluster 1 have highest values of variables from var cluster 1
# And wines from cluster 2 have highest values of variables from cluster 2 and at the same
# time the lowest values of variables from cluster 1

wine.clust <- res.km.ind %>% augment(wine)

wine.clust %>% group_by(.cluster) %>% dplyr::summarise(mean(Alcohol))
wine.clust %>% group_by(.cluster) %>% dplyr::summarise(mean(Mg))
wine.clust %>% group_by(.cluster) %>% dplyr::summarise(mean(Proline))
wine.clust %>% group_by(.cluster) %>% dplyr::summarise(mean(Color))

wine.var.clust <- wine.clust %>% 
    gather(key = "Metric", value = "value", -.cluster) %>% 
    left_join(data_frame(Metric = names(res.km$cluster), VarCluster = res.km$cluster)) %>% 
    mutate(Metric = factor(Metric))

library(RColorBrewer)
colset <- brewer.pal(range(wine.var.clust$VarCluster)[2], "Dark2")

col <- colset[wine.var.clust %>% 
                  select(Metric, VarCluster) %>%  
                  group_by(Metric) %>% 
                  dplyr::summarise(col = first(VarCluster)) %>% 
                  arrange(col) %>% 
                  select(col) %>% 
                  unlist()]

wine.var.clust %>% 
    group_by(Metric, .cluster) %>%
    dplyr::summarise(avgValue = mean(value)) %>% 
    ungroup() %>% 
    # rejoin VarCluster, needed for reordering of metrics
    left_join(wine.var.clust %>% select(Metric, VarCluster) %>%  group_by(Metric) %>% dplyr::summarise(VarCluster = first(VarCluster))) %>% 
    arrange(Metric, avgValue) %>% 
    group_by(Metric) %>% 
    dplyr::mutate(rank = rank(avgValue, ties.method = "average")) %>%   # rank wine clusters by their average value of a metric, within each metric
    ggplot(aes(x = reorder(Metric, VarCluster), y = factor(rank), color = .cluster, group = .cluster)) +
    geom_line(size = 2) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = col)) +
    ggtitle("Average value of a metric for wine clusters") +
    scale_y_discrete(labels = c("Lowest","Medium","Highest")) +
    scale_color_discrete(name = "Wine\ncluster") +
    xlab("Metric") +
    ylab("")
    
# The following chart reveals principal differencies between groups of wines, uncovered
# by PCA analysis.
# Wines in cluster 1 have the highest average value of metrics from metrics cluster 1 and the lowest
# from metrics cluster 2.
# Wines in cluster 2 have the highest average value of metrics from metrics cluster 2 and 3 and the lowest
# from metrics cluster 1.
# Wines in cluster 3 have the lowest average metric from metrics cluster 3.

wine.var.clust %>% 
    group_by(VarCluster, .cluster) %>%
    dplyr::summarise(avgValue = mean(value)) %>% 
    ungroup() %>% 
    group_by(VarCluster) %>% 
    dplyr::mutate(rank = rank(avgValue, ties.method = "average")) %>%   # rank wine clusters by their average value of all metrics in a cluster, within each metric cluster 
    ggplot(aes(x = VarCluster, y = factor(rank), color = .cluster, group = .cluster)) +
    geom_line(size = 2) +
    scale_x_continuous(breaks=c(1,2,3)) +
    scale_y_discrete(labels = c("Lowest","Medium","Highest")) +
    scale_color_discrete(name = "Wine\ncluster") +
    xlab("Metrics cluster") +
    ylab("") + 
    ggtitle("Average value of a metric within metric cluster for wine clusters")


##################################### Manual calc ###############################################
#
# Computing coord, cos2 and contribution by hand
#
#************************************************************************************************


# From http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

var_coord_func <- function(loadings, comp.sdev){
    loadings*comp.sdev
}

# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
# var.coord = loadings * the component standard deviations
#
loadings <- wine.pca$rotation
sdev <- wine.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
var.coord

# Compute Cos2
#::::::::::::::::::::::::::::::::::::::::
# var.cos2 = var.coord^2
#
var.cos2 <- var.coord^2
var.cos2

# Compute contributions
#::::::::::::::::::::::::::::::::::::::::
#var.contrib. The contribution of a variable to a given principal component is (in percentage) :
# (var.cos2 * 100) / (total cos2 of the component)
#
comp.cos2 <- apply(var.cos2, 2, sum)

contrib <- function(var.cos2, comp.cos2) {
    var.cos2*100/comp.cos2
}

var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
var.contrib
range(wine.pca$rotation)
0
# Coordinates of individuals
#::::::::::::::::::::::::::::::::::
#
ind.coord <- wine.pca$x
ind.coord

# Cos2 of individuals
#:::::::::::::::::::::::::::::::::
# 1. square of the distance between an individual and the
# PCA center of gravity

center <- wine.pca$center
scale<- wine.pca$scale
getdistance <- function(ind_row, center, scale){
    return(sum(((ind_row-center)/scale)^2))
}

d2 <- apply(wine,1,getdistance, center, scale)
# 2. Compute the cos2. The sum of each row is 1
cos2 <- function(ind.coord, d2){
    return(ind.coord^2/d2)
}
ind.cos2 <- apply(ind.coord, 2, cos2, d2)
ind.cos2

# Contributions of individuals
#:::::::::::::::::::::::::::::::
#
contrib <- function(ind.coord, comp.sdev, n.ind){
    100*(1/n.ind)*ind.coord^2/comp.sdev^2
}
ind.contrib <- t(apply(ind.coord, 1, contrib, 
                       wine.pca$sdev, nrow(ind.coord)))
ind.contrib
