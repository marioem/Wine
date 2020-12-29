library(tidyverse)
library(broom)

winecsv <- read.table("data/wine.data", sep = ",")
winecsv <- winecsv %>% 
    mutate(V1 = as.factor(V1))
           
V2stats <- winecsv %>% 
    mutate(mean = mean(V2)) %>% 
    group_by(mean, V1) %>% 
    select(mean, V1, V2) %>% 
    dplyr::summarise(V2grpmean = mean(V2), V2grpvar = var(V2), n = n())

V2withinvar <- anova(lm(V2~V1, data = winecsv))[2,3]
# or sum(V2stats$V2grpvar * (V2stats$n - 1)) / (sum(V2stats$n) - nrow(V2stats))

V2betweenvar <- sum((V2stats$V2grpmean - V2stats$mean)^2 * V2stats$n) / (nrow(V2stats) - 1)
# or anova(lm(V2~V1, data = winecsv))[1,3]

winestd <- winecsv %>% 
    mutate_at(vars(V2:V14), base::scale)

winepca <- prcomp(winestd[2:14])
winepccomp <- cbind(winepca$x, winecsv$V1)
winepccomp <- as.data.frame(winepccomp)

winepccomp %>% ggplot(aes(x = PC1, y = PC2, col = as.factor(V14))) + geom_point() # V14 here is from winecsv$V1
winepccomp %>% ggplot(aes(x = PC3, y = PC2, col = as.factor(V14))) + geom_point() # V14 here is from winecsv$V1
winepccomp %>% ggplot(aes(x = PC3, y = PC4, col = as.factor(V14))) + geom_point() # V14 here is from winecsv$V1

library("MASS")
wine.lda <- lda(winecsv$V1 ~ winecsv$V2 + winecsv$V3 + winecsv$V4 + winecsv$V5 + winecsv$V6 + 
                    winecsv$V7 + winecsv$V8 + winecsv$V9 + winecsv$V10 + winecsv$V11 + winecsv$V12 +
                    winecsv$V13 +winecsv$V14)

# Variance explained
var.expl <- wine.lda$svd^2/sum(wine.lda$svd^2)
var.expl

wine.lda.values <- predict(wine.lda, winecsv[2:14])

wine.lda.values$x <- cbind(as.data.frame(wine.lda.values$x), class = winecsv$V1)

# Separation with two highest separating original variables
winecsv %>% ggplot(aes(V8, V14, color = V1)) + geom_point()

# separation with LDA
wine.lda.values$x %>% ggplot(aes(LD1, LD2, color = class)) + geom_point()

# Value-based separation based on best vars of raw data
winecsv %>% ggplot(aes(V8, fill = V1)) + geom_density(alpha = .5)
winecsv %>% ggplot(aes(V14, fill = V1)) + geom_density(alpha = .5)

# Value-based separation based on linear discriminants
wine.lda.values$x %>% ggplot(aes(LD1, fill = class)) + geom_density(alpha = .5)
wine.lda.values$x %>% ggplot(aes(LD2, fill = class)) + geom_density(alpha = .5)

kmeans <- kmeans(wine.lda.values$x[,1:2], centers = 3)

kmeans %>% 
    augment(wine.lda.values$x) %>% 
    ggplot(aes(LD1, LD2, color = .cluster)) + geom_point(aes(shape = class)) +
    scale_color_discrete(name = "Cluster")

kmeans.all <- kmeans(wine.lda.values$x, centers = 3)

kmeans.all %>% 
    augment(wine.lda.values$x) %>% 
    ggplot(aes(LD1, LD2, color = .cluster)) + geom_point(aes(shape = class)) +
    scale_color_discrete(name = "Cluster")

