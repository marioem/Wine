library(tidyverse)
library(broom)

wine <- read.table("data/wine.data", sep = ",")
wine <- wine %>% 
    mutate(V1 = as.factor(V1))
           
V2stats <- wine %>% 
    mutate(mean = mean(V2)) %>% 
    group_by(mean, V1) %>% 
    select(mean, V1, V2) %>% 
    summarise(V2grpmean = mean(V2), V2grpvar = var(V2), n = n())

V2withinvar <- anova(lm(V2~V1, data = wine))[2,3]
# or sum(V2stats$V2grpvar * (V2stats$n - 1)) / (sum(V2stats$n) - nrow(V2stats))

V2betweenvar <- sum((V2stats$V2grpmean - V2stats$mean)^2 * V2stats$n) / (nrow(V2stats) - 1)
# or anova(lm(V2~V1, data = wine))[1,3]

winestd <- wine %>% 
    mutate_at(vars(V2:V14), scale)

winepca <- prcomp(winestd[2:14])

winepccomp %>% ggplot(aes(x = PC1, y = PC2, col = V14)) + geom_point()
winepccomp %>% ggplot(aes(x = PC1, y = PC2, col = as.factor(V14))) + geom_point()
winepccomp %>% ggplot(aes(x = PC3, y = PC2, col = as.factor(V14))) + geom_point()
winepccomp %>% ggplot(aes(x = PC3, y = PC4, col = as.factor(V14))) + geom_point()

library("MASS")
wine.lda <- lda(wine$V1 ~ wine$V2 + wine$V3 + wine$V4 + wine$V5 + wine$V6 + 
                    wine$V7 + wine$V8 + wine$V9 + wine$V10 + wine$V11 + wine$V12 +
                    wine$V13 +wine$V14)

wine.lda.values <- predict(wine.lda, wine[2:14])

wine.lda.values$x <- cbind(as.data.frame(wine.lda.values$x), class = wine$V1)

# Separation with two highest separating original variables
wine %>% ggplot(aes(V8, V14, color = V1)) + geom_point()

# separation with LDA
wine.lda.values$x %>% ggplot(aes(LD1, LD2, color = class)) + geom_point()

# Value-based separation based on best vars of raw data
wine %>% ggplot(aes(V8, fill = V1)) + geom_density(alpha = .5)
wine %>% ggplot(aes(V14, fill = V1)) + geom_density(alpha = .5)

# Value-based separation based on linear discriminants
wine.lda.values$x %>% ggplot(aes(LD1, fill = class)) + geom_density(alpha = .5)
wine.lda.values$x %>% ggplot(aes(LD2, fill = class)) + geom_density(alpha = .5)

kmeans <- kmeans(wine.lda.values$x[,1:2], centers = 3)

kmeans %>% 
    augment(wine.lda.values$x) %>% 
    ggplot(aes(LD1, LD2, color = .cluster)) + geom_point(aes(shape = class)) +
    scale_color_discrete(name = "Cluster")
