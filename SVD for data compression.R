library(imager)

x <- load.image("data/pansy.jpg")
plot(x)

r <- grayscale(x)
plot(r)

r.gr <- as.matrix(r)
plot(as.cimg(r.gr))

r.svd <- svd(r.gr)
d <- diag(r.svd$d)
dim(d)

u <- r.svd$u
v <- r.svd$v
plot(1:length(r.svd$d), r.svd$d)

u1 <- as.matrix(u[, 1])
v1 <- as.matrix(v[, 1])
d1 <- as.matrix(d[1, 1])
l1 <- u1 %*% d1 %*% t(v1)
plot(as.cimg(l1))

# more approximation
depth <- 5
us <- as.matrix(u[, 1:depth])
vs <- as.matrix(v[, 1:depth])
ds <- as.matrix(d[1:depth, 1:depth])
ls <- us %*% ds %*% t(vs)
plot(as.cimg(ls))

# more approximation
depth <- 10
ut <- as.matrix(u[, 1:depth])
vt <- as.matrix(v[, 1:depth])
dt <- as.matrix(d[1:depth, 1:depth])
lt <- ut %*% dt %*% t(vt)
plot(as.cimg(lt))

# more approximation
depth <- 100
uh <- as.matrix(u[, 1:depth])
vh <- as.matrix(v[, 1:depth])
dh <- as.matrix(d[1:depth, 1:depth])
lh <- uh %*% dh %*% t(vh)
plot(as.cimg(lh))

# This is how this image looks like in the "principal components" (kinda) space
newdim <- uh %*% dh
plot(as.cimg(newdim))

