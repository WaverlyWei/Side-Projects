# set the number of cores to use manually
nCores <- 8  
# register cores
registerDoParallel(nCores) 
# number of iterations
N <- 100
#number of k
kmax <- 10
# size of subset
m <- 0.8

# Function of parallely computing similarity measures 
# outer loop: number of k's
# inner loop: number of iterations 
result <- foreach(k = 2:kmax,.combine = cbind) %dopar% {
  foreach(i = 1:N,.combine=c) %dopar% {
    # indexing the job
    cat('Starting ', k, "&",i, 'th job.\n', sep = '')
    # create subsample 1 from original data 
    sub1 <- dat[sample(nrow(dat),m*nrow(dat)),]
    # create subsample 2 from original data 
    sub2 <- dat[sample(nrow(dat),m*nrow(dat)),]
    # cluter subsample 1
    L1 <- kmeans(sub1,k)
    # cluster subsample 2
    L2 <- kmeans(sub2,k)
    # get the index of subsample 1
    clust.1 <- L1$cluster
    # get the index of subsample 2
    clust.2 <-L2$cluster
    # find the intersect of two subsamples
    intersect <- intersect(rownames(sub1),rownames(sub2))
    # find the intersection that belongs to subsample 1
    L1.intersect <- clust.1[intersect]
    # find the intetsection that belongs to subsample 2
    L2.intersect <- clust.2[intersect]
    # pass two list of indices to similarity measure computation 
    output <- similarity(L1.intersect,L2.intersect,length(L1.intersect))
    # index finishing job
    cat('Finishing ', k,"&",i, 'th job.\n', sep = '')
    output # this will become part of the out object
  }
}

# save results to cluster 
write.csv(result, "results_foreach.csv")