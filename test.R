# create a random matrix
dimension <- 500
test_matrix <- matrix(rexp(dimension*dimension, rate=.1), ncol=dimension)

# time of inverse calculation
# Time difference of 0.2326021 secs
start_time <- Sys.time(); temp <- solve(test_matrix); Sys.time() - start_time;

# create "special matrix"
test_special_matrix <- test_special_matrix <- makeCacheMatrix(test_matrix)

# time first inverse calculation
# Time difference of 0.234868 secs
start_time <- Sys.time(); temp <- cacheSolve(test_special_matrix); Sys.time() - start_time;

# Repeat and time second inverse "calculation"
# getting cached data
# Time difference of 0.0007619858 secs
start_time <- Sys.time(); temp <- cacheSolve(test_special_matrix); Sys.time() - start_time;

# matrix comparison function from https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y) is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

# quick correctness check
matequal(solve(test_matrix),cacheSolve(test_special_matrix))