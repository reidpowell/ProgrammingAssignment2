## These functions will be used to calculate and cache the inverse of a matrix.
## A critical assumption of these functions is that the supplied matrix is
## invertible.

## The following function, makeCacheMatrix, creates a "special matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	# Initialize cache
	matrixInverse <- NULL

	# setter function
	set <- function(y) {
		# store the input
		x <<- y

		# reset the cache (to allow it to be recalculated when needed)
		matrixInverse <<- NULL
	}

	# getter function, which returns the matrix used to construct this object
	get <- function() x

	# setter function for the calculated inverse of the input matrix
	setInverse <- function(inverse) matrixInverse <<- inverse

	# getter function for calculated inverse of input matrix
	getInverse <- function() matrixInverse

	# return "special matrix"
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## The following function, cacheSolve, computes the inverse of the "special
## matrix" returned by makeCache matrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # fetch the inverse from the "special matrix"
        # if it exsists return it...
        matrixInverse <- x$getInverse()
        if(!is.null(matrixInverse)) {
        	message("getting cached data")
        	return(matrixInverse)
        }

        # ...otherwise it does not exist and we must calculate it
        data <- x$get()
        matrixInverse <- solve(data)
        x$setInverse(matrixInverse)
        matrixInverse
}
