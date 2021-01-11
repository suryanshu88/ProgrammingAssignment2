## makeCacheMatrix function creates and caches an inverse of a matrix object 
## If the Inverse of a matrix is already cached it supports retrival of the 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinverse <- function(m) inv <<- m
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve fucntion computes the inverse of a matrix
## If the inverse is already computed, it retrives the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
