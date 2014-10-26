# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than computing it repeatedly
# (there are also alternatives to matrix inversion that we will not discuss here). 
# This pair of functions cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        xinv <- NULL 
        set <- function(y) {
                x <<- y
                xinv <<- NULL 
        }
        
        get <- function() x 
        setInv <- function(inv) xinv <<- inv 
        getInv <- function() xinv 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}




## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv() # get the inversed matrix from object x
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m) 
        }
        data <- x$get() 
        m <- solve(data) 
        x$setInv(m) 
        m 
}








