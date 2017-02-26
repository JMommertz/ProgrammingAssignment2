## Script for showing the use of cached values with the superassignment pattern <<-

## How to check the function:
##
## > testVector <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## > cacheSolve(testVector)
##
## Sample Output:
##
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## This function creates a special "matrix" object, that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## preset invMatrix to NULL for further computing
        invMatrix <- NULL
        
        ## define internal functions for CacheMatrix
        get <- function() x
        setInverse <- function(Invers) invMatrix <<- Invers
        getInverse <- function() invMatrix
        
        ## create special vector with three elements resp. functions
        list(get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        ## get the inversed matrix saved in cache
        i <- x$getInverse()
        
        ## if not null is returned, the value is allready cached
        if(!is.null(i)) return(i)
        
        ## if null is returned, it has to be computed
        data <- x$get()
        i <- solve(data)
        
        ## save inversed matrix to cache
        x$setInverse(i)
        
        ## Return a matrix i that is the inverse of 'x'
        i
}
