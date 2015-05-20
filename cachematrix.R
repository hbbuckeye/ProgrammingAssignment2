## makecacheMatrix creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix computes the inverse of the special "matrix" returned by 
##makeCacheMatrix. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the 
##inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set<-function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
                
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getInverse()
        
        ##Check if the value of inv is not NULL, get the value from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##Otherwise calculate the inverse by using matrix x
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
}
