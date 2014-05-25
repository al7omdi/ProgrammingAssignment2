## Creating functions that would utilize chaching capability to 
## inverse the matrix.

## This function creates a special "matrix" object that chaches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) Inv <<- Inverse
        getInverse <- function() Inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This would compute the inverse of the matrix returned by the makeCacheMatrix
## above. If the inverse has alrady been calculated, the "cacheSolve" should 
## recall the inverse from chache.

cacheSolve <- function(x, ...) {
     
        ## Return a matrix that is the inverse of 'x'
        
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
}
