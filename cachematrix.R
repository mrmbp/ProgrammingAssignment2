## There are two functions here, makeCacheMatrix and cacheSolve. The first function 
#creates an invertible matrix and stores it and cacheSolve returns its mean.


## This function creates an invertible square matrix and uses the <<- operator to store its inverse.

makeCacheMatrix <- function(x = matrix()) { #create matrix x
        set <- function(y) {
        x <<- y
        m <<- NULL
}
get <- function() x
setmatrix <- function(mean) m <<- mean 
getmatrix <- function() m
list(set = set, get = get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
}


## This function checks the stored matrix to see if it has already been solved, in which case
## it returns the solution; otherwise, it returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
