## Together, these functions take input of an invertible matrix,
## check if the inverse has been calculated, 
## if it has, prints the inverted matrix from cache 
## if it hasn't, calculates, caches and prints the inverted matrix

## sets the value of "matrx" to NULL, sets the value of x as the input matrix,
## sets value of "get" to x, 
## creates functions to solve and cache the inverted matrix 
## when "matrx" is not NULL.

makeCacheMatrix <- function(x = matrix()){
        matrx <- NULL
        set <- function(y) {
                x <<- y
                matrx <<- NULL
        }
        get <- function() x
        setinv <- function(solve) matrx <<- solve
        getinv <- function() matrx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## checks if the inverted matrix has been cached, 
## if yes, returns the inverted matrix
## if no, gets the original matrix, calculates the inverse, caches it,
## and returns the inverted matrix.

cacheSolve <- function(x, ...) {
        matrx <- x$getinv()
        if(!is.null(matrx)) {
                message("getting cached data")
                return(matrx)
        }
        data <- x$get()
        matrx <- solve(data, ...)
        x$setinv(matrx)
        matrx
}
