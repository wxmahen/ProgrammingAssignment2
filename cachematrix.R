## A pair of functions to cache the inverse of a matrix

## This creates a matrix object that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setMatrixInv <- function(inv) m <<- inv
    getMatrixInv <- function() m
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setMatrixInv = setMatrixInv,
         getMatrixInv = getMatrixInv)
}


## This constructs the inverse of the matrix x or retrieves the cache if available
cacheSolve <- function(x, ...) {
    m <- x$getMatrixInv()
    if(!is.null(m)) {
        message("getting cached matrix inverse")
        return(m)
    }
    m1 <- x$getMatrix()
    m <- solve(m1, ...)
    x$setMatrixInv(m)
    m
}
