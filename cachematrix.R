##cachematrix takes an invertible square matrix and creates a cache of the inverse.
##The goal is to save time in performing large calculations over and over.

## makeCacheMatrix creates a list of functions that can be called uisng $notation to get and set the cached matrix

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL                                 ##initialize the variable mat
    set <- function(y) {                        ##set the matrix and delete the existing cached inverse
        x <<- y
        mat <<- NULL
    }
    get <- function() x                         ##return the matrix
    setinv <- function(solve) mat <<- solve     ##set the inverse function from a function call from cacheSolve
    getinv <- function() mat                    ##return the inverse matrix
    list(set = set, get = get, setinv  = setinv, getinv = getinv)   ##return the list of functions defined above
}


## cacheSolve actually does the legwork of checking to see whether or not an inverse has been calculated
## and then either returns the cached value or calculates and stores the inverse of the matrix.
## cacheSolve takes as an argument a list created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    mat <- x$getinv()                           ##Attempt to get a saved inverted matrix without doing any calculation
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)                             ##Return the cached matrix if it exists.
    }
    data <- x$get()                             ##If a cached matrix does not exist, get the original matrix.
    mat <- solve(data)                          ##Create the inverse matrix
    x$setinv(mat)                               ##Store the inverse matrix in the supplied list using the setinv function
    mat                                         ## Return a matrix that is the inverse of 'x'
}
