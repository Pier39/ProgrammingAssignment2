## This module calculates the inverse of a matrix, using a cache to save
## computation resources.
## Usage:
## 	cc <- makeCacheMatrix(x)
##	cacheSolve(cc)
## where x is an invertible matrix, e.g: x <- matrix(1:4, 2, 2)
## the next calls of cacheSolve will get inverse the from cache


## makeCacheMatrix is the compound object that associates a matrix and its cache

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setInv <- function(inverse)
        inv <<- inverse
    getInv <- function()
        inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve access the matrix-cache compound object and return the inverse if
#  already there; when the cache is empty, the inverse is calculated

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv))
    {
        message("getting cache data")
        return(inv)
    }
    # if inv is null, the inverse has to be calculated
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setInv(inv)
    
    inv
}
