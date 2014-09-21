## This module calculates the inverse of a matrix, using a cache to save
## computation resources.
## Usage:
## 	cc <- makeCacheMatrix(x)
##	cacheSolve(cc)
## where x is an invertible matrix, e.g: x <- matrix(1:4, 2, 2)
## the next calls of cacheSolve will get inverse from the cache.


## makeCacheMatrix is the compound object that associates a matrix
## and its inverse (if already calculated)
makeCacheMatrix <- function(x = matrix())
{
    # empty inverse
    inv <- NULL
    # function that stores a new compound object (inverse not calculated yet)
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    # function that returns the matrix itself
    get <- function()
        x
    # function that stores the inverse in cache
    setInv <- function(inverse)
        inv <<- inverse
    # function that returns the inverse from cache
    getInv <- function()
        inv
    # return the list of inner functions with their names
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve access the matrix-cache compound object and return the inverse if
#  already there; when the cache is empty, the inverse is calculated

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    # test the returned inverse; if it is not null, inverse was already
    # calculated and stored in the cache
    if (!is.null(inv))
    {
        message("getting cache data")
        # return the inverse from cache
        return(inv)
    }
    # if inv (inverse from the cache) is null, the inverse has to be calculated
    matr <- x$get()
    inv <- solve(matr, ...)
    # store the inverse in the cache
    x$setInv(inv)
    
    # return the inverse calculated
    inv
}
