## [Calculate and cache inverse of matricies]
## makeCacheMatrix and cacheSolve work cooperatively to show,
## set, show the inverse, set the inverse, and retrieve a
## cached inverse.

## makeCacheMatrix takes one argument (x), a matrix
## and creates a set of four functions that allow you to view,
## set, view the inverse of, and set the inverse of the passed
## matrix (x).

makeCacheMatrix <- function(x = matrix()) {
        mtrx_inverse <- NULL      ## Initializes inverse var for this FUN
        mtrx_set <- function(mtrx_new) {      ## Used to assign new matrix to x
                x <<- mtrx_new
                mtrx_inverse <<- NULL
        }
        mtrx_get <- function() {x}      ## Used to view contents of x
        mtrx_setinverse <- function(mtrx_newinverse) {  ## Maually set inverse
                mtrx_inverse <<- mtrx_newinverse
        }
        mtrx_getinverse <- function() {mtrx_inverse}  ## View a cached inverse
        list(mtrx_set = mtrx_set, mtrx_get = mtrx_get,
            mtrx_setinverse = mtrx_setinverse,
            mtrx_getinverse = mtrx_getinverse)
}


## cacheSolve is passed a list of functions from makeCacheMatrix
## and uses them to either compute the inverse of the passed
## matrix (x) or retrieve a previously calculated inverse.

cacheSolve <- function(x, ...) {
        mtrx_inverse <- x$mtrx_getinverse()  ## Initializes inverse var for this FUN
        if(!is.null(mtrx_inverse)) {  ## Checks if inverse was previously calculated
                message("getting cached inverse")
                return(mtrx_inverse)
        }
        data <- x$mtrx_get()
        mtrx_inverse <- solve(data, ...)  ## If no cached inverse, inverse is calc'ed
        x$mtrx_setinverse(mtrx_inverse)
        mtrx_inverse
}
