# File: cachematrix.R
# Date: 2015-08-11T14:12:40A
# Author: richard.zijdeman@iisg.nl
# Last change: -

## This file <cachematrix.R> contains two functions: 'makeCacheMatrix()' and 
## 'cacheSolve()' that combinedly cache the inverse of a matrix in order to 
## save considerable computing time as inversing matrices can be intensive 
## operations.

## The makeCacheMatrix() function requires a matrix as input and returns a list 
## of function required to decide whether to calculate the inverse matrix
## or get it from cache.

makeCacheMatrix <- function(x = matrix()) { # fun with matrix as argument
    s <- NULL                               # create / clear object 's' within
                                            # makeCacheMatrix()
    set <- function(y) {                    # define fun with 'y' as argument
        x <<- y                             # updates x in higher env. with y
        s <<- NULL                          # clears s in higher environment
    }
    
    get <- function() x                         # get function returns x 
    set.inverse <- function(solve) s <<- solve  # update s in higher env
    get.inverse <- function() s                 # get.inverse() returns s
    
    list(set = set, get = get,              # construct and print list of
         set.inverse = set.inverse,         # values for set, get,
         get.inverse = get.inverse)         # set.inverse and get.inverse
}


## The cacheSolve() function requires an object that is the result of the
## makeCacheMatrix. cacheSolve returns the inverse of the matrix specified
## in the MakeCacheMatrix() function, by computing it on the first call
## or getting it from cache on subsequent calls

cacheSolve <- function(x, ...) {        # create func. cacheSolve()
    s <- x$get.inverse()                # set s to the value of get.inverse
                                        # from x as defined by makeCacheMatrix()
    if(!is.null(s)) {                   # if get.inverse holds a value, then:
        message("getting cached data")  # print that cached data are retrieved
        return(s)                       # output cached data and exit
    }                                       
                                # continue if no cached data were available:
    matrix <- x$get()           # obtain matrix provided to MakeCacheMatrix()
    s <- solve(matrix, ...)     # inverse that matrix and store it to 's'
    x$set.inverse(s)            # cache inversed matrix
    
    s                           # output the inversed matrix
}

## Example (uncomment to run)
# mm <- matrix(runif(9), nrow = 3)  # define 2x2 by matrix
# mmC <- makeCacheMatrix(mm)        # set/get inverse of matrix
# cacheSolve(mmC)                   # first time: calculate inverse of matrix
# cacheSolve(mmC)                   # subsequent times: retrieve inverse matrix