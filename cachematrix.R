## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
makeCasheMatrix <- function (x = matrix()) {
    inv1 <- NULL
    set <- function (y){
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    set_inv <- function(inv) set_inv <<- inv1
        get_inv <- function () inv
    list(set= set, get= get, set_inv = set_inv, get_inv = get_inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
    inv1 <- x$ get_inv ()
    if (!is.null(inv1)){
        message ("getting cashed data")
        return(inv1)
    }
    data <- x$get()
    inv <- solve (data)
    x$set_inv (inv)
    inv
}