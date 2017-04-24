
cacheSolve <- function(X) 
## Returns inverse matrix either calculated or from cache     
## X - object returned by makeCacheMatrix()
{
    M <- X$get_inverse()
    if(!is.null(M)) {
        message("Inverse matrix fom cache")
    }
    else {
        message("Inverse matrix fom solve()")
        M <- solve(X$get())
        X$set_inverse(M)
    }
    M
}

makeCacheMatrix <- function(M = matrix()) 
## Constructor of an object containing a matrix, it's cached inverse, 
## and set/get methods for both
{   M_INV <- NULL
    
    ## Method initiating matrix data
    set <- function(x) {
        M <<- x
        M_INV <<- NULL
    }
    
    ## Method returning matrix data
    get <- function() M
    
    ## Initiate Inversed matrix data
    set_inverse <- function(m_inv) M_INV <<- m_inv
    
    ## Method returning initiated matrix data
    get_inverse <- function() M_INV
    
    ## Return the constructed object as a list
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}