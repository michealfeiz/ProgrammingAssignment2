## Functions for creating and using inverted matrices which caching ability
## Using solve() function to calculate the inverse of a matrix. 
## with using R's scoping we are able to cach the result.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Define function to set the value of the matrix. It also clears the old
        # inverse from the cache
        set <- function(y) {
                x <<- y # Set the value
                m <<- NULL # Clear the cache
        }
        # Define function to get the value of the matrix
        get <- function() x
        # Define function to set the inverse.
        setsolve <- function(solve) m <<- solve
        # Define function to get the inverse
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#' Return inverse of matrix x
#'
#' This function computes the inverse of the special "matrix" returned by
#' makeCacheMatrix above. If the inverse has already been calculated
#' (and the matrix has not changed), then the cachesolve retrieves the
#' inverse from the cache.
#' 
cacheSolve <- function(x, ...) {
        m <- x$getsolve() 
        
        if(!is.null(m)) { # If the cache was not empty, we can just return it
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()       
        m <- solve(data, ...) 
        x$setsolve(m)	      
        m # Return the inverse		      
}