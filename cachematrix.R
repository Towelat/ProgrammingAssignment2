
## Below are a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. 
## This special "matrix", is really a list containing function calls to functions defined within the function.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y = matrix()) {
            x <<- y
            m <<- NULL
        }
        
        get <- function(){x
        } 
        
        setinverse <- function(solve){m <<- solve
        }
        
        getinverse <- function(){m
        } 
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinverse(m)
    m
}



