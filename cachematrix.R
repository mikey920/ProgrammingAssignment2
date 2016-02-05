## makeCacheMatrix creates a special matrix, which is
## really a list containing a function that
## 1.  Sets the contents of the matrix
## 2.  Gets the contents of the matrix
## 3.  Sets the contents of the inverse matrix
## 4.  Gets the contents of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix created
## in makeCacheMatrix if the inverse isn't already cached.  First,
## the function checks to see if the inverse is cached.  If so, 
## the inverse is pulled from cache and is not calculated.  Else, it
## calculates the inverse and saves it to cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
