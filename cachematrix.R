## This assignment (and following code), teaches the benefits of caching.
## Caching allows you to store data and retrieve it quickly to save time 
## and memory. If the data is used repeatedly, and the matrix is quite 
## large, the time to recreate the data each instance may be quite time 
## consuming. By caching the data in the parent environment it is quite 
## easy and quick to return the data. The two functions below work together 
## to cache a matrix and its inverse in its parental envrionment, retreive 
## it and compare to the current matrix. If its the same data it's done, 
## otherwise it calculates the new matix inverse. 


## This first function creates an object that makes a matrix and then caches
## it to its parent environment. By doing this there are pointers that stay 
## in place after "garbage collection" that allows the data to be retrieved 
## quickly.

makeCacheMatrix <- function(x = matrix()) {
        nvrs <- NULL
        set <- function(y) {
                x <<- y
                nvrs <<- NULL
        }     
        get <- function() x
        setInverse <- function(inverse) nvrs <<- inverse
        getInverse <- function() nvrs
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function retrieves the cached matrix inverse of the value of x that 
## was stored in it parent environment by the above function: 
## makeCacheMatrix. If it is found to be the same inverse matrix it does 
## nothing. Otherwise it computes the inverse matrix of the new matrix,
## that is the current value of x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        nvrs <- x$getInverse()
        if (!is.null(nvrs)) {
                message("getting cached data")
                return(nvrs)
        }
        mat <- x$get()
        nvrs <- solve(mat, ...)
        x$setInverse(nvrs)
        nvrs
}
