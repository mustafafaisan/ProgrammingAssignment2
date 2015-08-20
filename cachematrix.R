## Put comments here that give an overall description of what your
## functions do

## function to 

makeCacheMatrix <- function(x = matrix()) {
    y <- NULL
    get <- function() x
    set <- function(mat){
        x <<- mat
        y <<- NULL
    }
    getinverse <- function() y
    setinverse <- function(m) y <<- mat
    list(set = set, get = get,getinverse = getinverse,
         setinverse = setinverse)
}


## cacheSolve returns inverse of the matrix from the cache
## if the matrix is modified, inverse is recalculated and returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv = x$getinverse()
    if (!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    matData = x$get()
    inv = solve(matData)
    x$setinverse(inv)
    inv
}

# To Genreate a matrix whose inverse exists
hilbert <- function(n) { 
    i <- 1:n; 1 / outer(i - 1, i, "+") 
}
  

