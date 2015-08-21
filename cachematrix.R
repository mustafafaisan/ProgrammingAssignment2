## Put comments here that give an overall description of what your
## functions do

## function to 

makeCacheMatrix <- function(x = matrix()) {
    # This function creates and returns a list of getter and setter functions
    # to functions to matrix x and its inverse y
    y <- NULL # to hold inverse of matrix x
    get <- function() x
    set <- function(mat){
        x <<- mat
        y <<- NULL # matrix is updated, assign null to inverse
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
    # For matrix inverse cannot be calulated, using try cache
    tryCatch({
        inv = solve(matData)
        x$setinverse(inv)
    }, error = function(err){
        print("Error occured while solving for inverse of matrix")
    }, finally = {
        inv
    })
    inv
}

# To Genreate a matrix for inverse calculation
hilbert <- function(n) { 
    i <- 1:n; 1 / outer(i - 1, i, "+") 
}

# solve inverse without cache
inverseMatrixNoCache <- function(n, size){
    # n - no of iterations
    # size is the size of the matrix, it will be square matrix
    
    # This will try to find time taken for n iterations without cache
    curMatrix = hilbert(size)
    start = Sys.time()
    for(i in 1:n){
        tryCatch({
            m <- solve(curMatrix)
        }, error = function(err){
            print(paste0("Error calculating invesre of matrix", err))
        }, finally = {
            
        }
        )
    }
    end = Sys.time()
    print("withOutCacheCheck")
    print(end - start)
}

# solve inverse using local cache
inverseMatrixWithCache <- function(n, size) {
    # n - no of iterations
    # size is the size of the matrix, it will be square matrix
    # This will try to find time taken for n iterations using local cache
    
    curMatrix = hilbert(size)
    cdata = makeCacheMatrix(curMatrix)
    start = Sys.time()
    for(i in 1:n){
        tryCatch({
            m <- solve(cdata$get())
        }, error = function(err){
            print(paste0("Error calculating invesre of matrix", err))
        }, finally = {
            
        }
        )
    }
    end = Sys.time()
    print("WithCache")
    print(end -start)
}

# To do some perfromance testing 
runExpr <- function(n){
    # n - The size of matrix
    size = as.integer(runif(1, 5, 15))
    print(paste0("The size =", size))
    #size = 10
    inverseMatrixNoCache(n, size)
    inverseMatrixWithCache(n, size)
}
  

