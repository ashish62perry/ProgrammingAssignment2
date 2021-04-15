
# makeCacheMatrix function takes a square matrix as a parameter \
# inverse of the matrix is first set to NULL (to be calculated later by another function cacheSolve)
# the set function inside makeCacheMatrix does the same thing first two lines do but is used to change matrix 
# the get function inside makeCacheMatrix retrieves current matrix 
# setInverse function inside makeCacheMatrix requires another function to set inverse to parent environment of makeCacheMatrix
# getInverse function inside makeCacheMatrix retrieves inverse


makeCacheMatrix <- function(x = matrix()){
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve function takes a makeCacheMatrix() object as its parameter
# cacheSolve function first checks if there exists an inverse value of the provided matrix
# and returns the current inverse value from cache if matrix has not been changed
# otherwise calculate inverse


cacheSolve <- function(mat) {
        m <- mat$getInverse()
        if (!is.null(m)){
                message('Retriving Inverse from cache')
                return (m)
        }
        data <- mat$get()
        inv <- solve(data)
        mat$setInverse(inv)
        inv
}


sampleMatrix <- matrix(1:4, 2, 2) # This is a square matrix for which we will calculate inverse

mat <- makeCacheMatrix(sampleMatrix)
cacheSolve(mat) # cacheSolve function calculates inverse and stores its value in cache
cacheSolve(mat) # cacheSolve function retrieves inverse from cache instead of calculating

