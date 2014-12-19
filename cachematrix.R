#####################################################################
###Assignement 2
#####################################################################

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(matrix = matrix()) {
iv <- NULL
set <- function(y) {
matrix <<- y
iv <<- NULL
}
get <- function() matrix
setinverse <- function(inverse) iv <<- inverse
getinverse <- function() iv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
iv <- x$getinverse()
if(!is.null(iv)) {
return(iv)
}
data <- x$get()
iv <- solve(data)
x$setinverse(iv)
iv
}

#Test:
matrix <- matrix(data = c(3,5,8,10), nrow = 2, ncol = 2)
matrix2 <- makeCacheMatrix(matrix)
cacheSolve(matrix2)
cacheSolve(matrix2)
