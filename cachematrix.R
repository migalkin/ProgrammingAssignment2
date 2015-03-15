## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function consists of :
## 1. variable 'inverse' to store the inverse of the matrix
## 2. set() function to set a new matrix
## 3. get() function to get a saved matrix
## 4. setInverse() function sets a new inverse of a matrix
## 5. getInverse() function gets the saved inverse value

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## The function consists of:
## 1. get cached 'inverse' value
## 2. check if it is empty
## 3. if there is a cached value then return it
## 4. otherwise compute an inverse for the input matrix
## 5. finally, return inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("Fetching cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
