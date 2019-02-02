## R Programming Assignment 2: Lexical Scoping


## makeCacheMatrix function creates "matrix" object, which contains a list of sub-functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cachesolve Function first checks  if the inverse matrix is already  caclulated, 
##If found, it takes the matrix from cache. or else, it will calculate the matrix
## inverse 


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Example
mat1 = matrix(rnorm(10000), nrow = 100, ncol = 100)
mat1
m1<-makeCacheMatrix(mat1)
m1
cacheSolve(m1)