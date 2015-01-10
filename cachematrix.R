## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. Folling functions allows to:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## It is assume that the matrices supplied are square, numerical and always invertible

## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix (get)
## 2. get the value of the matrix (set)
## 3. set the value of the inverse matrix (setInverse)
## 4. get the value of the inverse matrix (getInverse)
## =========
## Examples: 
## M <- makeCacheMatrix(matrix(c(1,2,3,4), nrow =2))
## M$get()

makeCacheMatrix <- function(x = matrix()) {
        ## Create a special "matrix", which is a list of function get, set, getInverse and setInverse
        inv <- NULL
        set <- function(Matr) {
                x <<- Matr
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function cahceSolve inverts the special "matrix" created with the function makeCacheMatrix. 
## Howwever, it first checks to see if inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the value of the inverse matirx in the cache 
## via the setInverse function.
## =======
## v <- cacheSolve(M)
## v %*% M$get()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
