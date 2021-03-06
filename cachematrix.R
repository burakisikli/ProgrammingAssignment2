## Programming Assingment 2
## Caching the Inverse of a Matrix

# Input: x, a square invertible matrix
# Output: a list containing functions to
#  1. set the matrix
#  2. get the matrix
#  3. set the inverse matrix
#  4. get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  result <- NULL
  set <- function(value) {
    x <<- value
    result <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) result <<- inv
  getinverse <- function() result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Input: x, a square invertible matrix
# Output: return the inverse matrix. 
# If reverse matrix has already been computed, it gets the result from the cache, 
# otherwise first it calculates the inverse, 
# sets the inverse matrix to the cache for the future calls 
# and return the inverse matrix
# If the column size of matrix and row size of matrix isn't equal, reverse matrix 
# can not be calculated so function returns an error message
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  nrow(data) 
  ncol(data)
  if(nrow(data) != ncol(data)){
    stop("Matrix isn't reversable")
  }
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## > x = matrix( rnorm(4*4), 4, 4) 
## > x
## [,1]       [,2]       [,3]       [,4]
## [1,]  0.6620174  0.9161560 -2.1062903  1.6868503
## [2,] -0.4679449 -0.6783725 -0.4448144  1.7986049
## [3,]  0.1270844  0.2589254  1.6507074  0.1088753
## [4,]  1.1072815  0.7657950  0.6802348 -0.5961138

## > m = makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]        [,2]       [,3]        [,4]
## [1,] -0.4001333  0.98346112 -0.9316322  1.66488534
## [2,]  0.8105560 -1.20543245  1.1746208 -1.12884948
## [3,] -0.1078742  0.08836469  0.4723390  0.04762787
## [4,]  0.1749326  0.37906000  0.3174578  0.01917019

## > cacheSolve(m)
## getting cached data
## [,1]        [,2]       [,3]        [,4]
## [1,] -0.4001333  0.98346112 -0.9316322  1.66488534
## [2,]  0.8105560 -1.20543245  1.1746208 -1.12884948
## [3,] -0.1078742  0.08836469  0.4723390  0.04762787
## [4,]  0.1749326  0.37906000  0.3174578  0.01917019

## > x = matrix( rnorm(4*3), 4, 3) 
## > x
##           [,1]       [,2]       [,3]
## [1,] 1.5852080  0.2941187  0.1529524
## [2,] 0.5811735  0.5939843  0.5287651
## [3,] 0.2862531 -0.3325974 -1.3868125
## [4,] 0.9317229  1.0535774  0.4423358

## > m = makeCacheMatrix(x)
## > cacheSolve(m)
## Error in cacheSolve(m) : Matrix isn't reversable