## 
## cachematrix is a function that stores a list of functions:
## makeCacheMatrix and cacheSolve

cachematrix <- function() {
  
  ## MakeCacheMatrix: Function to create a special "matrix" object that 
  ## can cache its inverse
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  }## end makeCacheMatrix
  
  ## cacheSolve: Function to computes the inverse of the special "matrix" 
  ## returned by makeCacheMatrix
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setsolve(m)
    m
  }## end cacheSolve
  
  ## cachematrix stores the pair of functions makeCacheMatrix and cacheSolve as result
  list(makeCacheMatrix = makeCacheMatrix, cacheSolve = cacheSolve )
  
} ##end