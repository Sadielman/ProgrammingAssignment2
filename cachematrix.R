#####################################################################
###                       cachematrix.R                           ###
###   Contains two related functions:                             ###
###      (1)  makeCacheMatrix creates list of functions to        ###
###           set and retrieve a matrix and its inverse using     ###
###           using global assignments to create a "pseudo-cache" ###
###      (2)  cacheSolve   uses the output of makeCacheMatrix.R   ###
###           to retrieve the matrix inverse from the cache if it ###
###           has already been calculated and store cached; and to###
###           calculate it anew if it is not cached.              ###
#####################################################################

## makeCacheMatrix accepts a matrix as input and creates a list  ##
## of 4 functions: set, get, setinverse, getinverse to establish ##
## and retrieve the passed matrix and its inverse.               ##

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
 } 


## cacheSolve takes as input the list created by makeCacheMatrix    ##
## and retrieves the matrix inverse from globally-assigned cache    ##
## if it has been previously calculated and stored; otherwise it    ##
## calls built-in R function solve() to calculate it and stores     ##
## it using setinverse() function created by makeCacheMatrix above  ##


cacheSolve <- function(x, ...) { 
## Return a matrix that is the inverse of 'x' 
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
} 
