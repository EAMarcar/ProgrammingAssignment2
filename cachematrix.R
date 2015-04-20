## R programming assignment 2

## makeCacheMatrix  - first of two functions

## this function will accept in a matrix, use the solve
## function to invert it and store it in cache

makeCacheMatrix <- function(x = matrix()) {

      ## initialize the inverse (inv) to NULL 
      
      inv <- NULL
      
      ## create a function to load the matrix
      ## and set the inverse to NULL
      
      set_m <- function (y) {
            x <<- y
            inv <<- NULL
      }
            
   
      ## build functions to:
      ##          get the matix
      ##          set the inverse
      ##          get the inverse
      
      get <- function() { x  }
      set_inv <- function (solve) { inv <<- solve }
      get_inv <- function () inv
      
      ## create a list that consists 
      ## of the functions above
      ## and store it in cache
      
      fl <<- list(set_m = set_m, get = get, set_inv = set_inv, get_inv = get_inv)
      
}
      

##    ############    ##

## cacheSolve  - second of two functions

## This function will determine if an inversion
## of a matrix needs to be performed or if it is
## already available in cache.
## In either case an inversion of the matrix is returned

cacheSolve <- function(x, ...) {
      
      ## check if the inverse is null
      ## if not, return it from cache
      
      inv <- fl$get_inv()
      if (!is.null(inv)) {
            
            message ("getting cached data")
            return (inv)
            
      }
      
       
      ## if the inverse is null it gets created here
      ## and returned
      
      mat <- fl$get()
      
      inv <- solve(mat, ...)
      
      fl$set_inv(inv)
      
      inv
      
      
}
