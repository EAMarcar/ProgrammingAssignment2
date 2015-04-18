## R programming assignment 2

## makeCacheMatrix  - first of two functions

## this function will accept in a matrix, use the solve
## function to invert it and store it in cache

makeCacheMatrix <- function(x = matrix()) {

      ## Set up a function to create a list consisting
      ## of the matrix and its inversion.
      ## This function will be called below 
      ## after the inversion takes place

      store_m <- function (y) {
            mat <<- x
            mat_inv <<- y
            m <<- list(matrix = mat, inverse = mat_inv)
   
      ## m is a list that contains the matrix and its
      ## inverse; the matrix will be used in the next
      ## function to test if it has changed
      
      }  

## invert the matrix "X" 
## call store_m (defined above) to store the results 


y <- solve(x)
store_m(y)

      
}


##    ############    ##

## cacheSolve  - second of two functions

## This function will determine if an inversion
## of a matrix needs to be performed or if it is
## already available in cache;
## in either case an inversion of the matrix is returned

cacheSolve <- function(x, ...) {
       
      ## First the function will create some variables
      ## used to test that the inversion exists
      ## or if the matrix has changed 
      
      unchgd <- FALSE
      inv <- FALSE
      tm_x <- dim(x)    
      tm_m <- dim(m$matrix)
      ti_m <- length(m$inversion)
      
      
      ## Check that the inversion is present
      
      if (ti_m > 0) {
            inv <- TRUE
            
      
            ## if the inversion is present, 
            ## Test to see if the matrix has changed
            ## (match rows, columns and top left cell).
            ## !! NOT A REAL WORLD TEST !!
                 
            if   ( tm_x[1] == tm_m[1] && 
                    tm_x[2] == tm_m[2] &&
                       x[1,1] == m$matrix[1,1]) {
                              
                             unchgd <- TRUE
                             
            }           
      } 
        
      ## If the matrix hasn't changed and the inversion
      ## exists, retrieve the inverse from cache      
      ## and return it
                  
      if (unchgd == TRUE && inv == TRUE) {
            
            return (m$inverse)
            }
      else {
      
            ## if the matrix is changed or there is no inverse,
            ## replace the matrix and its inverse
            ## and save them to cache
          
            m$matrix <<- x
            m$inverse <<- solve(x)
             
             ## return the inverted matrix 
         
            return (m$inverse)
             
             }


}
