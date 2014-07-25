## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function caches the value of the matrix x, 
# where the value of x is specified by the user (default is empty 0x0)
# usage: x2<-makeCacheMatrix(x)
# Created 24-July-2014 by JJB
makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL
      set <- function(y) { # function to set the value of the inputted matrix; set inverse to null
            x <<- y
            m_inv <<- NULL
      }
      get <- function() x # function to get the value of the inputted matrix
      setinv <- function(inv) m_inv <<- inv # function to set the value of the inverse matrix
      getinv <- function() m_inv # assigns the value of the inverse matrix to getinv
      list(set = set, get = get, # List is the output of this function ("special matrix")
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
# This function calculates the inverse of the matrix x, or alternatively, 
# if a cached value of this inverse exists 
# (created using the makeCacheMatrix function above, it is returned instead.
# usage: m_inv<-cacheSolve(x)
# Created 24-July-2014 by JJB

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m_inv <- x$getinv()#Extract the value of getinv field, assign to m_inv 
      # if m_inv isn't a NULL value (i.e. an inverse matrix exists already), return this value; skip calculation
      if(!is.null(m_inv)) { 
            message("getting cached data") # alert the user that the cached data is returned; return it.
            return(m_inv)
      }
      # Otherwise, if m_inv is a NULL value, calculate its inverse:
      data <- x$get()
      m_inv <- solve(data, ...)
      
      x$setinv(m_inv) # Use the setinv function to assign matrix inverse to x.
      m_inv #return the value of the inverset
}
