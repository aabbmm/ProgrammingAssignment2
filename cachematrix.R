makeCacheMatrix <- function(mat=matrix()){
      
      # This function parallels makeVector in example code for the assignment
      # The  matrix is stored in the mat variable and is accessed/modified by the 
      # with the get/set functions.
      # The inverse is stored in the inv and and is accessed/modified by the
      # getInverse/setInverse  function.
      # The function returns the list of get/set and getInverse/setInverse
      # functions
      
      inv <- NULL
      set <- function(y_mat){
            mat <<- y_mat
            inv <<- NULL
      }
      get <- function() mat
      setInverse <- function(y_inv) inv <<- y_inv
      getInverse <- function() inv
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

cacheSolve <- function(m,...){
      
      x <- m$getInverse()         #get inverse variable stored in m, store in x
      if (is.null(x)){    
            
            #if no inverse stored, compute inverse and store in m and x
            
            x <- solve(m$get(),...)
            m$setInverse(x)
      }
      x                    #return inverse as stored in x
}