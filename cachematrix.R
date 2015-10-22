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
            mat <<- y_mat      #store matrix in mat, set inverse to null
            inv <<- NULL
      }
      get <- function() mat   #return matrix
      setInverse <- function(y_inv) inv <<- y_inv  #set stored inverse 
      getInverse <- function() inv                 #return stored inverse
      
      #return list of get/set functions:
      
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

cacheSolve <- function(m,...){
      
      # This function parallels cachemean function in example code 
      # It checks stored inverse in m and if NULL computes inverse, stores it
      # in m and returns inverse
      
      x <- m$getInverse()         #get inverse variable stored in m, store in x
      if (is.null(x)){    
            
            #if no inverse stored, compute inverse and store in m and x
            
            x <- solve(m$get(),...)
            m$setInverse(x)
      }
      x                    #return inverse as stored in x 
}