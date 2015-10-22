makeCacheMatrix <- function(mat=matrix()){
      
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
      
      x <- m$getInverse()
      if (is.null(x)){
            x <- solve(m$get(),...)
            m$setInverse(x)
      }
      x
}