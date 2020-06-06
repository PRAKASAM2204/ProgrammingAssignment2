## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        getInverse <- function() {
                if(length(x)%%sqrt(length(x))==0) { ## make sure matrix is square
                        inv <<- solve(x)
                }
        }
        list(get = get, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        if(!is.atomic(x)){     
                inv <- x$getInverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }     
        } else {        
                message("getting the inverse-- no cached data found")
                return(makeCacheMatrix(x)$getInverse())
        } 
        
}


f <- makeCacheMatrix(matrix(1:9, nrow = 3, ncol = 3))
print(cacheSolve(f))


print(cacheSolve(matrix(1:9, nrow = 3, ncol = 3)))


