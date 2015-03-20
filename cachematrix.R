##This functions accepts a matrix as input
##setMatrix function sets the input matrix
##getMatrix function returns the input matrix as it is
##setInverseMatrix inverses the input matrix using solve
##getInverseMatrix returns the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL

        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }

        getMatrix <- function() x

        setInverseMatrix <- function(Inversed) m <<- Inversed
        getInverseMatrix <- function() m

        list(set = setMatrix, get = getMatrix,
             setInverse = setInverseMatrix,
             getInverse = getInverseMatrix)

}


##This function checks for the presence of cached inverse of input matrix. If one exists, returns it. 
##Otherwise prepares the inverse of it and returns.
cacheSolve <- function(x,...){
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}