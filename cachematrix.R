# ProgrammingAssignment2
# The functions below allow the user calculate the inverse the matrix and
# cache the results. So, after to calculate the first time, the user will
# save the time necessary to make the calculations.

# The first function, `makeCacheMatrix` creates a special "vector",
# extending it through addition of some methods in this vecto
# 1. set - store in a particular env the matrix
# 2. get  - return the matrix
# 3. setinverse - store inverse matrix in a specific env
# 4. getinverse - return the inverse matrix if this specific env

makeCacheMatrix <- function( x = matrix() ) {

        inversematrix = NULL

        set <- function( y ) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x

        setinverse <- function(solve) inversematrix <<- solve
        getinverse <- function() inversematrix

        list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The function "cacheSolve" calculates the mean of "matrix" created
# through function "makeCacheMatrix"
# However, through the getinverse method, it first checks to see
# if the mean has already been calculated.
# If so, it `get`s the mean from the cache and skips the computation.
# Otherwise, it calculates the mean of the data and sets the value of
# the mean in the cache via the `setmean` function.

cacheSolve <- function( x, ... ) {

        inversematrix <- x$getinverse()

        if( !is.null( inversematrix ) ) {
                message("getting cached data")
                return( inversematrix )
        }
        invertiblematrix <- x$get()
        inversematrix <- solve(invertiblematrix, ...)
        x$setinverse(inversematrix)
        inversematrix
}
