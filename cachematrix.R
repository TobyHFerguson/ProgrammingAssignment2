## This function `makeCacheMatrix` creates a special matrix, which is a list
## containing 4 functions which will
## 1 set the value of the matrix
## 2 get teh varlue of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- solve(y)
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## the following function calculates the inverse of the special matrix
## created with the above function. However it first checks to see if the
## inv has already been calculated and if the matrix is equal to the currently
## cached one; iff both of these conditions are true then it returns the
## cached inverse
cacheSolve <- function(x, ...) {
    if (is.null(x$getinv())) {
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
    } else {
        message("getting cached inverse")
    }
    x$getinv()
}

## Tests
identity <- matrix(c (1,0,0,1),2,2)
id2 <- makeCacheMatrix(identity)
identical(cacheSolve(id2), identity)
## This next call should evaluate to true and print out the message 'getting cached inverse'
identical(cacheSolve(id2), identity)

large_matrix <- matrix (rnorm(100),10,10)
lm2 <- makeCacheMatrix(large_matrix)
identical (cacheSolve (lm2), solve (large_matrix))
identical (cacheSolve (lm2), solve (large_matrix))

large_matrix[large_matrix < 0.6] <- 0.372
identical(cacheSolve(id2), identity)
identical (cacheSolve (lm2), solve (large_matrix))

