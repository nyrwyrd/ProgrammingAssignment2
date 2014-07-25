# if you happen to stumble onto this file, ignore it
# this is the same implementation as "cachematrix.R" but it was renamed for development
# and not intended for upload; just can't figure how to get rid of it
# please see "cachematrix.R"
# 
# matrix cache functions
# author: jrb
# date: 2014-07-14
# 
# these 3 functions -
#     makeCacheMatrix: define a cache for a square matrix
#     cacheSolve: operate on an instantiation of makeCacheMatrix
#     testCacheMatrix: exercises operation on passed matrix
# 
# assumptions:
#     specified matrix is invertible
# 
# test case examples:
##     inverible 2x2 matrix:
#         m2<-matrix(1:4, 2, 2)
#         testCacheMatrix(m2)
# 
##     inverible 4x4 matrix:
#         m4<-matrix(c(1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 0, 1, 4, 2, 3), 4, 4, byrow = T)
#         testCacheMatrix(m4)
# 
##     singular 3x3 matrix:
#         m3<-matrix(rep(1,9), 3 , 3)
#         testCacheMatrix(m3)
# 

# makeCacheMatrix
#     purpose: eliminate redundant computation of matrix inverse by caching result
#     input: square matrix
#     assumption: matrix is invertible
#     returns: list of available functions
#     properties:
#         contains a cached copy of a matrix and its inverse
#         expose a list of functions used to operate on the cache
#         functions:
#             set - save matrix to cache
#             get - retreive matrix from cache
#             setInverse - save inverse of matrix to cache
#             getInverse - retrieve matrix inverse from cache
makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL  
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xi <<- inverse
    getInverse <- function() xi
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse
    )
}

# cacheSolve
#     purpose: compute inverse of matrix associated with instantiation of 'makeCacheMatrix'
#     input: object defined by instantiation of 'makeCacheMatrix'
#     returns: inverse of cached matrix
#     properties:
#         requires instantiation of 'makeCacheMatrix'
#         checks cache for existing inverse before computing
#         singular matrix generates error and cached inverse remains null
cacheSolve <- function(x, ...) {
    xi <- x$getInverse()
    if(!is.null(xi)) {
        message("getting cached data")
        return(xi)
    }
    data <- x$get()
    xi <- solve(data, ...)  # use solve to get inverse
    x$setInverse(xi)
    xi
}

# testCacheMatrix
#     purpose: exercise use of mobjects of 'makeCacheMatrix'
#     input: matrix to be managed 
#     returns: 
#         identity: when inverse computed correctly
#         error: when solution of inversion fails
#     properties:
#         prints intermediate results of function calls to object
testCacheMatrix <- function(x=matrix()) {
            xt<-makeCacheMatrix(x)
            print(xt$get())
            print(xt$getInverse())
            print(cacheSolve(xt))
            print(cacheSolve(xt)) ## get from cache
            xt$get() %*% xt$getInverse() ## identity
}

