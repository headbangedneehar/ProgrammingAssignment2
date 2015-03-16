## makeCacheMatrix() is to accept matrix as input.
## it does 4 basic tasks:
## 1. sets/stores a matrix
## 2. gets/returns a matrix
## 3. sets matrix inverse
## 4. gets matrix inverse

makeCacheMatrix<-function(matrix=matrix())
{
    matrixInverse<-NULL  ##matrixInverse is inverse of matrix
    set<-function(y)     ##set() to store matrix in cache
    {
        matrix<<-y
        matrixInverse<<-NULL
    }
    get<-function() matrix ##get() to get matrix
    setInverse<-function(inv) matrixInverse<<-inv ##setInverse() to set inverse and cache
    getInverse<-function() matrixInverse ##getInver() to get matrix inverse
    list(set=set,get=get,setInverseMatrix=setInverse,
         getInverse=getInverse)
}

## cacheSolve() takes object of makeCacheMatrix() as input
## it checks to see if inverse of matrix is present in cache
## if inverse present in cache, returns matrix inverse without calculating
## if not present in cache, calculates and returns matrix inverse

cacheSolve<-function(matrix, ...)
{
    matrixInverse<-matrix$getInverse() ## gets the inverse
    if(!is.null(matrixInverse)) ## checks if inverse in cache
    {
        message("getting cached data")
        return(matrixInverse) ## returns cached inversed if present in cache
    }
    data<-matrix$get()
    matrixInverse<-solve(data, ...) ## calculate inverse if not in cache
    message("No cached data found, calculating inverse...")
    matrix$setInverse(matrixInverse)
    matrixInverse  ## return inverse
}
