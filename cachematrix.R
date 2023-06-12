makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(matrix) {
        x <<- matrix
        inv <<- NULL  # Limpar o valor do inverso ao definir uma nova matriz
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x) {
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}


# Definindo a matriz de exemplo
matriz <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

# Criando o objeto cacheMatrix
cacheMatrix <- makeCacheMatrix(matriz)

# Obtendo a matriz armazenada no objeto cacheMatrix
matriz_obtida <- cacheMatrix$get()

# Imprimindo a matriz obtida
print(matriz_obtida)

# Calculando o inverso da matriz
inverso <- cacheSolve(cacheMatrix)

# Imprimindo o inverso da matriz
print(inverso)
