# # La función makeCacheMatrix almacena en caché la inversa de una matriz y la función
# # cacheSolve calcula la inversa de la matriz

# # La siguiente función makeCacheMatrix crea un objeto "matriz" especial que puede
# # caché su inverso.

makeCacheMatrix <- function(x = matrix()){
  inversa <- NULL
  set <- function(y){
    x<<-y
    inversa<<-NULL
    
  }
  get<-function(){x}
  setInversa<- function(inversacalculada){inversa<<-inversacalculada}
  getInversa<- function(){inversa}
  list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}

# # La siguiente función cacheSolve calcula la inversa de la "matriz" especial
# # devuelto por makeCacheMatrix arriba.

cacheSolve <- function(x, ...) {
  inversa <- x$getInversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setInversa(inversa)
  inversa
}