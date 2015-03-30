my_kohonen_som <- setClass(
  Class="my_kohonen_som",
  slots=c(
    x="matrix",
    xNormalized="matrix",
    normalizationFactor="numeric",
    weights="matrix",
    output="matrix",
    winner="numeric",
    learnRate="numeric",
    learnRateCurrent="numeric",
    quitError="numeric",
    monitor="function",
    iteration="numeric"
  ),
  prototype=list(
    weights=NULL,
    learnRate=0.5,
    quitError=0.1,
    monitor=NULL
  )
);

setMethod (
  "initialize",
  "my_kohonen_som",
  function (.Object, x, weights, monitor, learnRate, ...) {
    if (missing(x)) {
      stop('Input x data must be provided');
    }
    # normalize inputs
    .Object@x <- scale(x)
    
    if (!missing(weights)) {
      if (!is.matrix(weights)) {
        stop('Weights must be matrix');
      } else if (ncol(weights) != ncol(x)) {
        stop('Weights matrix column count must match with x matrix column count');
      } else if (nrow(weights) != ncol(x)) {
        stop('Weights matrix row count must match with x matrix column count');
      }
      .Object@weights <- weights;
    }
    
    if (!missing(monitor)) {
      if (!is.function(monitor)) {
        stop('Monitor argument must be a function')
      }
      .Object@monitor <- monitor;
    }
    
    if (!missing(learnRate)) {
      if (!is.numeric(learnRate)) {
        stop('Monitor argument must be a function')
      }
      .Object@learnRate <- learnRate;
    }
    
    return(my_kohonen_som.run(.Object));
  }
)

setGeneric(
  "my_kohonen_som.run", 
  function(.Object, ...) standardGeneric("my_kohonen_som.run")
)
setMethod(
  "my_kohonen_som.run",
  "my_kohonen_som",
  function (.Object, ...) {
    # set initial random weights if not provided
    if (is.null(.Object@weights)) {
      inputNeuronsCount <- ncol(.Object@x);
      .Object@weights <- matrix(runif(inputNeuronsCount ^ 2), inputNeuronsCount, inputNeuronsCount)
    }
    
    len <- nrow(.Object@x)
    .Object@learnRateCurrent <- .Object@learnRate
    for (i in 1:len) {
      .Object@iteration <- i
      # calculating each neuron's output
      .Object@output <- (.Object@x[i, ] %*% t(.Object@weights)) #* .Object@normalizationFactor
      
      # choosing the winner
      .Object@winner <- which(.Object@output == max(.Object@output, na.rm=TRUE))
      
      # adjust winner neuron weights with subtractive method(original additive method by Kohonen
      # may shows excessive instability)
      oldWeigths <- .Object@weights[.Object@winner, ]
      e <- .Object@x[i, ] - .Object@weights[.Object@winner, ]
      .Object@weights[.Object@winner, ] <- .Object@weights[.Object@winner, ] + .Object@learnRate * e
      
      if (.Object@learnRateCurrent > 0.01 ) {
        .Object@learnRateCurrent <- .Object@learnRateCurrent * 0.99;
      }
      if (is.function(.Object@monitor)) {
        .Object@monitor(.Object)
      }
    }
    
    
    return(.Object)
  })


setGeneric(
  "my_kohonen_som.predict", 
  function(.Object, x, ...) standardGeneric("my_kohonen_som.predict")
)
setMethod(
  "my_kohonen_som.predict",
  "my_kohonen_som",
  function (.Object, x, ...) {
    x <- scale(x, center = FALSE);
    len <- nrow(x)
    result <- matrix(NA, nrow=len, ncol=1)
    for (i in 1:len) {
      # calculating each neuron's output
      output <- (x[i, ] %*% t(.Object@weights)) #* .Object@normalizationFactor

      # choosing the winner
      result[i,] <- which(output == max(output, na.rm=TRUE))
    }
    return(result)
  })