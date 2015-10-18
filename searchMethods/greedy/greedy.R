
require(foreach)

forward <- function(attributes,
                    eval.fun,
                    runSearchInParallel = TRUE,
                    ...) {
  
  greedy(attributes,
         eval.fun,
         TRUE,
         runSearchInParallel,
         ...)
}

backward <- function(attributes,
                     eval.fun,
                     runSearchInParallel = TRUE,
                     ...) {
  
  greedy(attributes,
         eval.fun,
         FALSE,
         runSearchInParallel,
         ...)
}

greedy <- function(attributes,
                   eval.fun,
                   forward = TRUE,
                   runSearchInParallel = TRUE,
                   ...) {
  
  if(length(attributes) == 0)
    stop("Attributes not specified")
  
  eval.fun = match.fun(eval.fun)
  best = list(
    result = -Inf,
    attrs = rep(as.numeric(!forward), length(attributes))
  )
  
  # initial evaluation for full set when backward
  if(!forward) {
    value = eval.fun(attributes[as.logical(best$attrs)], ...)
    best$result = value
  }
  
  forward_text = ifelse(forward, "forward", "backward")
  
  # main loop
  repeat {
    
    # create new matrix of children to evaluate
    children = create.children(best$attrs, forward_text)
    
    if(is.null(children)){
      break()
    }
    
    if (runSearchInParallel) {
      children_results <- 
        foreach(childIndex = 1:nrow(children),
                .combine = "c"
        ) %dopar% {
          eval.fun(attributes[as.logical(children[childIndex,])],
                   ...)
        }
    } else {
      children_results <- 
        apply(children, 1,
              function(vec) {
                eval.fun(attributes[as.logical(vec)],
                         ...)
              })
    }
   
    local_best = find.best(children_results)
    
    # compare to the best so far
    if(local_best$result > best$result) {
      best$result = local_best$result
      best$attrs = children[local_best$idx,]
    } else {
      break()
    }
  }
  
  return(attributes[as.logical(best$attrs)])
  
}

best.first.search <- function(attributes,
                              eval.fun,
                              max.backtracks = 5,
                              runSearchInParallel = TRUE,
                              ...) {
  
  if(length(attributes) == 0)
    stop("Attributes not specified")
  
  eval.fun = match.fun(eval.fun)
  
  attach_children <- function(states, best) {
    parent_state = states$attrs[best$idx,]
    children = create.children(parent_state, "forward", omit.func = function(...) {
      length(find.subset(states$attrs, ...)) > 0
    })
    children_len = ifelse(is.null(children), 0, dim(children)[1])
    if(children_len > 0) {
      states$attrs = rbind(states$attrs, children)
      states$open = c(states$open, rep(TRUE, children_len))
      states$results = c(states$results, rep(NA, children_len))
    }
    return(states)
  }
  
  states = list(
    attrs = diag(length(attributes)),
    open = rep(TRUE, length(attributes)),
    results = rep(NA, length(attributes))
  )
  colnames(states$attrs) = attributes
  
  best = list(
    result = -Inf,
    idx = NULL
  )
  
  repeat {
    # calculate merit for every open and not evaluated subset of attributes
    rows_to_eval = states$open & is.na(states$results)
    if(any(rows_to_eval)) {
      states$results[rows_to_eval] = 
        apply(states$attrs[rows_to_eval,, drop=FALSE], 1, function(vec) {
          attrs = attributes[as.logical(vec)]
          return(eval.fun(attrs, ...))
        })
    }
    #find best
    new_best = find.best(states$results, states$open)
    
    #check if better
    if(is.null(new_best$result))
      break()
    states$open[new_best$idx] = FALSE
    if(new_best$result > best$result) {
      best = new_best
      states = attach_children(states, best)
    } else {
      if(max.backtracks > 0) {
        max.backtracks = max.backtracks - 1
      } else
        break
    }
  }
  return(attributes[as.logical(states$attrs[best$idx, ])])
  
}


#-------------------------search misc--------------------------------

find.subset <- function(subsets.matrix, subset) {
  subset = as.vector(subset)
  len = length(subset)
  if(len == 0)
    stop("Empty atrributes subset.")
  if(dim(subsets.matrix)[2] != len)
    stop("Incorrect dimensions.")
  
  if(dim(subsets.matrix)[1] == 0)
    return(as.integer(NULL))
  
  cond = rep(TRUE, dim(subsets.matrix)[1])
  for(i in 1:len)
    cond = cond & (subsets.matrix[,i] == subset[i])
  return(which(cond))
}

create.children <- function(parent, direction = c("forward", "backward", "both"), omit.func = NULL ) {
  direction = match.arg(direction)
  
  if(!is.null(omit.func)) {
    omit.func = match.fun(omit.func)
  }
  
  cols = length(parent)
  if(cols <= 0)
    stop("Parent attribute set cannot be empty.")
  
  m1 = NULL
  m2 = NULL
  
  if(direction == "forward" || direction == "both") {
    rows = cols - sum(parent)
    if(rows > 0) {
      m1 = matrix(parent, ncol = cols, nrow = rows, byrow = TRUE)
      
      current_row = 1
      current_col = 1
      repeat {
        if(current_col > cols || current_row > rows)
          break()
        
        if(m1[current_row, current_col] == 0) {
          m1[current_row, current_col] = 1
          current_row = current_row + 1
        }
        
        current_col = current_col + 1
      }
    }
  }
  
  if(direction == "backward" || direction == "both") {
    rows = sum(parent)
    if(rows > 1) { # skipped if only 0s
      m2 = matrix(parent, ncol = cols, nrow = rows, byrow = TRUE)
      
      current_row = 1
      current_col = 1
      repeat {
        if(current_col > cols || current_row > rows)
          break()
        
        if(m2[current_row, current_col] == 1) {
          m2[current_row, current_col] = 0
          current_row = current_row + 1
        }
        
        current_col = current_col + 1
      }
    }
  }
  
  m = rbind(m1, m2)
  if(is.null(m))
    return(m)
  if(!is.null(omit.func)) {
    rows_to_omit = apply(m, 1, omit.func)
    return(m[!rows_to_omit,, drop = FALSE])
  } else {
    return(m)
  }
}

find.best <- function(results, subset = rep(TRUE, length(results))) {
  best = list(
    result = NULL,
    idx = NULL
  )
  
  w = which(subset)
  if(length(w) > 0) {
    children_results = results[w]
    max_idx = which.max(children_results)
    best$result = children_results[max_idx]
    best$idx = w[max_idx]
  }
  return(best)
}
