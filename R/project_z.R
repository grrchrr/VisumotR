# Z-projection for 3D-image stacks
project_z <- function(image, width, height, method, depth, weights=NULL) {
  
  methods <- list(mean = mean,
                  sd = sd,
                  min = min,
                  max = max,
                  median = median)
  n <- length(image)
  
  stack <- array(NA, dim = c(height,width,n))
  for (i in c(1:n)) {
    stack[,,i] <- image[[i]] %>% as.integer()
  }
  
  if (is.null(weights)) {
    projection <- array(apply(stack, c(1,2), methods[[method]]) %>% as.vector(), dim = c(height,width, 1))
  } else {
    projection <- array(apply(stack, c(1,2), weighted.mean, weights) %>% as.vector(), dim = c(height,width, 1))
  }
  
  if (depth == 8) {
    return(image_read(projection/255))
  } 
  
  if (depth == 16) {
    return(image_read(projection/65536))
  } 
}



