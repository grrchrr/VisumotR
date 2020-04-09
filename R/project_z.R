
project_z <- function(image, width, height, method, depth) {
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
  projection <- array(apply(stack, c(1,2), methods[[method]]) %>% as.vector(), dim = c(height,width, 1))
  
  if (depth == 8) {
    return(image_read(projection/255))
  } 
  if (depth == 16) {
    return(image_read(projection/65536))
  } 
}



