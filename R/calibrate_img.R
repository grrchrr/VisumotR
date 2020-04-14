
calibrate_img <- function(df,width,height,pars.list){
  pos <- df%>% filter(time==pars.list$frame -1) %>% mutate(X=round(X), Y=round(Y)) %>% select(X,Y) %>% as.list()
  print(pos)
  array <- array(NA,dim=c(height,width,1))
  for (i in c(1:length(pos$Y))){
    array[pos$Y[i], pos$X[i],1] <- 1
  }
  return(image_read(array))
}



