# 
# read.csv('../201910003_2B7_10min_LP30_1_5e8701666bcfb_hrm_Spots.txt', sep='\t') %>% as_tibble() %>%
# rename(track=TRACK_ID, X= POSITION_X, Y=POSITION_Y, Z=POSITION_Z, time=POSITION_T, int=MAX_INTENSITY) %>%
#   select(track, time, X, Y, Z, int) %>% mutate(X=X/0.160, Y=Y/0.160, Z=Z/0.3) -> df_3D
# df_3D$track <- as.factor(df_3D$track)
# 
# 
# images <-  list.files(pattern='.tif')
#  visumot_frame(df_3D, image=images[3], frame=3, dimensions=3, image_depth=8, projection='max', par.map='track')
# # image <-  image_read(images)
# 
# 
# 
# 
# pos <- df_3D %>% filter(time==0) %>% mutate(X=round(X), Y=round(Y)) %>% select(X,Y) %>% as.list()
# 
# array <- array(NA,dim=c(511,511,1))
# 
# for (i in c(1:length(pos$Y))){
# array[pos$Y[i], pos$X[i],1] <- 1
# }
# 
# 
# image_read(array) %>% image_write('calibrate.tiff', format='tif')









