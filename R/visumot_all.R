# wrapper to run over image series

#' @title visumot_all
#' @description To be written...
#' @details  To be written...
#' @examples
#' visumot_all()




#' @export
visumot_all <- function(df, images, frame_range = NULL,
                        display_frame = TRUE, display_summary = FALSE,
                        visumot_frame.list = NULL,
                        visumot_summary.list = NULL,
                        save = TRUE, file_name = NULL,
                        width = 2000, height = 1200, rel_width = 1/3,
                        browse = FALSE, return = FALSE) {

  #' @param df dataframe of the form: \code{df(track, time, X, Y, mapping_parameters, ...)}
  #' @param images \code{vector}: filenames of images ordered by frames/time
  #' @param frame_range \code{integer}: frames to be mapped; default=\code{NULL} maps full time/frame range found in \code{df}
  #' @param display_frame \code{logical}: display output from \code{\link{visumot_frame}}
  #' @param display_summary \code{logical}: display output from \code{\link{visumot_summary}}
  #' @param visumot_frame.list \code{list}: parameters passed to \code{\link{visumot_frame}}
  #' @param visumot_summary.list \code{list}: parameters passed to \code{\link{visumot_summary}}
  #' @param save \code{logical}: saves tiff-stack or image to ./output/visumotr-date.tiff
  #' @param width \code{numeric}: width of output tiff in pixels
  #' @param height \code{numeric}: height of output tiff in pixels
  #' @param rel_width \code{numeric}: \code{if(display_frame & display_summary)}, \code{rel_width} defines
  #' the relative width of the summary plot to the mapped frame
  #' @param browse \code{logical}: browse output with default system application for tiffs, e.g. imageJ
  #' @param return \code{logical}: return images-series as magick image-stack

  message(paste('Started visumotR: ', Sys.time(), sep = ''))

  # extracting parameter lists
  temp.dir.frame <- NULL
  temp.dir.sum <- NULL

  # extracting frames
  if (is.null(frame_range)) {
    warning('frame_range not specified, defaulting to maximal frame range...')
    last_frame <- df %>% select(time) %>% pull() %>% max()
    last_frame <- last_frame + 1
    first_frame <- df %>% select(time) %>% pull() %>% min()
    first_frame <- first_frame + 1
  } else {
    first_frame <- frame_range[1]
    last_frame <- frame_range[2]
  }

  # visumot_summary()
  if (display_summary == TRUE) {
    message('Running visumot_summary()...')
    if (display_frame) {
      visumot_summary.list$legend <- FALSE
    }
    # create temp folder
    pos <- gregexpr('/',getwd())[[1]]
    temp.dir.sum <- paste0(str_sub(getwd(),0,pos[length(pos)]),'temp_sum')
    dir.create(temp.dir.sum)
    # set up cluster
    nc <- detectCores() - 1
    cl <- makeCluster(rep("localhost", nc), type = "SOCK")
    registerDoSNOW(cl)
    # start loop
    foreach(i = c(first_frame:last_frame),
            .export = c('visumot_summary', 'summary_mot', 'transfer_pars'),
            .packages = (.packages())
    ) %dopar% {
      frame_stat <- image_graph(width = width*rel_width, height = height, res = 100)
      visumot_summary.list$frame <- i
      print(visumot_summary(df,
                            visumot_summary.list,
                            all.list = TRUE))
      dev.off()
      frame_stat %>%
        image_write(paste0(temp.dir.sum,'/',str_c('0000',i) %>% str_sub(-4),'.tiff'), format = 'tif')
    }
    # stop cluster
    stopCluster(cl)
  }

  # visumot_frame()-plot
  if (display_frame == TRUE) {
    message('Running visumot_frame()...')
    # create temp folder
    pos <- gregexpr('/',getwd())[[1]]
    temp.dir.frame <- paste0(str_sub(getwd(),0,pos[length(pos)]),'temp_frame')
    dir.create(temp.dir.frame)
    # set up cluster
    nc <- detectCores() - 1
    cl <- makeCluster(rep("localhost", nc), type = "SOCK")
    registerDoSNOW(cl)
    # start loop
    foreach(i = c(first_frame:last_frame),
            .export = c('visumot_frame', 'crop_string', 'crop_string_df','get_crop_pars','transfer_pars','process_img','plot_frame','plot_frame_sub'),
            .packages = (.packages())) %dopar% {
              frames_map <- image_graph(width = width*(1 - rel_width), height = height, res = 100)
              visumot_frame.list$image <- images[i]
              visumot_frame.list$frame <- i
              print(visumot_frame.list)
              print(visumot_frame(df,
                                  visumot_frame.list,
                                  all.list = TRUE))
              dev.off()
              frames_map %>% image_write(paste0(temp.dir.frame,'/',str_c('0000',i) %>% str_sub(-4),'.tiff'), format = 'tif')
            }
    # stop cluster
    stopCluster(cl)
  }
  message(paste('Finished analysis: ', Sys.time(), sep = ''))

  # stack stuff
  message('Creating image stacks...')
  if (display_frame) {
    images_frame <- list.files(temp.dir.frame, full.names = TRUE) %>% image_read()
  }
  if (display_summary) {
    images_sum <- list.files(temp.dir.sum, full.names = TRUE) %>% image_read()
  }

  if (display_summary & display_frame == TRUE) {
    stack <- image_append(c(images_frame[1],images_sum[1]))
    if (length(images_sum) > 1 & length(images_frame) > 1) {
      for (i in c(2:length(images_frame))) {
        stack <- c(stack,image_append(c(images_frame[i],images_sum[i])))
      }
    }
  }
  message('Done.')

  # save stuff
  if (save == TRUE) {
    message('Saving images...')
    # create output folder
    pos <- gregexpr('/',getwd())[[1]]
    out.dir <- str_sub(getwd(),0,pos[length(pos)])
    dir.create(paste0(out.dir,'output'))
    # check file_name
    if (is.null(file_name)) {
      out_file <- paste(out.dir,'output/','visumotr_',Sys.Date(),'.tiff',sep = '')
    } else {
      out_file <- paste(out.dir,'output/',file_name,'_',Sys.Date(),'.tiff',sep = '')
    }
    # write image files
    if (display_summary & display_frame == TRUE) {
      stack %>% image_convert(format = 'tif') %>% image_write(path = out_file)
    } else {
      if (display_summary == TRUE) {
        images_sum %>% image_convert(format = 'tif') %>% image_write(path = out_file)
      } else {
        images_frame %>% image_convert(format = 'tif') %>% image_write(path = out_file)
      }
    }
    message(paste("Saved to: ", out_file, sep = ""))
  }

  # browse stuff
  if (browse == TRUE) {
    message('Initialized image browsing...')
    # initialize browsing
    if (display_summary & display_frame == TRUE) {
      stack %>% image_convert(format = 'tif') %>% image_browse()
    } else {
      if (display_summary == TRUE) {
        images_sum  %>% image_convert(format = 'tif') %>% image_browse()
      } else {
        images_frame %>% image_convert(format = 'tif') %>% image_browse()
      }
    }
  }
  unlink(temp.dir.frame, recursive = TRUE)
  unlink(temp.dir.sum, recursive = TRUE)
  message('Deleted temp_files.')
  # return stuff
  if (return == TRUE) {
    message('Returning image stack...')
    # returning stuff
    if (display_summary & display_frame == TRUE) {
      return(stack)
    } else {
      if (display_summary == TRUE) {
        return(images_sum)
      } else {
        return(images_frame)
      }
    }
  }
}
