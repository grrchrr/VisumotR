# Mapping one frame



#' @title visumot_frame
#' @description To be written...
#' @details  To be written...
#' @examples
#' visumot_frame()

#' @export
visumot_frame <- function(df, ...) {

  # set default parameters
  pars.list.default <- list(image = NULL, frame = NULL, tracks = NULL, all.list = FALSE,
                            par.map = NULL, par.shape = NULL, par.display = TRUE, par.max = NaN, par.unit = NULL,
                            crop = FALSE, crop_pars = NULL, sub.img = FALSE , sub.window = 200, sub.col = 3,
                            tracks.size = 1, tracks.alpha = 0.5, tracks.length = NULL,
                            points.size = 1, points.alpha = 0.9, points.stat = 'echo', points.shape = 16,
                            axis.tick = 100, axis.display = TRUE, axis.labs = TRUE,
                            unit = 'px', scaling = 1,
                            scale.bar = FALSE, scale.width = 40, scale.height = 10, scale.x = 10,
                            scale.y = 10, scale.color = 'grey70')

  #' @param df dataframe of the form: \code{df(track, time, X, Y, mapping_parameters, ...)}
  #' @param image \code{character}: filename of image
  #' @param frame \code{integer}: frame to be mapped
  #' @param tracks \code{vector}: defining tracks to be displayed
  #' @param par.map \code{character}: specifying parameter in \code{df} to be visualized by color
  #' @param par.shape \code{character}: specifying parameter in \code{df} to be mapped on shape
  #' @param par.display display option for mapping; default: \code{TRUE}, mapping is disable with: \code{FALSE}
  #' @param par.max \code{numeric}: defining range of color mapping
  #' @param par.unit \code{character}: unit of the numeric mapped parameter
  #' @param crop \code{logical} option for cropping images; default: \code{FALSE}
  #' @param sub.img \code{logical} option for creating sub-images from specified \code{tracks} or pre-filtered \code{df}; default: \code{FALSE}
  #' @param sub.window \code{numeric}: size of the sub-images in pixels
  #' @param sub.col \code{numeric}: number of columns in which sub-images are arranged
  #' @param tracks.size \code{numeric}: size of tracks
  #' @param tracks.alpha \code{numeric}: transparency of tracks
  #' @param tracks.length \code{numeric}: length of tracks (in frames)
  #' @param points.size \code{numeric}: size of points
  #' @param points.alpha \code{numeric}: transparency of points
  #' @param points.stat \code{character}: display statistic; default: \code{'echo'}, for blurring; without blurring \code{'identity'}
  #' @param points.shape \code{numeric}: set shape from ggplot2 shape palette
  #' @param axis.tick \code{numeric}: axis ticks in px
  #' @param axis.display \code{logical}: display axis
  #' @param axis.labs \code{logical}: display labs
  #' @param unit \code{character}: setting name of unit; default: \code{'px'}
  #' @param scaling \code{numeric}: scaling factor for unit; default: \code{1}
  #' @param scale.bar \code{logical}: show scalebar; default: \code{FALSE}
  #' @param scale.width \code{numeric}: width of scalebar; default: \code{40}
  #' @param scale.height \code{numeric}: height of scalebar; default: \code{10}
  #' @param scale.x \code{numeric}: distance from left border of the image towards scalebar
  #' @param scale.y \code{numeric}: distance from bottom border of the image towards scalebar
  #' @param scale.color \code{character}: specify color from R-color palette or hexcode
  #' @return ggplot2 plot-object

  # get user input
  pars.list.user <- list(...)
  if (length(pars.list.user) == 0) {
    pars.list <- pars.list.default
  } else {
    if (!is.null(pars.list.user$all.list)) {
      pars.list.user <- pars.list.user[-which(names(pars.list.user) == "all.list")]
      pars.list.user <- pars.list.user[[1]]
    }
    # match user and default values
    pars.list <- transfer_pars(pars.list.user,pars.list.default)
  }


  # check image path
  if (is.null(pars.list$image)) {
    stop('Image file not specified.')
  }

  # check frame
  if (is.null(pars.list$frame)) {
    stop('Frame not specified.')
  }
  # check if mapping parameter is specified
  if (is.null(pars.list$par.map)) {
    pars.list$par.map <- colnames(df)[5]
    if (is.character(pars.list$par.map)) {
      warning('par.map not specified...\n',
              paste('\tdefaulted to:', pars.list$par.map,'\n'),
              '\tassuming: df(id, time, X, Y, mapping_parameters, ...)', call. = FALSE)
    } else {
      pars.list$par.map <- NULL
      warning('par.map not specified...\n','no mapping parameter found\n',
              'color mapping disabled...', call. = FALSE)
    }
  }

  if (is.numeric(df[pars.list$par.map] %>% pull())) {
    pars.list$par.max <- df %>% select(c(pars.list$par.map)) %>% pull() %>% max(na.rm = TRUE)
  }

  # read in image
  image <- image_read(pars.list$image) %>% image_normalize()
  pars.list$width <- image_info(image) %>% select(width) %>% pull()
  pars.list$height <- image_info(image) %>% select(height) %>% pull()

  # get cropping pars
  pars.list$crop_pars <- get_crop_pars(df,pars.list)
  #return(pars.list)
  # image processing
  image <- process_img(df,image, pars.list)

  # plot according to parameters
  suppressMessages(suppressWarnings(
    if (pars.list$sub.img) {
      return(plot_frame_sub(df, image, pars.list))
    } else {
      return(plot_frame(df, image, pars.list))
    }
  ))

}
