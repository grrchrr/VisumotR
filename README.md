# VisumotR - Visualizing Motility Parameters on Images in R
<!-- badges: start -->
![](https://img.shields.io/badge/lifecycle-alpha-orange.svg)
<!-- badges: end -->
 
VisumotR allows to visualize time-resolved motility parameters or any other summary statistic onto images in R. Originally, this package was intended to be used with live-cell microscopy images and cell-tracking data. But in general VisumotR can be used with any kind of data supplying a dataframe with trackIds, time and spatial coordinates (2D/3D), as well as numeric or discrete mapping parameters and corresponding images.

![HIV-1 infected CD4+ T-cells in an complex collagen environment](images/animation.png)

The package acts as a wrapper for annotating provided images with several, modifiable ggplot2-layers. With `visumot_frame()`, continous and discrete parameters can be mapped individually on color, shape and size for one timepoint. `visumot_summary()` allows to create a corresponding plot of all given statistics for the whole range of the provided data. A time-resolved image-series can be created with `visumot_all()` which allows to combine mapped frames and summary plots created by `visumot_frame()` and `visumot_summary()`. The ouput of all functions are ggplot2-objects which can further be manipulated in a common manner.

## Display modes

### Mapping color and shape
VisumotR allows for several display modes. The most common is shown above where the complete image is captured and one continous parameter is mapped on the track color. In addition, it is also possible to map discrete variables on shape and/or color or continous variables on size and/or alpha. In this example, contact-state and infection-status of tracked cells were mapped to color and shape.

![Mapping contact state and infection state](images/shape_color.jpg)

### Color-mapping combined with summary statistics
Each output-type of `visumot_frame()` can be combined with the output from `visumot_summary()`.

![Output from visumot_frame() combined with visumot_summary()](images/frame_summary.jpg)

### Follow single tracks within sub-windows
The output of `visumot_frame()` does not need to be the whole image. It can be cropped manually or automatically or create sub-windows that allow to follow individual tracks, that might be interesting due to prior perfomed statistical analysis that highlighted for example outliers.

![Sub-window output](images/visu_sub.png)

### 3D images and Z-projections
VisumotR supports .tiff-stacks of 3D image data as input. Since ggplot2 just works in 2D, the user can choose from different Z-projections to view the data in 2D. All output modes work with 3D-image data. In addition, it is possible to follow several individual tracks on their respective Z-axis in parallel.

## Install VisumotR
```{r}
# install dependencies which are not found on CRAN
devtools::install_github("zeehio/facetscales")
remotes::install_github("coolbutuseless/ggecho")

# install VisumotR from git repo
devtools::install_git("https://github.com/grrchrr/VisumotR")
```
