# VisumotR - Visualizing Motility Parameters on Images in R

VisumotR allows to visualize time-resolved motility parameters or any other summary statistic onto images in R. Originally this package was intended to be used on live-cell microscopy image and tracking data, but it can be used with any kind of data supplying a dataframe with track Ids, time, X and Y coordinates, as well as numeric or discrete mapping parameters and corresponding images.

![HIV-1 infected CD4+ T-cells in an complex collagen environment](images/example_visumotr-1.gif)

The package acts as a wrapper for annotating provided images with several, modifiable ggplot2-layers. With visumot_frame, continous and discrete parameters can be mapped individually on color, shape and size for one timepoint. visumot_summary() allows to create a corresponding plot of all given statistics for the whole range of the provided data. A time-resolved image-series can be created with visumot_all which allows to combine mapped frames and summary plots created by visumot_frame and visumot_summary. The ouput of all functions are ggplot2-objects which can further be manipulated in a common manner.

# visumot_frame()


# visumot_summary()

# visumot_all()

