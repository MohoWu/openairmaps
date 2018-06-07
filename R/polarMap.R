# function to plot polar plots on leaflet maps

#' Bivariate polar plots on interactive leaflet maps
#'
#' @param data A data frame. The data frame must contain the data to
#'   plot a \code{polarPlot}, which includes wind speed (\code{ws}),
#'   wind direction (\code{wd}) and the column representing the
#'   concentration of a pollutant. In addition, \code{data} must
#'   include a decimal latitude and longitude.
#' @param pollutant The column name of the pollutant to plot.
#' @param x The radial axis variable to plot.
#' @param latitude The decimal latitude.
#' @param longitude The decimal longitude.
#' @param dir_polar The location of a directory to store the polar
#'   plots that are generated. Note, if the directory does not exist
#'   it is generated. If the directory does exist all plots will be
#'   deleted when the function is run.
#' @param provider The default base map to be used. Available options: "OpenStreetMap", "Toner", "Toner lite",
#'   "Landscape", "Transport dark", "Outdoors", "Images".
#' @param type The grouping variable that provides a data set for a
#'   specific location. Often, with several sites, \code{type =
#'   "site"} is used.
#' @param cols The colours used for plotting.
#' @param alpha The alpha transparency to use for the plotting surface
#'   (a value between 0 and 1 with zero being fully transparent and 1
#'   fully opaque).
#' @param iconWidth The actual width of the plot on the map in pixels.
#' @param iconHeight The actual height of the plot on the map in pixels.
#' @param fig.width The width of the plots to be produced in inches.
#' @param fig.height The height of the plots to be produced in inches.
#' @param ... other arguements pass to \code{\link[openair]{polarPlot}}
#' @param leaflet.width The width of leaflet plot.
#' @param leaflet.height The height of leaflet plot.
#'
#' @return A leaflet object.
#' @import leaflet
#' @importFrom grDevices dev.off png
#' @export
#'
#' @examples
#' polarMap(polar_data, latitude = "latitude", longitude = "longitude",
#' x = "ws", type = "site", provider = "CartoDB.DarkMatter")
#'
polarMap <- function(data, pollutant = "nox", x = "ws",
                     latitude = "lat",
                     longitude = "lon",
                     dir_polar = "~/dir_polar",
                     provider = "Toner lite",
                     type = "default",
                     cols = "jet",
                     alpha = 1,
                     leaflet.width = 800, leaflet.height = 400,
                     iconWidth = 200, iconHeight = 200,
                     fig.width = 3, fig.height = 3, ...) {

  ## extract variables of interest
  vars <- c("wd", x, pollutant, latitude, longitude, type)

  # cut data
  data <- openair::cutData(data, type)

  # check to see if variables exist in data
  if (length(intersect(vars, names(data))) != length(vars))
    stop(paste(vars[which(!vars %in% names(data))], "not found in data"), call. = FALSE)


    # check that directory is empty / exists
  if (dir.exists(dir_polar)) {
    # remove existing files
    files <- list.files(dir_polar, full.names = TRUE)
    file.remove(files)

  } else {

    dir.create(dir_polar)

  }

  # calculate the limit used in polar plot, i.e. 10 and 90 percentile of the entire data
  polar_limit <- quantile(data[[pollutant]], c(0.1, 0.9), na.rm = TRUE)

  # function to produce a polar plot, with transparent background
  plot_polar <- function(data, same.scale = FALSE, ...) {

    if (same.scale) {

      png(paste0(dir_polar, "/", data[[type]], ".sc.png"),
          width = fig.width * 300,
          height = fig.height * 300, res = 300, bg = "transparent")

      plt <- polarPlot(data, pollutant, x,
                       key = FALSE,
                       par.settings = list(axis.line = list(col = "transparent")),
                       alpha = alpha,
                       limits = polar_limit,
                       ...)

    } else {

      png(paste0(dir_polar, "/", data[[type]], ".png"),
          width = fig.width * 300,
          height = fig.height * 300, res = 300, bg = "transparent")

      plt <- polarPlot(data, pollutant, x,
                       key = TRUE,
                       par.settings = list(axis.line = list(col = "transparent")),
                       alpha = alpha,
                       key.position = "bottom",
                       ...)

    }

    dev.off()

    return(NULL)

  }

  # go through all sites and make plot
  # plot with same scale
  split(data, data[[type]]) %>%
    purrr::walk(plot_polar, same.scale = TRUE, ...)

  # plot with individual scale
  split(data, data[[type]]) %>%
    purrr::walk(plot_polar, same.scale = FALSE, ...)

  # summarise data - one line per location
  plot_data <- group_by_(data, .dots = type) %>%
    slice(n = 1)

  # definition of 'icons' aka the openair plots
  sc_plots <- list.files(dir_polar, pattern = "\\.sc\\.", full.names = TRUE)
  reg_plots <- setdiff(list.files(dir_polar, full.names = TRUE), sc_plots)

  sc_icons <- make_leaf_icon(sc_plots, icon.name = unique(data[[type]]),
                             iconWidth, iconHeight)
  reg_icons <- make_leaf_icon(reg_plots, icon.name = unique(data[[type]]),
                             iconWidth, iconHeight)

  # plot leaflet basemap
  basegroups <- c("OpenStreetMap", "Toner", "Toner lite", "Landscape",
                  "Transport dark", "Outdoors", "Images")

  m <- leaflet(data = plot_data, width = leaflet.width, height = leaflet.height) %>%
    addTiles(group = "OpenStreetMap",
             urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner lite") %>%
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "landscape", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Landscape"
    ) %>%
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "transport-dark", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Transport dark"
    ) %>%
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "outdoors", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Outdoors"
    ) %>%
    addProviderTiles("Esri.WorldImagery", group = "Images")

  # add polarplots
  plot_values <- data[[pollutant]][dplyr::between(data[[pollutant]], polar_limit[1], polar_limit[2])]
  pal <- colorNumeric(palette = openair::openColours("jet", 10),
                      domain = plot_values,
                      na.color = NA)

  m <- m %>%
    addMarkers(plot_data[[longitude]], plot_data[[latitude]],
               icon = sc_icons, popup = plot_data[[type]], group = "Same scale") %>%
    addLegend(position = "bottomleft", pal = pal, values = plot_values, group = "Same scale") %>%
    addMarkers(plot_data[[longitude]], plot_data[[latitude]],
               icon = reg_icons, popup = plot_data[[type]], group = "Individual scale") %>%
    hideGroup("Individual scale")


  # add layer contrl
  m <- m %>%
    addLayersControl(
      baseGroups = c(provider,
                     setdiff(basegroups, provider)),
      overlayGroups = c("Same scale", "Individual scale"))

  # return
  m

}

# function to make leaf icons
make_leaf_icon <- function(file, icon.width, icon.height, icon.name) {

  leaf_icons = lapply(file,
                      makeIcon,
                      iconWidth = icon.width, iconHeight = icon.height)
  names(leaf_icons) = icon.name
  class(leaf_icons) <- "leaflet_icon_set"

  return(leaf_icons)

}


