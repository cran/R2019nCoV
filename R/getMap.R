#' Getting real-time monitoring map of the 2019-nCoV virus
#'
#' @return a ggplot2 object which shows the real-time monitoring map of the 2019-nCoV virus
#' @export
#' @import dplyr
#' @import ggplot2
#' @import pinyin
#' @import maps
#' @param x a R2019-nCoV object, i.e. the result of function get_2019nCoV
#' @examples
#'
#' x <- get_2019nCoV()
#' map <- getMap(x)
getMap <- function(x) {
    world_map <- map_data("world")
    df <- getDataFrame(x)
    df$province <- pinyin::py(df$name, sep = "", dic = pinyin::pydic(method = "toneless",
        dic = "pinyin2"))
    df <- df %>% dplyr::arrange(df$name)
    df$province[24] <- "shannxi"
    china_line_data <- china_line_data
    china_map_data <- china_map_data
    china_data <- dplyr::full_join(china_map_data, df, by = "province")
    china_data <- china_data %>% dplyr::mutate(value = china_data$total$confirm)
    temp <- cut(china_data$value, c(0, 10, 100, 500, 1000, 10000, Inf))
    levels(temp) <- c("0-10", "11-100", "101-500", "501-1000", "1001-10000", "> 10000   ")
    china_data <- china_data %>% dplyr::mutate(value_factor = temp)
    p <- ggplot() +
      geom_polygon(data=world_map,
                   aes(x = world_map$long, y = world_map$lat, group = world_map$group),
                   colour = "grey20",
                   fill = "#EAEAEA",
                   size = .1) +
      geom_path(data = china_line_data,
                aes(x = china_line_data$long, y = china_line_data$lat, group = china_line_data$group),
                colour = "black", size = 0.5) +
      geom_polygon(data = china_data,
                   aes(x = china_data$long, y = china_data$lat, group = china_data$group, fill = china_data$value_factor),
                   colour = "grey10", size = 0.3) +
      scale_fill_manual(breaks = levels(temp),
                        labels = levels(temp),
                        values = c("#F8E2CF", "#E3B1AB", "#D58877", "#B53E3C", "#622E2F", "#360D0E"),
                        na.value = "#FEFFE8") +
      coord_quickmap() +
      theme_void() +
      theme(legend.title = element_blank(), legend.box.just = "left",
            legend.key.height = unit(0.6, "cm"), legend.key.width = unit(0.4, "cm"),
            plot.background = element_rect(fill = "lightskyblue")) +
      xlim(70, 140) + ylim(5, 55)
    return(p)
}


