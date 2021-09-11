#' Create picharts for storage usage from rasppies
#'
#' @param data_set Dataset provided by bash commands.
#' @param is_hdd Whether its HDD or RAM Storage
#'
#' @return
#' @export
#'
#' @examples
plot_storage_pieplot <- function(data_set, is_hdd = TRUE){
  if(is_hdd) {
    plot_data <- data_set %>%
      dplyr::select(- `Use%`) %>%
      dplyr::mutate(`Unknown [GB]` = as.character(
        as.numeric(`Total [GB]`) - (as.numeric(`Used [GB]`) + as.numeric(`Available [GB]`))
      )
      ) %>%
      dplyr::select(- `Total [GB]`) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Status", values_to = "Amount") %>%
      dplyr::mutate(Amount = as.numeric(Amount))


    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = "", y = Amount, fill = Status)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::scale_color_manual(values = c(
        "Available [GB]" = "greenyellow",
        "Unknown [GB]" = "grey",
        "Used [GB]" = "tomato2"
      ),
      aesthetics = c("color", "fill")
      ) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    return(p)
  } else {

  }
}
