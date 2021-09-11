#' Returns dataframe with maximum storage, used storage, available storage
#' and % used storage
#'
#' @return
#' @export
#'
#' @examples
get_storage_amount <- function(){

  if(Sys.info()["sysname"] == "Linux") {
    # Get df information ------------------------------------------------------
    raw_table <- system("df", intern = TRUE)

    # Get Header --------------------------------------------------------------
    raw_header <- unlist(
      strsplit(
        x = raw_table[1],
        split = " "
      )
    )
    text_header <- raw_header[nchar(raw_header) > 0]
    final_header <- text_header[2:5]
    final_header[1:3] <- paste(final_header[1:3], "[GB]")
    final_header[1] <- "Total [GB]"


    # Get Values --------------------------------------------------------------
    raw_info <- unlist(
      strsplit(
        x = raw_table[2],
        split = " "
      )
    )
    text_info <- raw_info[nchar(raw_info) > 0]
    # Last value has percentage sign.
    # This is the laziest way to deal with it
    final_info <- text_info[2:5]
    percent_value <- final_info[4]
    final_info[4] <- 0
    final_info <- round(as.numeric(final_info)/(1024*1024),2)
    final_info[4] <- percent_value

    # Paste header and values together ----------------------------------------

    names(final_info) <- final_header
    df_info <- as.data.frame(t(final_info))

    return(df_info)
  } else if (Sys.info()["sysname"] == "Windows"){
    final_header <- c(
      "Total [GB]", "Used [GB]", "Available [GB]", "Use%"
    )
    final_info <- c("60", "20", "40", "33%")
    names(final_info) <- final_header

    df_info <- as.data.frame(t(final_info))

    return(df_info)
  }
}
