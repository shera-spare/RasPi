#' Get CPU temperature of Raspberry Pi in degree celsius
#'
#' @return
#' @export
#'
#' @examples
get_cpu_temp_c <- function(){
# Read data. Should only work on raspberries ------------------------------
  tryCatch(
    cpu_temp <- as.numeric(
          unlist(
              read.table(file = "/sys/class/thermal/thermal_zone0/temp")
          )
      ),
    warning = function(w){
      cpu_temp <<- 0L
    },
    error = function(e){
      cpu_temp <<- 0L
    }
  )
  # Value is in millidegree celsius
  cpu_temp_in_c <- cpu_temp / 1000
  return(cpu_temp_in_c)
}
