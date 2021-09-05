#' Split dive profile into dives
#'
#' @param dt Datetime (POSIXct)
#' @param depth Depth (numeric)
#' @param surface Threshold for surface activity (numeric(1))
#' @param min_depth Minimum depth for a dive (numeric(1))
#' @param min_dur Minimum duration of a dive (numeric(1), in seconds)
#'
#' @return A factor, the same length as \code{depth}, with levels corresponding
#'   to individual dives. Inter-dive periods marked as NA.
#' @export
split_dives <- function(dt, depth, surface = 0, min_depth = 0, min_dur = 0) {
  dive_start <- depth >= surface & lag(depth) < surface
  dive_start[1] <- FALSE
  dive_id <- cumsum(dive_start)
  dive_id[depth < surface | dive_id == 0] <- NA
  dive_id <- factor(dive_id)
  dive_depth <- tapply(depth, dive_id, max)
  dive_dur <- tapply(dt,
                     dive_id,
                     function(x) as.numeric(max(x) - min(x), unit = "secs"))
  not_dives <- which(dive_depth < min_depth | dive_dur < min_dur)
  dive_id[dive_id %in% not_dives] <- NA
  dive_id <- droplevels(dive_id)
  levels(dive_id) <- seq_along(levels(dive_id))
  dive_id
}

#' Normalize dive time
#'
#' Normalize the time in each dive to [0, 1] where 0 is start of dive and 1 is
#' the end.
#'
#' @param dive_id A factor of dive id's, as from \code{split_dives()}.
#'
#' @return A numeric vector, the same length as \code{dive_id}.
#' @export
normalize_dives <- function(dive_id) {
  dive_rle <- rle(as.character(dive_id))
  mapply(function(l, v) if (!is.na(v)) seq(0, 1, length.out = l) else NA,
         dive_rle$lengths,
         dive_rle$values) %>%
    unlist()
}
