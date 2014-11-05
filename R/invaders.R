#' @export
display_colours <- function(colours, jitter = TRUE) {
    # Check the colours argument.
    if (!is.character(colours))
        stop("The colours argument must be a character vector.")
    if (length(colours) %% 2 != 0) {
        warning("The colour palette must have even length. Truncating it.")
        colours <- colours[-1]
    }
    
    if (is.logical(jitter))
        j.amount <- ifelse(jitter, 0.15, 0.0)
    else if (is.numeric(jitter) && jitter >= 0)
        j.amount <- jitter
    else
        stop("The jitter argument must be TRUE, FALSE, or a positive numeric.")
    
    # Compute the number of aliens to print, then sample from the available
    # list of alien portraits.
    size <- length(colours) / 2
    aliens <- sample(ALIENS, size,
                     replace = ifelse(size > length(ALIENS), TRUE, FALSE))
    ids <- sample(LETTERS, size)
    
    flatten_ <- function(mat, id) {
        result <- to_coordinates(mat)
        result$id <- id
        result
    }
    
    if (requireNamespace("data.table", quietly = TRUE)) {
        alien.list <- data.table::rbindlist(mapply(
            flatten_, mat = aliens, id = ids, SIMPLIFY = FALSE
        ))
    } else {
        alien.list <- do.call("rbind", mapply(
            flatten_, mat = aliens, id = ids, SIMPLIFY = FALSE
        ))
    }
    
    colour.map <- data.frame(
        value = 0:1,
        id = rep(ids, each = 2),
        colour = seq(0, by = 1, length.out = 2 * length(ids))
    )
    
    final <- merge(alien.list, colour.map, by = c("id", "value"), all = TRUE)
    
    if (j.amount != 0) {
        final$colour <- jitter(final$colour, amount = j.amount)
    }
    
    g <-
        ggplot2::ggplot(final, ggplot2::aes(x = x, y = y, fill = colour)) +
        ggplot2::geom_tile(colour = "gray65", size = 0.01) +
        ggplot2::scale_fill_gradientn(colours = colours) +
        ggplot2::scale_y_continuous(expand = c(0, 0), trans = "reverse") +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::facet_wrap(~ id, nrow = 2) +
        ggplot2::coord_equal() +
        ggplot2::theme(
            axis.text = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_blank(),
            legend.position = "none",
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = NA, colour = NA))
    g
}
