#' @export
display_brewer <- function(palette, jitter = TRUE) {
    # First, check for RColorBrewer.
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
        stop("The RColorBrewer package is required to use this function.")
    }

    # Check the palette argument.
    if (!is.character(palette))
        stop("The palette argument must be a string.")
    if (is.na(brewer.pal.info[palette, 'maxcolors']))
        stop(paste(palette, "is not a recognized RColorBrewer palette."))

    # Extract the full palette of colours from RColorBrewer.
    colours <- brewer.pal(brewer.pal.info[palette, 'maxcolors'], palette)

    # Call the main display function with the colour parameter set by the
    # RColorBrewer palette.
    display_colours(colours, jitter)
}

#' @export
display_colours <- function(colours, jitter = TRUE) {
    # Check the colours argument.
    if (!is.character(colours))
        stop("The colours argument must be a character vector.")
    if (length(colours) %% 2 != 0) {
        warning("The colour palette must have even length. Truncating it.")
        colours <- colours[-1]
    }

    # Check the jitter argument.
    if (is.logical(jitter)) j.amount <-
        ifelse(jitter, getOption("invaders.default.jitter", 0.15), 0.0)
    else if (is.numeric(jitter) && jitter >= 0)
        j.amount <- jitter
    else
        stop("The jitter argument must be TRUE, FALSE, or a positive numeric.")

    # Abstract away from the default sprite list (so that it is one day
    # possible to supply one's own spites).
    sprite.list <- INVADERS

    # Compute the number of sprites to print, then sample from the available
    # list of sprites.
    size <- length(colours) / 2
    sprites <-
        sample(sprite.list, size,
               # If there are a small number of tiles, things are more
               # interesting if the sprites are more diverse. So no replace.
               replace = ifelse(size > length(sprite.list), TRUE, FALSE))
    
    # Create a list of sprite identifiers.
    ids <- LETTERS[1:size]

    # A helper function for flattening matrices into melted data frames, with
    # a unique identifier for each sprite that is later used in faceting.
    flatten_ <- function(mat, id) {
        result <- to_coordinates(mat)
        result$id <- id
        result
    }

    # Append all of the sprites together after they have been reshaped into
    # data frames.
    if (requireNamespace("data.table", quietly = TRUE)) {
        alien.list <- data.table::rbindlist(mapply(
            flatten_, mat = sprites, id = ids, SIMPLIFY = FALSE
        ))
    } else {
        # When data.table::rbindlist is not available, fall back on the
        # (slower) base R method.
        alien.list <- do.call("rbind", mapply(
            flatten_, mat = sprites, id = ids, SIMPLIFY = FALSE
        ))
    }

    # Define a mapping of pixel values and sprite identifiers to colours.
    colour.map <- data.frame(
        value = 0:1,
        id = rep(ids, each = 2),
        colour = seq(0, by = 1, length.out = 2 * length(ids))
    )

    # Merge this into the final data set for plotting.
    final <- merge(alien.list, colour.map, by = c("id", "value"), all = TRUE)

    # Adjust the colour values if a jitter parameter has been supplied.
    if (j.amount != 0) {
        final$colour <- jitter(final$colour, amount = j.amount)
    }

    # Set the number of rows in the output, using the package option as a rule.
    n.rows <- ifelse(size < getOption("invaders.wrap.at", 5), 1, 2)

    # Create and return a ggplot2 object.
    g <-
        ggplot2::ggplot(final, ggplot2::aes(x = x, y = y, fill = colour)) +
        ggplot2::geom_tile(colour = "gray65", size = 0.01) +
        ggplot2::scale_fill_gradientn(colours = colours) +
        ggplot2::scale_y_continuous(expand = c(0, 0), trans = "reverse") +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::facet_wrap(~ id, nrow = n.rows) +
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
