# Turn a matrix of values into a data frame with x and y
# coordinate columns and a value column.
to_coordinates <- function(mat) {
    # Argument checking:
    if (!is.matrix(mat))
        stop("to_coordinates() must be applied to a matrix.")
    if (length(dim(mat)) != 2)
        stop("Invalid matrix dimensions (must be n x m).")
    
    result = reshape2::melt(mat)
    colnames(result) <- c("y", "x", "value")
    result
}
