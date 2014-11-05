#' invaders.
#'
#' @name invaders
#' @docType package
NULL

# Define the package options using .onLoad
.onLoad <- function(libname, pkgname) {
    # Specify options for this package.
    op.invaders <- list(
        invaders.default.jitter = 0.15,
        invaders.wrap.at = 5
    )
    
    # Check for options collisions.
    toset <- !(names(op.invaders) %in% names(options()))
    
    # Make options available (unless there is a conflict).
    if(any(toset)) options(op.invaders[toset])
    
    invisible()
}
