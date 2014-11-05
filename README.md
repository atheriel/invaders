# invaders: An R package for previewing colour palettes using pixel art.

In the spirit of terminal colourscheme display scripts, this package provides a
small number of functions for previewing colour palettes using pixel art. It
has built-in functionality for working with RColorBrewer, but any user-defined
colour palettes will work just as well. The package is named for the various
`invaders.sh` scripts that inspired it.

## Installation

The package is not available from CRAN, but you can install it from source as
follows:

```r
devtools::install_github("atheriel/invaders")
```

## Usage

`invaders` is really for visualizing colour palettes, and a good place to start
is with those provided by `RColorBrewer`:

```r
library(RColorBrewer)
library(invaders)

# These two are equivalent:
invaders::display_colours(brewer.pal(12, "Paired"), jitter = FALSE)
invaders::display_brewer("Paired", jitter = FALSE)
```

## License

The code is licensed under the MIT License. See the `LICENSE` file for details.
