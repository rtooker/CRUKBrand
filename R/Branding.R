#' Get Footnotes Text
#'
#' This function returns the footnotes text
#' @return The footnotes text
#' @keywords CRUK
#' @export
#' @examples
#' getFootnotesText()
getFootnotesText <- function() {
  footnotes <- ""
  footnotes <- c(footnotes, "Source: cruk.org/cancerstats")
  footnotes <- c(footnotes, "")
  footnotes <- c(footnotes, "You are welcome to reuse this Cancer Research UK statistics content for your own work.")
  footnotes <- c(footnotes, "Credit us as authors by referencing Cancer Research UK as the primary source.")
  footnotes <- c(footnotes, "Suggested style: Cancer Research UK, full URL of the page, Accessed [month] [year].")

  footnote <- paste(footnotes, collapse="\n")
  return (footnote)
}

#' Get Footnotes Graphical Object
#'
#' This function returns the footnotes graphical object
#' @param footNoteText If NA then will use default text
#' @return The footnotes graphical object
#' @keywords CRUK
#' @export
#' @examples
#' getFootnotesGrob()
#' getFootnotesGrob("my footnote text")
getFootnotesGrob <- function(footNoteText=NULL) {
  if (is.null(footNoteText)) {
    footnote <- getFootnotesText()
  } else {
    footnote <- footNoteText
  }

  # concatenate elements of vector with newline between each (paste) and create graphical object (textGrob)
  footnoteGrob <- textGrob(footnote, x = 0.05, y=0.7, just = "left", vjust=1, gp = gpar(fontface = "italic", fontsize = 8))
  return(footnoteGrob)
}


#' Get CRUK Logo Graphical Objects
#'
#' This function returns the CRUK Logo at 25% of the png (bottom right quarter)
#' @return The CRUK logo graphical object
#' @keywords CRUK
#' @export
#' @examples
#' getCRUKLogo()
getCRUKLogo <- function() {
  # create a little baby image - could save this with white space instead or make massive grid later
  img <- readPNG(system.file("img", "CRUKLogo.png", package="CRUKBrand"))
  logo <- rasterGrob(img, interpolate=TRUE)
  lay <- rbind(c(NA,NA),
               c(NA,1))
  gs <- list(logo)
  return(arrangeGrob(grobs = gs, layout_matrix = lay))
}

#' Apply brand to plot
#'
#' This function takes a plot and applies the CRUK brand and standard footnote text
#' @param plot the plot
#' @return A branded plot including footnotes and logo if required
#' @keywords CRUK
#' @export
#' @examples
#' g <- applyCRUKBrand(plot)
#' grid.draw(g)
applyCRUKBrand <- function (plot) {
  # basic stylings
  theme_set(theme_bw(12))

  # palette
  cbPalette <- c("#2E008B", "#EC008C", "#00B6ED", "A7A8AA", "AB99D1", "F799D1", "99E2F8")

  # plot formatting
  plot <- plot +
    theme(plot.title = element_text(lineheight=10, face="italic")) +
    scale_colour_manual(values=cbPalette)


  # https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
  # create a grid layout with 5 x 4 to combine plot, footnote, and logo
  # 1 = plot, 2 = footnotes, 3 = logo
  lay <- rbind(c(1,1,1,1,1),
               c(1,1,1,1,1),
               c(1,1,1,1,1),
               c(2,2,2,3,3))

  gs = list(plot, getFootnotesGrob(), getCRUKLogo())

  # arrange grid, use blank text to create border (better way???)
  g <- arrangeGrob(grobs=gs, layout_matrix = lay, bottom="", right="", left="", top="")
  return(g)

}
