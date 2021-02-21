\name{psm_plot}
\alias{psm_plot}

\title{
Plot of the van Westendorp Price Sensitivity Meter Analysis (PSM)
}

\description{
\code{psm_plot()} uses \code{ggplot()} to show the standard van
  Westendorp Price Sensitivity Meter plot that allows to see the
  acceptance for each price on each of the four variables.

  It takes the object created by \code{psm_analysis()} or
  \code{psm_analysis_weighted()} as an input.
}

\usage{
psm_plot(psm_result,
         shade_pricerange = TRUE,
         line_toocheap = TRUE,
         line_tooexpensive = TRUE,
         line_notcheap = TRUE,
         line_notexpensive = TRUE,
         point_idp = TRUE,
         label_idp = TRUE,
         point_opp = TRUE,
         label_opp= TRUE,
         pricerange_color = "grey50",
         pricerange_alpha = 0.3,
         line_color = c("too cheap" = "#009E73",
                        "not cheap" = "#009E73",
                        "not expensive" = "#D55E00",
                        "too expensive" = "#D55E00"),
         line_type = c("too cheap" = "dotted",
                       "not cheap" = "solid",
                       "not expensive" = "solid",
                       "too expensive" = "dotted"))
}

\arguments{
  \item{psm_result}{Result of a Price Sensitivity Meter analysis,
  created by running \code{psm_analysis()} or
  \code{psm_analysis_weighted()}. (Object of class \code{"psm"})}
  \item{shade_pricerange}{logical value. Determines if the
  acceptable price range is shown as a shaded area or not.}
  \item{line_toocheap}{logical value. Determines if the line
  for the "too cheap" price curve is shown or not.}
  \item{line_tooexpensive}{logical value. Determines if the
  line for the "too expensive" price curve is shown or not.}
  \item{line_notcheap}{logical value. Determines if the line
  for the "not cheap" price curve is shown or not.}
  \item{line_notexpensive}{logical value. Determines if the
  line for the "not expensive" price curve is shown or not.}
  \item{point_idp}{logical value. Determines if the
  Indifference Price Point is shown or not.}
  \item{label_idp}{logical value. Determines if the label for
  the Indifference Price Point is shown or not.}
  \item{point_opp}{logical value. Determines if the Optimal
  Price Point is shown or not.}
  \item{label_opp}{logical value. Determines if the label for
  the Optimal Price Point is shown or not.}
  \item{pricerange_color}{character, specifying the
  background color for the accepted price range. Can be a
  hex color (e.g. "#7f7f7f") or one of R's built-in colors
  (e.g. "grey50"). You can see all of R's built-in colors
  with the function \code{colors()}. Is only applied if
  \code{shade_pricerange = TRUE}}
  \item{pricerange_alpha}{numeric between 0 and 1,
  specifying the alpha transparency for the shaded area of
  the the accepted price range. Is only applied if
  \code{shade_pricerange = TRUE}}
  \item{line_color}{character vector, specifying the line
  color for each of the price curves shown. Color
  definitions must match the lines you have defined via
  \code{line_toocheap, line_tooexpensive, line_notcheap}
  and \code{line_notexpensive}. Can be a hex color (e.g.
  "#7f7f7f") or one of R's built-in colors (e.g. "grey50").}
  \item{line_type}{vector, specifying the line type for each
  of the price curves shown. Definitions must match the lines
  you have defined via \code{line_toocheap, line_tooexpensive,
  line_notcheap} and \code{line_notexpensive}. Values must
  match ggplot2's expectations for line types: An integer (0:8),
  a name (blank, solid, dashed, dotted, dotdash, longdash,
  twodash), or a string with an even number (up to eight) of
  hexadecimal digits which give the lengths in consecutive
  positions in the string.}
}

\value{
The function output is a ggplot2 object.
}

\references{
  Van Westendorp, P (1976) "NSS-Price Sensitivity Meter (PSM) --
  A new approach to study consumer perception of price"
  \emph{Proceedings of the ESOMAR 29th Congress}, 139--167. Online
  available at \url{https://www.researchworld.com/a-new-approach-to-study-consumer-perception-of-price/}.
}

\seealso{
The vignette "Visualizing PSM Results" shows a similar way and more custom way to plot the data.
}

\examples{
# set up example data and run psm_analysis()

tch <- round(rnorm(n = 250, mean = 5, sd = 0.5), digits = 2)
ch <- round(rnorm(n = 250, mean = 8.5, sd = 0.5), digits = 2)
ex <- round(rnorm(n = 250, mean = 13, sd = 0.75), digits = 2)
tex <- round(rnorm(n = 250, mean = 17, sd = 1), digits = 2)

output_psm_demo <- psm_analysis(toocheap = tch,
  cheap = ch,
  expensive = ex,
  tooexpensive = tex)

# create the plot (note that ggplot's convention
# is to *not* show it by default)
\dontrun{psm_result_plot <- psm_plot(output_psm_demo)

# to show the plot, call the object (and maybe
# additional ggplot functions if you like)
psm_result_plot + ggplot2::theme_minimal()}
}

