\name{summary.psm}
\alias{summary.psm}

\title{
Summarizing Price Sensitivity Meter Analyses
}
\description{
\code{summary} method for class \code{"psm"}.
}
\usage{
\method{summary}{psm}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"psm"}, usually a
  result of a call to \code{\link{psm_analysis}} or
  \code{\link{psm_analysis_weighted}}.}
  \item{\dots}{further arguments from other methods - will be
  ignored in the summary}
}

\seealso{
A comprehensive description of all elements of an object of
  class \code{"psm"} can be found in \code{\link{psm_analysis}}
  and \code{\link{psm_analysis_weighted}}.

The generic \code{\link{summary}} function.
}

\references{
  Van Westendorp, P (1976) "NSS-Price Sensitivity Meter (PSM) --
  A new approach to study consumer perception of price"
  \emph{Proceedings of the 29th ESOMAR Congress}, 139--167. Online
  available at \url{https://archive.researchworld.com/a-new-approach-to-study-consumer-perception-of-price/}.

  Newton, D, Miller, J, Smith, P, (1993) "A market acceptance
  extension to traditional price sensitivity measurement"
  \emph{Proceedings of the American Marketing Association
  Advanced Research Techniques Forum}.
}
