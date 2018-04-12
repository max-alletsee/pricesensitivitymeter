\name{psm-class}
\Rdversion{1.1}
\docType{class}
\alias{psm-class}
\alias{psm.class}

\title{Class \code{"psm"}}
\description{
Class \code{"psm"} is a class for outputs of Price Sensitivity Meter analyses as performed by the \code{\link{psm_analysis}} function.

The main purpose is to create a custom \code{summary} function for objects of class \code{"psm"}.
}

\section{Objects from the Class}{
Objects are usually created as a result of a call of the \code{\link{psm_analysis}} function.
}

\section{Slots}{
  \describe{
    \item{\code{data_input}:}{Object of class \code{"data.frame"}. Contains the data that was used as an input for the analysis.}
    \item{\code{validated}:}{Object of class \code{"logical"}. Indicates whether the \code{"validate"} option of the \code{psm_analysis} function has been used (to exclude cases with intransitive price preferences).}
    \item{\code{invalid_cases}:}{Object of class \code{"numeric"}. Number of cases with intransitive price preferences.}
    \item{\code{total_sample}:}{Object of class \code{"numeric"}. Total sample size of the input sample \emph{before} assessing the transitivity of individual price preferences.}
    \item{\code{data_vanwestendorp}:}{Object of class \code{"data.frame"}. Output data of the Price Sensitivity Meter analysis. Contains the cumulative distribution functions for the four price assessments (too cheap, cheap, expensive, too expensive) for all prices.}
    \item{\code{pricerange_lower}:}{Object of class \code{"numeric"}. Lower limit of the acceptable price range as defined by the Price Sensitivity Meter, also known as \bold{point of marginal cheapness}: Intersection of the "too cheap" and the "expensive" curves.}
    \item{\code{pricerange_upper}:}{Object of class \code{"numeric"}. Upper limit of the acceptable price range as defined by the Price Sensitivity Meter, also known as \bold{point of marginal expensiveness}: Intersection of the "too expensive" and the "cheap" curves.}
    \item{\code{idp}:}{Object of class \code{"numeric"}. \bold{Indifference Price Point} as defined by the Price Sensitivity Meter: Intersection of the "cheap" and the "expensive" curves.}
    \item{\code{opp}:}{Object of class \code{"numeric"}. \bold{Optimal Price Point} as defined by the Price Sensitivity Meter: Intersection of the "too cheap" and the "too expensive" curves.}
    \item{\code{NMS}:}{Object of class \code{"logical"}. Indicates whether the additional analyses of the Newton Miller Smith Extension were performed.}
    \item{\code{data_nms}:}{Object of class \code{"data.frame"}. Output of the Newton Miller Smith extension: calibrated mean purchase probabilities for each price point.}
    \item{\code{pi_scale}:}{Object of class \code{"data.frame"}. Shows the values of the purchase intent variable and the corresponding calibrated purchase probabilities as defined in the function input for the Newton Miller Smith extension.}
    \item{\code{price_optimal_trial}:}{Object of class \code{"numeric"}. Output of the Newton Miller Smith extension: Estimate for the price with the highest trial rate.}
    \item{\code{price_optimal_revenue}:}{Object of class \code{"numeric"}. Output of the Newton Miller Smith extension: Estimate for the price with the highest revenue (based on the trial rate).}
  }
}

\section{Methods}{
\code{\link{summary.psm}}
}

\seealso{
To understand the main function that creates an object of class \code{"psm"}, see \code{\link{psm_analysis}}.

To understand how the summaries of objects of class \code{"psm"} look like, see \code{\link{summary.psm}}
}

\examples{
showClass("psm")
}

\references{
  Van Westendorp, P (1976) "NSS-Price Sensitivity Meter (PSM) --
  A new approach to study consumer perception of price"
  \emph{Proceedings of the 29th ESOMAR Congress}, 139--167. Online
  available at \url{https://rwconnect.esomar.org/a-new-approach-to-study-consumer-perception-of-price/}.

  Newton, D, Miller, J, Smith, P, (1993) "A market acceptance
  extension to traditional price sensitivity measurement"
  \emph{Proceedings of the American Marketing Association
  Advanced Research Techniques Forum}.
}
\keyword{classes}
