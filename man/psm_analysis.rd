\name{psm_analysis}
\alias{psm_analysis}

\title{
Van Westendorp Price Sensitivity Meter Analysis (PSM)
}

\description{
\code{psm_analysis()} performs an analysis of consumer price
  preferences and price sensitivity known as \bold{van
  Westendorp Price Sensitivity Meter (PSM)}. It takes respondent's
  price preferences (from survey data) as an input and estimates
  acceptable price ranges and price points. For a description of
  the method see the \emph{Details} section.
}

\usage{
psm_analysis(
  toocheap, cheap, expensive, tooexpensive,
  data = NA,
  validate = TRUE,
  interpolate = FALSE,
  intersection_method = "min",
  pi_cheap = NA, pi_expensive = NA,
  pi_scale = 5:1,
  pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0))
}

\arguments{
  \item{toocheap, cheap, expensive, tooexpensive}{If a
  data.frame/matrix is provided in the \code{data} argument:
  names of the variables in the data.frame/matrix that contain
  the survey data on the respondents' "too cheap", "cheap",
  "expensive" and "too expensive" price preferences.

  If no data.frame/matrix is provided in the \code{data}
  argument: numeric vectors that directly include this
  information. If numeric vectors are provided, it is assumed
  that they are sorted by respondent ID (the preferences for
  respondent \code{n} are stored at the \code{n}-th position in
  all vectors).

  If the \code{toocheap} price was not assessed, a
  variable/vector of NAs can be used instead. This
  variable/vector needs to have the same length as the other
  survey information. If \code{toocheap} is NA for all cases,
  it is possible to calculate the Point of Marginal
  Expensiveness and the Indifference Price Point, but it is
  impossible to calculate the Point of Marginal Cheapness and
  the Optimal Price Point.}
  \item{data}{data.frame or matrix that contains the function's
  input data. \code{data} input is not mandatory: Instead of
  using one data.frame/matrix as an input, it is also possible
  to provide the data directly as vectors in the "too cheap",
  "cheap", "expensive" and "too expensive" arguments.}
  \item{validate}{logical. should only respondents with
  consistent price preferences (too cheap < cheap < expensive
  < too expensive) be considered in the analysis?}
  \item{interpolate}{logical. should interpolation of the price
  curves be applied between the actual prices given by the
  respondents? If interpolation is enabled, the output appears
  less bumpy in regions with sparse price information. If the
  sample size is sufficiently large, interpolation should not
  be necessary.}
  \item{intersection_method}{"min" (default), "max", "mean" or
  "median". defines the method how to determine the price
  points (range, indifference price, optimal price) if there
  are multiple possible intersections of the price curves.
  "min" uses the lowest possible prices, "max" uses the
  highest possible prices, "mean" calculates the mean among
  all intersections and "median" uses the median of all
  possible intersections}
  \item{pi_cheap, pi_expensive}{Only required for the Newton
  Miller Smith extension. If \code{data} argument is provided:
  names of the variables in the data.frame/matrix that contain
  the survey data on the respondents' purchase intent at their
  individual cheap/expensive price.}
  \item{pi_scale}{Only required for the Newton Miller Smith
  extension. Scale of the purchase intent variables pi_cheap and
  pi_expensive. By default assuming a five-point scale with 5
  indicating the highest purchase intent.}
  \item{pi_calibrated}{Only required for the Newton Miller Smith
  extension. Calibrated purchase probabilities that are assumed
  for each value of the purchase intent scale. Must be the same
  order as the pi_scale variable so that the first value of
  pi_calibrated corresponds to the first value in the pi_scale
  variable. Default values are taken from the Sawtooth Software
  PSM implementation in Excel: 70\% for the best value of the
  purchase intent scale, 50\% for the second best value,
  30\% for the third best value (middle of the scale), 10\%
  for the fourth best value and 0\% for the worst value.}
}


\details{
The Price Sensitivity Meter method for the analysis of consumer
price preferences was proposed by the Dutch economist Peter van
Westendorp in 1976 at the ESOMAR conference. It is a
survey-based approach that has become one of the standard price
acceptance measurement techniques in the market research
industry and is still widely used for during early-stage product
development.

Price acceptance and price sensitivity are measured in van
Westendorp's approach by four open-ended survey questions:

\itemize{
\item At which price on this scale are you beginning to
experience \dots (test-product) as cheap?
\item At which price on this scale are you beginning to
experience \dots (test-product) as expensive?
\item At which price on this scale you are beginning to
experience \dots (test-product) as too expensive -- so that you
would never consider buying it yourself?
\item At which price on this scale you are beginning to
experience \dots (test-product) as too cheap -- so that you say
"at this price the quality cannot be good"?
}

Respondents with inconsistent price preferences (e.g. "cheap"
price larger than "expensive" price) are usually removed from
the data set. This function has built-in checks to detect
invalid preference structures and removes those respondents from
the analysis by default.

To analyze price preferences and price sensitivity, the method
uses cumulative distribution functions for each of the
aforementioned price steps (e.g. "how many respondents think
that a price of \code{x} \emph{or more} is expensive?"). By
convention, the distributions for the "too cheap" and the
"cheap" price are inverted. This leads to the interpretation
"how many respondents think that a price of \emph{up to}
\code{x} is (too) cheap?".

The interpretation is built on the analysis of the intersections
of the four cumulative distribution functions for the different
prices (usually via graphical inspection). The original paper
describes the four intersections as follows:

\itemize{
\item \bold{Point of Marginal Cheapness (MGP)}: Below this price
point, there are more respondents that consider the price as
"too cheap" than respondents who consider it as "not cheap"
(intersection of "too cheap" and "not cheap"). This is interpreted
as the lower limit of the range of acceptable prices.
\item \bold{Point of Marginal Expensiveness (MEP)}. Above this
price point, there are more respondent that consider the price
as "too expensive" than there are respondents who consider it as
"not expensive" (intersection of "not expensive" and "too
expensive"). This is interpreted as the upper limit of the
range of acceptable prices.
\item \bold{Indifference Price Point (IDP)}: The same number of
respondents perceives the price as "cheap" and "expensive"
(intersection of "cheap" and "expensive"). In van Westendorp's
interpretation, this is either the median price paid in the
market or the price of an important market-leader.
\item \bold{Optimal Price Point (OPP)}: The same number of
respondents perceives the product as "too cheap" and "too
expensive" (intersection of "too cheap" and "too expensive").
van Westendorp argues that this is the value for which the
respondents' resistance against the price is particularly low.
}

Besides those four intersections, van Westendorp's article
advises to analyze the cumulative distribution functions for
steep areas which indicate price steps.

To analyze trial rates and estimate revenue forecasts,
Newton/Miller/Smith have extended van Westendorp's original
model by adding two purchase intent questions that are asked for
the respondent's "cheap" and "expensive" price. The purchase
probability at the respondent's "too cheap" and "too expensive"
price are defined as \code{0}. The main logic is that the "too
expensive" price point is prohibitively expensive for the
respondent and a price at the "too cheap" price level raises
doubts about the product quality.

By combining the standard van Westendorp questions with those
two additional purchase intent questions, it becomes possible to
summarize the purchase probabilities across respondents (using
linear interpolation for the purchase probabilities between each
respondent's cornerstone prices). The maximum of this curve is
then defined as the price point with the highest expected trial
rate. Moreover, by multiplying the trial rate with the price, it
also becomes possible to estimate a price with the highest
expected revenue.

It has to be noted that the van Westendorp Price Sensitivity
Meter is useful in some cases, but does not answer every
pricing-related question. It may be a good tool to assess very
broadly if the consumers' price perceptions exceed the actual
production costs. For more complex analyses (e.g. defining
specific prices for different products to avoid cannibalization
and drive at the same time incremental growth), other
methodological approaches are needed.
}

\value{
The function output consists of the following elements:

    \item{\code{data_input}:}{\code{data.frame} object. Contains
    the data that was used as an input for the analysis.}
    \item{\code{validated}:}{\code{logical} object. Indicates
    whether the \code{"validate"} option has been used (to
    exclude cases with intransitive price preferences).}
    \item{\code{invalid_cases}:}{\code{numeric} object. Number
    of cases with intransitive price preferences.}
    \item{\code{total_sample}:}{\code{"numeric"} object.
    Total sample size of the input sample \emph{before}
    assessing the transitivity of individual price preferences.}
    \item{\code{data_vanwestendorp}:}{\code{data.frame} object.
    Output data of the Price Sensitivity Meter analysis.
    Contains the cumulative distribution functions for the four
    price assessments (too cheap, cheap, expensive, too
    expensive) for all prices.}
    \item{\code{pricerange_lower}:}{\code{numeric} object. Lower
    limit of the acceptable price range as defined by the
    Price Sensitivity Meter, also known as \bold{point of
    marginal cheapness}: Intersection of the "too cheap" and the
    "not cheap" curves.}
    \item{\code{pricerange_upper}:}{\code{numeric} object. Upper
    limit of the acceptable price range as defined by the Price
    Sensitivity Meter, also known as \bold{point of marginal
    expensiveness}: Intersection of the "too expensive" and the
    "not expensive" curves.}
    \item{\code{idp}:}{\code{numeric} object. \bold{Indifference
    Price Point} as defined by the Price Sensitivity Meter:
    Intersection of the "cheap" and the "expensive" curves.}
    \item{\code{opp}:}{\code{numeric} object. \bold{Optimal
    Price Point} as defined by the Price Sensitivity Meter:
    Intersection of the "too cheap" and the "too expensive"
    curves.}
    \item{\code{NMS}:}{\code{logical} object. Indicates whether
    the additional analyses of the Newton Miller Smith Extension
    were performed.}
    \item{\code{weighted}:}{\code{logical} object. Indicates
    if weighted data was used in the analysis. Outputs from
    \code{psm_analysis()} always have the value \code{FALSE}.
    When data is weighted, use the function
    \code{\link{psm_analysis_weighted}.}}
    \item{\code{data_nms}:}{\code{data.frame} object. Output of
    the Newton Miller Smith extension: calibrated mean
    purchase probabilities for each price point.}
    \item{\code{pi_scale}:}{\code{data.frame} object. Shows the
    values of the purchase intent variable and the
    corresponding calibrated purchase probabilities as defined
    in the function input for the Newton Miller Smith
    extension.}
    \item{\code{price_optimal_trial}:}{\code{numeric} object.
    Output of the Newton Miller Smith extension: Estimate for
    the price with the highest trial rate.}
    \item{\code{price_optimal_revenue}:}{\code{numeric} object.
    Output of the Newton Miller Smith extension:
    Estimate for the price with the highest revenue (based on
    the trial rate).}
}

\references{
  Van Westendorp, P (1976) "NSS-Price Sensitivity Meter (PSM) --
  A new approach to study consumer perception of price"
  \emph{Proceedings of the ESOMAR 29th Congress}, 139--167. Online
  available at \url{https://rwconnect.esomar.org/a-new-approach-to-study-consumer-perception-of-price/}.

  Newton, D, Miller, J, Smith, P, (1993) "A market acceptance
  extension to traditional price sensitivity measurement"
  \emph{Proceedings of the American Marketing Association
  Advanced Research Techniques Forum}.

  Sawtooth Software (2016) "Templates for van Westendorp PSM for
  Lighthouse Studio and Excel". Online available at
  \url{https://www.sawtoothsoftware.com/204-about-us/news-and-events/sawtooth-solutions/1759-templates-for-van-westendorp-psm-for-lighthouse-studio-excel}
}

\seealso{
The function \code{psm_analysis_weighted()} performs the same
analyses for weighted data.
}

\examples{

set.seed(42)

# standard van Westendorp Price Sensitivity Meter Analysis
# input directly via vectors

tch <- round(rnorm(n = 250, mean = 5, sd = 0.5), digits = 2)
ch <- round(rnorm(n = 250, mean = 8.5, sd = 0.5), digits = 2)
ex <- round(rnorm(n = 250, mean = 13, sd = 0.75), digits = 2)
tex <- round(rnorm(n = 250, mean = 17, sd = 1), digits = 2)

output_psm_demo1 <- psm_analysis(toocheap = tch,
  cheap = ch,
  expensive = ex,
  tooexpensive = tex)

# additional analysis with Newton Miller Smith Extension
# input via data.frame

pint_ch <- sample(x = c(1:5), size = length(tex),
  replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3))

pint_ex <- sample(x = c(1:5), size = length(tex),
  replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1))

data_psm_demo <- data.frame(tch, ch, ex, tex, pint_ch, pint_ex)

output_psm_demo2 <- psm_analysis(toocheap = "tch",
  cheap = "ch",
  expensive = "ex",
  tooexpensive = "tex",
  pi_cheap = "pint_ch",
  pi_expensive = "pint_ex",
  data = data_psm_demo)

summary(output_psm_demo2)
}

