\name{psm_analysis_weighted}
\alias{psm_analysis_weighted}

\title{
Weighted van Westendorp Price Sensitivity Meter Analysis (PSM)
}

\description{
\code{psm_analysis_weighted()} performs a \bold{weighted} analysis
  of consumer price preferences and price sensitivity known as
  \bold{van Westendorp Price Sensitivity Meter (PSM)}. The function
  requires a sample design from the \pkg{survey} package as the
  main input. Custom weights or sample designs from other packages
  are not supported.

  To run a PSM analysis \bold{without} weighting, use the function
  \code{\link{psm_analysis}}.

}

\usage{
psm_analysis_weighted(
  toocheap, cheap, expensive, tooexpensive,
  design,
  validate = TRUE,
  interpolate = FALSE,
  interpolation_steps = get_psm_constant("DEFAULT_INTERPOLATION_STEPS"),
  intersection_method = "min",
  acceptable_range = "original",
  pi_cheap = NA, pi_expensive = NA,
  pi_scale = get_psm_constant("NMS_DEFAULTS.PI_SCALE"),
  pi_calibrated = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED"),
  pi_calibrated_toocheap = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOCHEAP"), 
  pi_calibrated_tooexpensive = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOEXPENSIVE")
  )
}

\arguments{
  \item{toocheap, cheap, expensive, tooexpensive}{Names
  of the variables in the data.frame/matrix that contain the
  survey data on the respondents' "too cheap", "cheap",
  "expensive" and "too expensive" price preferences.

  If the \code{toocheap} price was not assessed, a
  variable of NAs can be used instead.  If \code{toocheap}
  is NA for all cases, it is possible to calculate the Point of
  Marginal Expensiveness and the Indifference Price Point, but it
  is impossible to calculate the Point of Marginal Cheapness and
  the Optimal Price Point.}
  \item{design}{A survey design which has been created by the
  function \code{\link[survey]{svydesign}()} from the \pkg{survey}
  package. The data that is used as an input of \code{svydesign()}
  must include all the variable names for \code{toocheap},
  \code{cheap}, \code{expensive} and \code{tooexpensive} variables
  specified above.}
  \item{validate}{logical. should only respondents with
  consistent price preferences (too cheap < cheap < expensive
  < too expensive) be considered in the analysis?}
  \item{interpolate}{logical. should interpolation of the price
  curves be applied between the actual prices given by the
  respondents? If interpolation is enabled, the output appears
  less bumpy in regions with sparse price information. If the
  sample size is sufficiently large, interpolation should not
  be necessary.}
  \item{interpolation_steps}{numeric. if \code{interpolate} is
  \code{TRUE}: the size of the interpolation steps. Set by
  default to \code{get_psm_constant("DEFAULT_INTERPOLATION_STEPS")}, which should be appropriate for most goods
  in a price range of 0-50 USD/Euro.}
  \item{intersection_method}{"min" (default), "max", "mean" or
  "median". defines the method how to determine the price
  points (range, indifference price, optimal price) if there
  are multiple possible intersections of the price curves.
  "min" uses the lowest possible prices, "max" uses the
  highest possible prices, "mean" calculates the mean among
  all intersections and "median" uses the median of all
  possible intersections}
  \item{acceptable_range}{"original" (default) or "narrower".
  Defines which intersection is used to calculate the point of
  marginal cheapness and point of marginal expensiveness, which
  together form the range of acceptable prices. "original"
  uses the definition provided in van Westendorp's paper:
  The lower end of the price range (point of marginal
  cheapness) is defined as the intersection of "too cheap"
  and the inverse of the "cheap" curve. The upper end of the
  price range (point of marginal expensiveness) is defined
  as the intersection of "too expensive" and the inverse of
  the "expensive" curve. Alternatively, it is possible to use
  a "narrower" definition which is applied by some market
  research companies. Here, the lower end of the price range
  is defined as the intersection of the "expensive" and the
  "too cheap" curves and the upper end of the price range is
  defined as the intersection of the "too expensive" and the
  "cheap" curves. This leads to a narrower range of acceptable
  prices. Note that it is possible that the optimal price
  according to the Newton/Miller/Smith extension is higher
  than the upper end of the acceptable price range in the
  "narrower" definition.}
  \item{pi_cheap, pi_expensive}{Only required for the Newton
  Miller Smith extension. Names of the variables in the data
  that contain the survey data on the respondents' purchase
  intent at their individual cheap/expensive price.}
  \item{pi_scale}{Only required for the Newton Miller Smith
  extension. Scale of the purchase intent variables pi_cheap and
  pi_expensive. By default using \code{get_psm_constant("NMS_DEFAULTS.PI_SCALE")}, assuming a five-point scale with 5
  indicating the highest purchase intent.}
  \item{pi_calibrated}{Only required for the Newton Miller Smith
  extension. Calibrated purchase probabilities that are assumed
  for each value of the purchase intent scale. Must be the same
  order as the pi_scale variable so that the first value of
  pi_calibrated corresponds to the first value in the pi_scale
  variable. Default values are \code{get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED")}, taken from the Sawtooth Software
  PSM implementation in Excel: 70\% for the best value of the
  purchase intent scale, 50\% for the second best value,
  30\% for the third best value (middle of the scale), 10\%
  for the fourth best value and 0\% for the worst value.}
  \item{pi_calibrated_toocheap, pi_calibrated_tooexpensive}{
  Only required for the Newton Miller Smith extension. Calibrated
  purchase probabilities for the "too cheap" and the "too
  expensive" price, respectively. Must be a value between 0 and
  1; by default set to \code{get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOCHEAP")} and \code{get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOEXPENSIVE")}, respectively, following the logic in van
  Westendorp's paper.}
}


\details{
The main logic of the Price Sensitivity Meter Analysis is
explained in the documentation of the \code{\link{psm_analysis}}
function. The \code{psm_analysis_weighted} performs the same
analysis, but weights the survey data according to a known
population.}

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
    Contains the \bold{weighted} cumulative distribution
    functions for the four price assessments (too cheap, cheap,
    expensive, too expensive) for all prices.}
    \item{\code{pricerange_lower}:}{\code{numeric} object. Lower
    limit of the acceptable price range as defined by the
    Price Sensitivity Meter, also known as \bold{point of
    marginal cheapness}: Intersection of the "too cheap" and the
    "expensive" curves.}
    \item{\code{pricerange_upper}:}{\code{numeric} object. Upper
    limit of the acceptable price range as defined by the Price
    Sensitivity Meter, also known as \bold{point of marginal
    expensiveness}: Intersection of the "too expensive" and the
    "cheap" curves.}
    \item{\code{idp}:}{\code{numeric} object. \bold{Indifference
    Price Point} as defined by the Price Sensitivity Meter:
    Intersection of the "cheap" and the "expensive" curves.}
    \item{\code{opp}:}{\code{numeric} object. \bold{Optimal
    Price Point} as defined by the Price Sensitivity Meter:
    Intersection of the "too cheap" and the "too expensive"
    curves.}
    \item{\code{weighted}:}{\code{logical} object. Indicating
    if weighted data was used in the analysis. Outputs from
    \code{psm_analysis_weighted()} always have the value
    \code{TRUE}. When data is unweighted, use the function
    \code{\link{psm_analysis}.}}
    \item{\code{survey_design}:}{\code{survey.design2} object.
    Returning the full survey design as specified with the
    \code{\link[survey]{svydesign}} function from the \pkg{survey} package.}
    \item{\code{NMS}:}{\code{logical} object. Indicates whether
    the additional analyses of the Newton Miller Smith Extension
    were performed.}
}

\references{
  Van Westendorp, P (1976) "NSS-Price Sensitivity Meter (PSM) --
  A new approach to study consumer perception of price"
  \emph{Proceedings of the ESOMAR 29th Congress}, 139--167. Online
  available at \url{https://archive.researchworld.com/a-new-approach-to-study-consumer-perception-of-price/}.

  Newton, D, Miller, J, Smith, P, (1993) "A market acceptance
  extension to traditional price sensitivity measurement"
  \emph{Proceedings of the American Marketing Association
  Advanced Research Techniques Forum}.

  Sawtooth Software (2016) "Templates for van Westendorp PSM for
  Lighthouse Studio and Excel". Online available at
  \url{https://sawtoothsoftware.com/resources/software-downloads/tools/van-westendorp-price-sensitivity-meter}
}

\examples{

# assuming a skewed sample with only 1/3 women and 2/3 men

input_data <- data.frame(tch = round(rnorm(n = 250, mean = 8, sd = 1.5), digits = 2),
                         ch = round(rnorm(n = 250, mean = 12, sd = 2), digits = 2),
                         ex = round(rnorm(n = 250, mean = 13, sd = 1), digits = 2),
                         tex = round(rnorm(n = 250, mean = 15, sd = 1), digits = 2),
                         gender = sample(x = c("male", "female"),
                                         size = 250,
                                         replace = TRUE,
                                         prob = c(2/3, 1/3)))

# ... and in which women have on average 1.5x the price acceptance of men
input_data$tch[input_data$gender == "female"] <- input_data$tch[input_data$gender == "female"] * 1.5
input_data$ch[input_data$gender == "female"] <- input_data$ch[input_data$gender == "female"] * 1.5
input_data$ex[input_data$gender == "female"] <- input_data$ex[input_data$gender == "female"] * 1.5
input_data$tex[input_data$gender == "female"] <- input_data$tex[input_data$gender == "female"] * 1.5

# creating a sample design object using the survey package
# ... assuming that gender is balanced equally in the population of 10000

input_data$gender_pop <- 5000

input_design <- survey::svydesign(ids = ~ 1, # no clusters
                          probs = NULL, # hence no cluster sampling probabilities,
                          strata = input_data$gender, # stratified by gender
                          fpc = input_data$gender_pop, # strata size in the population
                          data = input_data)
                          # data object used as input: no need to specify single variables


output_weighted_psm <- psm_analysis_weighted(toocheap = "tch",
  cheap = "ch",
  expensive = "ex",
  tooexpensive = "tex",
  design = input_design)

summary(output_weighted_psm)
}