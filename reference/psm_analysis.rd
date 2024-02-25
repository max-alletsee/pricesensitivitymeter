<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Van Westendorp Price Sensitivity Meter Analysis (PSM) — psm_analysis • pricesensitivitymeter</title><!-- jquery --><script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.4.1/jquery.min.js" integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=" crossorigin="anonymous"></script><!-- Bootstrap --><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.4.1/css/bootstrap.min.css" integrity="sha256-bZLfwXAP04zRMK2BjiO8iu9pf4FbLqX6zitd+tIvLhE=" crossorigin="anonymous"><script src="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.4.1/js/bootstrap.min.js" integrity="sha256-nuL8/2cJ5NDSSwnKD8VqreErSWHtnEP9E7AySL+1ev4=" crossorigin="anonymous"></script><!-- bootstrap-toc --><link rel="stylesheet" href="../bootstrap-toc.css"><script src="../bootstrap-toc.js"></script><!-- Font Awesome icons --><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous"><!-- clipboard.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script><!-- headroom.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script><!-- pkgdown --><link href="../pkgdown.css" rel="stylesheet"><script src="../pkgdown.js"></script><meta property="og:title" content="Van Westendorp Price Sensitivity Meter Analysis (PSM) — psm_analysis"><meta property="og:description" content="psm_analysis() performs an analysis of consumer price
  preferences and price sensitivity known as van
  Westendorp Price Sensitivity Meter (PSM). It takes respondent's
  price preferences (from survey data) as an input and estimates
  acceptable price ranges and price points. For a description of
  the method see the Details section."><!-- mathjax --><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script><!--[if lt IE 9]>
<script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
<script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]--></head><body data-spy="scroll" data-target="#toc">
    

    <div class="container template-reference-topic">
      <header><div class="navbar navbar-default navbar-fixed-top" role="navigation">
  <div class="container">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <span class="navbar-brand">
        <a class="navbar-link" href="../index.html">pricesensitivitymeter</a>
        <span class="version label label-default" data-toggle="tooltip" data-placement="bottom" title="">1.3.0</span>
      </span>
    </div>

    <div id="navbar" class="navbar-collapse collapse">
      <ul class="nav navbar-nav"><li>
  <a href="../reference/index.html">Reference</a>
</li>
<li class="dropdown">
  <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" data-bs-toggle="dropdown" aria-expanded="false">
    Articles
     
    <span class="caret"></span>
  </a>
  <ul class="dropdown-menu" role="menu"><li>
      <a href="../articles/interpolation-in-small-samples.html">Interpolation in Small Samples</a>
    </li>
    <li>
      <a href="../articles/visualizing-psm-results.html">Visualizing PSM Results</a>
    </li>
    <li>
      <a href="../articles/weighted-data.html">Using Weighted Survey Data</a>
    </li>
  </ul></li>
<li>
  <a href="../news/index.html">Changelog</a>
</li>
      </ul><ul class="nav navbar-nav navbar-right"><li>
  <a href="https://github.com/max-alletsee/pricesensitivitymeter/" class="external-link">
    <span class="fab fa-github fa-lg"></span>
     
  </a>
</li>
      </ul></div><!--/.nav-collapse -->
  </div><!--/.container -->
</div><!--/.navbar -->

      

      </header><div class="row">
  <div class="col-md-9 contents">
    <div class="page-header">
    <h1>Van Westendorp Price Sensitivity Meter Analysis (PSM)</h1>
    
    <div class="hidden name"><code>psm_analysis.rd</code></div>
    </div>

    <div class="ref-description">
    <p><code>psm_analysis()</code> performs an analysis of consumer price
  preferences and price sensitivity known as <b>van
  Westendorp Price Sensitivity Meter (PSM)</b>. It takes respondent's
  price preferences (from survey data) as an input and estimates
  acceptable price ranges and price points. For a description of
  the method see the <em>Details</em> section.</p>
    </div>

    <div id="ref-usage">
    <div class="sourceCode"><pre class="sourceCode r"><code><span><span class="fu">psm_analysis</span><span class="op">(</span></span>
<span>  <span class="va">toocheap</span>, <span class="va">cheap</span>, <span class="va">expensive</span>, <span class="va">tooexpensive</span>,</span>
<span>  data <span class="op">=</span> <span class="cn">NA</span>,</span>
<span>  validate <span class="op">=</span> <span class="cn">TRUE</span>,</span>
<span>  interpolate <span class="op">=</span> <span class="cn">FALSE</span>,</span>
<span>  interpolation_steps <span class="op">=</span> <span class="fl">0.01</span>,</span>
<span>  intersection_method <span class="op">=</span> <span class="st">"min"</span>,</span>
<span>  acceptable_range <span class="op">=</span> <span class="st">"original"</span>,</span>
<span>  pi_cheap <span class="op">=</span> <span class="cn">NA</span>, pi_expensive <span class="op">=</span> <span class="cn">NA</span>,</span>
<span>  pi_scale <span class="op">=</span> <span class="fl">5</span><span class="op">:</span><span class="fl">1</span>,</span>
<span>  pi_calibrated <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fl">0.7</span>, <span class="fl">0.5</span>, <span class="fl">0.3</span>, <span class="fl">0.1</span>, <span class="fl">0</span><span class="op">)</span>,</span>
<span>  pi_calibrated_toocheap <span class="op">=</span> <span class="fl">0</span>, pi_calibrated_tooexpensive <span class="op">=</span> <span class="fl">0</span></span>
<span>  <span class="op">)</span></span></code></pre></div>
    </div>

    <div id="arguments">
    <h2>Arguments</h2>
    <dl><dt>toocheap, cheap, expensive, tooexpensive</dt>
<dd><p>If a
  data.frame/matrix/tibble is provided in the <code>data</code>
  argument: names of the variables in the data.frame/matrix
  that contain the survey data on the respondents' "too cheap",
  "cheap", "expensive" and "too expensive" price preferences.</p>
<p>If no data.frame/matrix/tibble is provided in the <code>data</code>
  argument: numeric vectors that directly include this
  information. If numeric vectors are provided, it is assumed
  that they are sorted by respondent ID (the preferences for
  respondent <code>n</code> are stored at the <code>n</code>-th position in
  all vectors).</p>
<p>If the <code>toocheap</code> price was not assessed, a
  variable/vector of NAs can be used instead. This
  variable/vector needs to have the same length as the other
  survey information. If <code>toocheap</code> is NA for all cases,
  it is possible to calculate the Point of Marginal
  Expensiveness and the Indifference Price Point, but it is
  impossible to calculate the Point of Marginal Cheapness and
  the Optimal Price Point.</p></dd>

  <dt>data</dt>
<dd><p>data.frame, matrix or tibble that contains the
  function's input data. <code>data</code> input is not mandatory:
  Instead of using a data.frame/matrix/tibble as an input, it
  is also possible to provide the data directly as vectors in
  the "too cheap", "cheap", "expensive" and "too expensive"
  arguments.</p></dd>

  <dt>validate</dt>
<dd><p>logical. should only respondents with
  consistent price preferences (too cheap &lt; cheap &lt; expensive
  &lt; too expensive) be considered in the analysis?</p></dd>

  <dt>interpolate</dt>
<dd><p>logical. should interpolation of the price
  curves be applied between the actual prices given by the
  respondents? If interpolation is enabled, the output appears
  less bumpy in regions with sparse price information. If the
  sample size is sufficiently large, interpolation should not
  be necessary.</p></dd>

  <dt>interpolation_steps</dt>
<dd><p>numeric. if <code>interpolate</code> is
  <code>TRUE</code>: the size of the interpolation steps. Set by
  default to 0.01, which should be appropriate for most goods
  in a price range of 0-50 USD/Euro.</p></dd>

  <dt>intersection_method</dt>
<dd><p>"min" (default), "max", "mean" or
  "median". defines the method how to determine the price
  points (range, indifference price, optimal price) if there
  are multiple possible intersections of the price curves.
  "min" uses the lowest possible prices, "max" uses the
  highest possible prices, "mean" calculates the mean among
  all intersections and "median" uses the median of all
  possible intersections</p></dd>

  <dt>acceptable_range</dt>
<dd><p>"original" (default) or "narrower".
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
  "narrower" definition.</p></dd>

  <dt>pi_cheap, pi_expensive</dt>
<dd><p>Only required for the Newton
  Miller Smith extension. If <code>data</code> argument is provided:
  names of the variables in the data.frame/matrix/tibble that
  contain the survey data on the respondents' purchase intent
  at their individual cheap/expensive price.</p></dd>

  <dt>pi_scale</dt>
<dd><p>Only required for the Newton Miller Smith
  extension. Scale of the purchase intent variables pi_cheap and
  pi_expensive. By default assuming a five-point scale with 5
  indicating the highest purchase intent.</p></dd>

  <dt>pi_calibrated</dt>
<dd><p>Only required for the Newton Miller Smith
  extension. Calibrated purchase probabilities that are assumed
  for each value of the purchase intent scale. Must be the same
  order as the pi_scale variable so that the first value of
  pi_calibrated corresponds to the first value in the pi_scale
  variable. Default values are taken from the Sawtooth Software
  PSM implementation in Excel: 70% for the best value of the
  purchase intent scale, 50% for the second best value,
  30% for the third best value (middle of the scale), 10%
  for the fourth best value and 0% for the worst value.</p></dd>

  <dt>pi_calibrated_toocheap, pi_calibrated_tooexpensive</dt>
<dd><p>Only required for the Newton Miller Smith extension. Calibrated
  purchase probabilities for the "too cheap" and the "too
  expensive" price, respectively. Must be a value between 0 and
  1; by default set to zero following the logic in van
  Westendorp's paper.</p></dd>

</dl></div>
    <div id="details">
    <h2>Details</h2>
    <p>The Price Sensitivity Meter method for the analysis of consumer
price preferences was proposed by the Dutch economist Peter van
Westendorp in 1976 at the ESOMAR conference. It is a
survey-based approach that has become one of the standard price
acceptance measurement techniques in the market research
industry and is still widely used for during early-stage product
development.</p>
<p>Price acceptance and price sensitivity are measured in van
Westendorp's approach by four open-ended survey questions:</p>
<ul><li><p>At which price on this scale are you beginning to
experience ... (test-product) as cheap?</p></li>
<li><p>At which price on this scale are you beginning to
experience ... (test-product) as expensive?</p></li>
<li><p>At which price on this scale you are beginning to
experience ... (test-product) as too expensive -- so that you
would never consider buying it yourself?</p></li>
<li><p>At which price on this scale you are beginning to
experience ... (test-product) as too cheap -- so that you say
"at this price the quality cannot be good"?</p></li>
</ul><p>Respondents with inconsistent price preferences (e.g. "cheap"
price larger than "expensive" price) are usually removed from
the data set. This function has built-in checks to detect
invalid preference structures and removes those respondents from
the analysis by default.</p>
<p>To analyze price preferences and price sensitivity, the method
uses cumulative distribution functions for each of the
aforementioned price steps (e.g. "how many respondents think
that a price of <code>x</code> <em>or more</em> is expensive?"). By
convention, the distributions for the "too cheap" and the
"cheap" price are inverted. This leads to the interpretation
"how many respondents think that a price of <em>up to</em>
<code>x</code> is (too) cheap?".</p>
<p>The interpretation is built on the analysis of the intersections
of the four cumulative distribution functions for the different
prices (usually via graphical inspection). The original paper
describes the four intersections as follows:</p>
<ul><li><p><b>Point of Marginal Cheapness (PMC)</b>: Below this price
point, there are more respondents that consider the price as
"too cheap" than respondents who consider it as "not cheap"
(intersection of "too cheap" and "not cheap"). This is interpreted
as the lower limit of the range of acceptable prices.</p></li>
<li><p><b>Point of Marginal Expensiveness (PME)</b>. Above this
price point, there are more respondent that consider the price
as "too expensive" than there are respondents who consider it as
"not expensive" (intersection of "not expensive" and "too
expensive"). This is interpreted as the upper limit of the
range of acceptable prices.</p></li>
<li><p><b>Indifference Price Point (IDP)</b>: The same number of
respondents perceives the price as "cheap" and "expensive"
(intersection of "cheap" and "expensive"). In van Westendorp's
interpretation, this is either the median price paid in the
market or the price of an important market-leader.</p></li>
<li><p><b>Optimal Price Point (OPP)</b>: The same number of
respondents perceives the product as "too cheap" and "too
expensive" (intersection of "too cheap" and "too expensive").
van Westendorp argues that this is the value for which the
respondents' resistance against the price is particularly low.</p></li>
</ul><p>Besides those four intersections, van Westendorp's article
advises to analyze the cumulative distribution functions for
steep areas which indicate price steps.</p>
<p>To analyze reach (trial rates) and estimate revenue forecasts,
Newton/Miller/Smith have extended van Westendorp's original
model by adding two purchase intent questions that are asked for
the respondent's "cheap" and "expensive" price. The purchase
probability at the respondent's "too cheap" and "too expensive"
price are defined as <code>0</code>. The main logic is that the "too
expensive" price point is prohibitively expensive for the
respondent and a price at the "too cheap" price level raises
doubts about the product quality.</p>
<p>By combining the standard van Westendorp questions with those
two additional purchase intent questions, it becomes possible to
summarize the purchase probabilities across respondents (using
linear interpolation for the purchase probabilities between each
respondent's cornerstone prices). The maximum of this curve is
then defined as the price point with the highest expected reach.
Moreover, by multiplying the reach with the price, it also
becomes possible to estimate a price with the highest expected
revenue.</p>
<p>It has to be noted that the van Westendorp Price Sensitivity
Meter is useful in some cases, but does not answer every
pricing-related question. It may be a good tool to assess very
broadly if the consumers' price perceptions exceed the actual
production costs. For more complex analyses (e.g. defining
specific prices for different products to avoid cannibalization
and drive at the same time incremental growth), other
methodological approaches are needed.</p>
    </div>
    <div id="value">
    <h2>Value</h2>
    

<p>The function output consists of the following elements:</p>
<dl><dt><code>data_input</code>:</dt>
<dd><p><code>data.frame</code> object. Contains
    the data that was used as an input for the analysis.</p></dd>

    <dt><code>validated</code>:</dt>
<dd><p><code>logical</code> object. Indicates
    whether the <code>"validate"</code> option has been used (to
    exclude cases with intransitive price preferences).</p></dd>

    <dt><code>invalid_cases</code>:</dt>
<dd><p><code>numeric</code> object. Number
    of cases with intransitive price preferences.</p></dd>

    <dt><code>total_sample</code>:</dt>
<dd><p><code>"numeric"</code> object.
    Total sample size of the input sample <em>before</em>
    assessing the transitivity of individual price preferences.</p></dd>

    <dt><code>data_vanwestendorp</code>:</dt>
<dd><p><code>data.frame</code> object.
    Output data of the Price Sensitivity Meter analysis.
    Contains the cumulative distribution functions for the four
    price assessments (too cheap, cheap, expensive, too
    expensive) for all prices.</p></dd>

    <dt><code>pricerange_lower</code>:</dt>
<dd><p><code>numeric</code> object. Lower
    limit of the acceptable price range as defined by the
    Price Sensitivity Meter, also known as <b>point of
    marginal cheapness</b>: Intersection of the "too cheap" and the
    "not cheap" curves.</p></dd>

    <dt><code>pricerange_upper</code>:</dt>
<dd><p><code>numeric</code> object. Upper
    limit of the acceptable price range as defined by the Price
    Sensitivity Meter, also known as <b>point of marginal
    expensiveness</b>: Intersection of the "too expensive" and the
    "not expensive" curves.</p></dd>

    <dt><code>idp</code>:</dt>
<dd><p><code>numeric</code> object. <b>Indifference
    Price Point</b> as defined by the Price Sensitivity Meter:
    Intersection of the "cheap" and the "expensive" curves.</p></dd>

    <dt><code>opp</code>:</dt>
<dd><p><code>numeric</code> object. <b>Optimal
    Price Point</b> as defined by the Price Sensitivity Meter:
    Intersection of the "too cheap" and the "too expensive"
    curves.</p></dd>

    <dt><code>NMS</code>:</dt>
<dd><p><code>logical</code> object. Indicates whether
    the additional analyses of the Newton Miller Smith Extension
    were performed.</p></dd>

    <dt><code>weighted</code>:</dt>
<dd><p><code>logical</code> object. Indicates
    if weighted data was used in the analysis. Outputs from
    <code>psm_analysis()</code> always have the value <code>FALSE</code>.
    When data is weighted, use the function
    <code><a href="psm_analysis_weighted.html">psm_analysis_weighted</a>.</code></p></dd>

    <dt><code>data_nms</code>:</dt>
<dd><p><code>data.frame</code> object. Output of
    the Newton Miller Smith extension: calibrated mean
    purchase probabilities for each price point.</p></dd>

    <dt><code>pi_scale</code>:</dt>
<dd><p><code>data.frame</code> object. Shows the
    values of the purchase intent variable and the
    corresponding calibrated purchase probabilities as defined
    in the function input for the Newton Miller Smith
    extension.</p></dd>

    <dt><code>price_optimal_reach</code>:</dt>
<dd><p><code>numeric</code> object.
    Output of the Newton Miller Smith extension: Estimate for
    the price with the highest reach (trial rate).</p></dd>

    <dt><code>price_optimal_revenue</code>:</dt>
<dd><p><code>numeric</code> object.
    Output of the Newton Miller Smith extension:
    Estimate for the price with the highest revenue (based on
    the reach).</p></dd>

</dl></div>
    <div id="references">
    <h2>References</h2>
    <p>Van Westendorp, P (1976) "NSS-Price Sensitivity Meter (PSM) --
  A new approach to study consumer perception of price"
  <em>Proceedings of the ESOMAR 29th Congress</em>, 139--167. Online
  available at <a href="https://archive.researchworld.com/a-new-approach-to-study-consumer-perception-of-price/" class="external-link">https://archive.researchworld.com/a-new-approach-to-study-consumer-perception-of-price/</a>.</p>
<p>Newton, D, Miller, J, Smith, P, (1993) "A market acceptance
  extension to traditional price sensitivity measurement"
  <em>Proceedings of the American Marketing Association
  Advanced Research Techniques Forum</em>.</p>
<p>Sawtooth Software (2016) "Templates for van Westendorp PSM for
  Lighthouse Studio and Excel". Online available at
  <a href="https://sawtoothsoftware.com/resources/software-downloads/tools/van-westendorp-price-sensitivity-meter" class="external-link">https://sawtoothsoftware.com/resources/software-downloads/tools/van-westendorp-price-sensitivity-meter</a></p>
<p>Examples for companies that use a narrower definition than
  van Westendorp's original paper include Conjoint.ly
  (<a href="https://conjointly.com/products/van-westendorp/" class="external-link">https://conjointly.com/products/van-westendorp/</a>),
  Quantilope (<a href="https://www.quantilope.com/resources/glossary-how-to-use-van-westendorp-pricing-model-to-inform-pricing-strategy" class="external-link">https://www.quantilope.com/resources/glossary-how-to-use-van-westendorp-pricing-model-to-inform-pricing-strategy</a>),
  and Milieu (<a href="https://www.mili.eu/learn/what-is-the-van-westendorp-pricing-study-and-when-to-use-it" class="external-link">https://www.mili.eu/learn/what-is-the-van-westendorp-pricing-study-and-when-to-use-it</a>)</p>
    </div>
    <div id="see-also">
    <h2>See also</h2>
    <div class="dont-index"><p>The function <code><a href="psm_analysis_weighted.html">psm_analysis_weighted()</a></code> performs the same
analyses for weighted data.</p></div>
    </div>

    <div id="ref-examples">
    <h2>Examples</h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/Random.html" class="external-link">set.seed</a></span><span class="op">(</span><span class="fl">42</span><span class="op">)</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># standard van Westendorp Price Sensitivity Meter Analysis</span></span></span>
<span class="r-in"><span><span class="co"># input directly via vectors</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">tch</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/Round.html" class="external-link">round</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/stats/Normal.html" class="external-link">rnorm</a></span><span class="op">(</span>n <span class="op">=</span> <span class="fl">250</span>, mean <span class="op">=</span> <span class="fl">5</span>, sd <span class="op">=</span> <span class="fl">0.5</span><span class="op">)</span>, digits <span class="op">=</span> <span class="fl">2</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">ch</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/Round.html" class="external-link">round</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/stats/Normal.html" class="external-link">rnorm</a></span><span class="op">(</span>n <span class="op">=</span> <span class="fl">250</span>, mean <span class="op">=</span> <span class="fl">8.5</span>, sd <span class="op">=</span> <span class="fl">0.5</span><span class="op">)</span>, digits <span class="op">=</span> <span class="fl">2</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">ex</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/Round.html" class="external-link">round</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/stats/Normal.html" class="external-link">rnorm</a></span><span class="op">(</span>n <span class="op">=</span> <span class="fl">250</span>, mean <span class="op">=</span> <span class="fl">13</span>, sd <span class="op">=</span> <span class="fl">0.75</span><span class="op">)</span>, digits <span class="op">=</span> <span class="fl">2</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">tex</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/Round.html" class="external-link">round</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/stats/Normal.html" class="external-link">rnorm</a></span><span class="op">(</span>n <span class="op">=</span> <span class="fl">250</span>, mean <span class="op">=</span> <span class="fl">17</span>, sd <span class="op">=</span> <span class="fl">1</span><span class="op">)</span>, digits <span class="op">=</span> <span class="fl">2</span><span class="op">)</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">output_psm_demo1</span> <span class="op">&lt;-</span> <span class="fu">psm_analysis</span><span class="op">(</span>toocheap <span class="op">=</span> <span class="va">tch</span>,</span></span>
<span class="r-in"><span>  cheap <span class="op">=</span> <span class="va">ch</span>,</span></span>
<span class="r-in"><span>  expensive <span class="op">=</span> <span class="va">ex</span>,</span></span>
<span class="r-in"><span>  tooexpensive <span class="op">=</span> <span class="va">tex</span><span class="op">)</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># additional analysis with Newton Miller Smith Extension</span></span></span>
<span class="r-in"><span><span class="co"># input via data.frame</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">pint_ch</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/sample.html" class="external-link">sample</a></span><span class="op">(</span>x <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fl">1</span><span class="op">:</span><span class="fl">5</span><span class="op">)</span>, size <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/length.html" class="external-link">length</a></span><span class="op">(</span><span class="va">tex</span><span class="op">)</span>,</span></span>
<span class="r-in"><span>  replace <span class="op">=</span> <span class="cn">TRUE</span>, prob <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fl">0.1</span>, <span class="fl">0.1</span>, <span class="fl">0.2</span>, <span class="fl">0.3</span>, <span class="fl">0.3</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">pint_ex</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/sample.html" class="external-link">sample</a></span><span class="op">(</span>x <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fl">1</span><span class="op">:</span><span class="fl">5</span><span class="op">)</span>, size <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/length.html" class="external-link">length</a></span><span class="op">(</span><span class="va">tex</span><span class="op">)</span>,</span></span>
<span class="r-in"><span>  replace <span class="op">=</span> <span class="cn">TRUE</span>, prob <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fl">0.3</span>, <span class="fl">0.3</span>, <span class="fl">0.2</span>, <span class="fl">0.1</span>, <span class="fl">0.1</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">data_psm_demo</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/data.frame.html" class="external-link">data.frame</a></span><span class="op">(</span><span class="va">tch</span>, <span class="va">ch</span>, <span class="va">ex</span>, <span class="va">tex</span>, <span class="va">pint_ch</span>, <span class="va">pint_ex</span><span class="op">)</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">output_psm_demo2</span> <span class="op">&lt;-</span> <span class="fu">psm_analysis</span><span class="op">(</span>toocheap <span class="op">=</span> <span class="st">"tch"</span>,</span></span>
<span class="r-in"><span>  cheap <span class="op">=</span> <span class="st">"ch"</span>,</span></span>
<span class="r-in"><span>  expensive <span class="op">=</span> <span class="st">"ex"</span>,</span></span>
<span class="r-in"><span>  tooexpensive <span class="op">=</span> <span class="st">"tex"</span>,</span></span>
<span class="r-in"><span>  pi_cheap <span class="op">=</span> <span class="st">"pint_ch"</span>,</span></span>
<span class="r-in"><span>  pi_expensive <span class="op">=</span> <span class="st">"pint_ex"</span>,</span></span>
<span class="r-in"><span>  data <span class="op">=</span> <span class="va">data_psm_demo</span><span class="op">)</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/summary.html" class="external-link">summary</a></span><span class="op">(</span><span class="va">output_psm_demo2</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Van Westendorp Price Sensitivity Meter Analysis</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Accepted Price Range: 6.35 - 14.67 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Indifference Price Point: 9.98 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Optimal Price Point: 6.35 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Newton Miller Smith Extension</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Price with Optimal Reach: 8.72 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Price with Optimal Revenue: 11.49 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> ---</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 250 cases with individual price preferences were analyzed (unweighted data).</span>
</code></pre></div>
    </div>
  </div>
  <div class="col-md-3 hidden-xs hidden-sm" id="pkgdown-sidebar">
    <nav id="toc" data-toggle="toc" class="sticky-top"><h2 data-toc-skip>Contents</h2>
    </nav></div>
</div>


      <footer><div class="copyright">
  <p></p><p>Developed by Max Alletsee.</p>
</div>

<div class="pkgdown">
  <p></p><p>Site built with <a href="https://pkgdown.r-lib.org/" class="external-link">pkgdown</a> 2.0.7.</p>
</div>

      </footer></div>

  


  

  </body></html>
