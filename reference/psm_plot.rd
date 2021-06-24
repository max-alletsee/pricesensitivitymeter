<!-- Generated by pkgdown: do not edit by hand -->
<!DOCTYPE html>
<html lang="en">
  <head>
  <meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1.0">

<title>Plot of the van Westendorp Price Sensitivity Meter Analysis (PSM) — psm_plot • pricesensitivitymeter</title>


<!-- jquery -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.4.1/jquery.min.js" integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=" crossorigin="anonymous"></script>
<!-- Bootstrap -->

<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.4.1/css/bootstrap.min.css" integrity="sha256-bZLfwXAP04zRMK2BjiO8iu9pf4FbLqX6zitd+tIvLhE=" crossorigin="anonymous" />

<script src="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.4.1/js/bootstrap.min.js" integrity="sha256-nuL8/2cJ5NDSSwnKD8VqreErSWHtnEP9E7AySL+1ev4=" crossorigin="anonymous"></script>

<!-- bootstrap-toc -->
<link rel="stylesheet" href="../bootstrap-toc.css">
<script src="../bootstrap-toc.js"></script>

<!-- Font Awesome icons -->
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous" />
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous" />

<!-- clipboard.js -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script>

<!-- headroom.js -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script>

<!-- pkgdown -->
<link href="../pkgdown.css" rel="stylesheet">
<script src="../pkgdown.js"></script>




<meta property="og:title" content="Plot of the van Westendorp Price Sensitivity Meter Analysis (PSM) — psm_plot" />
<meta property="og:description" content="psm_plot() uses ggplot() to show the standard van
  Westendorp Price Sensitivity Meter plot that allows to see the
  acceptance for each price on each of the four variables.
It takes the object created by psm_analysis() or
  psm_analysis_weighted() as an input." />




<!-- mathjax -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script>

<!--[if lt IE 9]>
<script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
<script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]-->



  </head>

  <body data-spy="scroll" data-target="#toc">
    <div class="container template-reference-topic">
      <header>
      <div class="navbar navbar-default navbar-fixed-top" role="navigation">
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
        <span class="version label label-default" data-toggle="tooltip" data-placement="bottom" title="Released version">1.2.2</span>
      </span>
    </div>

    <div id="navbar" class="navbar-collapse collapse">
      <ul class="nav navbar-nav">
        <li>
  <a href="../index.html">
    <span class="fas fa-home fa-lg"></span>
     
  </a>
</li>
<li>
  <a href="../reference/index.html">Reference</a>
</li>
<li class="dropdown">
  <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">
    Articles
     
    <span class="caret"></span>
  </a>
  <ul class="dropdown-menu" role="menu">
    <li>
      <a href="../articles/interpolation-in-small-samples.html">Interpolation in Small Samples</a>
    </li>
    <li>
      <a href="../articles/using-weighted-data.html">Using Weighted Survey Data</a>
    </li>
    <li>
      <a href="../articles/visualizing-psm-results.html">Visualizing PSM Results</a>
    </li>
  </ul>
</li>
      </ul>
      <ul class="nav navbar-nav navbar-right">
        <li>
  <a href="https://github.com/max-alletsee/pricesensitivitymeter/">
    <span class="fab fa-github fa-lg"></span>
     
  </a>
</li>
      </ul>
      
    </div><!--/.nav-collapse -->
  </div><!--/.container -->
</div><!--/.navbar -->

      

      </header>

<div class="row">
  <div class="col-md-9 contents">
    <div class="page-header">
    <h1>Plot of the van Westendorp Price Sensitivity Meter Analysis (PSM)</h1>
    
    <div class="hidden name"><code>psm_plot.rd</code></div>
    </div>

    <div class="ref-description">
    <p><code>psm_plot()</code> uses <code><a href='https://ggplot2.tidyverse.org/reference/ggplot.html'>ggplot()</a></code> to show the standard van
  Westendorp Price Sensitivity Meter plot that allows to see the
  acceptance for each price on each of the four variables.</p>
<p>It takes the object created by <code><a href='psm_analysis.html'>psm_analysis()</a></code> or
  <code><a href='psm_analysis_weighted.html'>psm_analysis_weighted()</a></code> as an input.</p>
    </div>

    <pre class="usage"><span class='fu'>psm_plot</span><span class='op'>(</span><span class='va'>psm_result</span>,
         shade_pricerange <span class='op'>=</span> <span class='cn'>TRUE</span>,
         line_toocheap <span class='op'>=</span> <span class='cn'>TRUE</span>,
         line_tooexpensive <span class='op'>=</span> <span class='cn'>TRUE</span>,
         line_notcheap <span class='op'>=</span> <span class='cn'>TRUE</span>,
         line_notexpensive <span class='op'>=</span> <span class='cn'>TRUE</span>,
         point_idp <span class='op'>=</span> <span class='cn'>TRUE</span>,
         point_color_idp <span class='op'>=</span> <span class='st'>"#009E73"</span>,
         label_idp <span class='op'>=</span> <span class='cn'>TRUE</span>,
         point_opp <span class='op'>=</span> <span class='cn'>TRUE</span>,
         point_color_opp <span class='op'>=</span> <span class='st'>"#009E73"</span>,
         label_opp<span class='op'>=</span> <span class='cn'>TRUE</span>,
         pricerange_color <span class='op'>=</span> <span class='st'>"grey50"</span>,
         pricerange_alpha <span class='op'>=</span> <span class='fl'>0.3</span>,
         line_color <span class='op'>=</span> <span class='fu'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='op'>(</span><span class='st'>"too cheap"</span> <span class='op'>=</span> <span class='st'>"#009E73"</span>,
                        <span class='st'>"not cheap"</span> <span class='op'>=</span> <span class='st'>"#009E73"</span>,
                        <span class='st'>"not expensive"</span> <span class='op'>=</span> <span class='st'>"#D55E00"</span>,
                        <span class='st'>"too expensive"</span> <span class='op'>=</span> <span class='st'>"#D55E00"</span><span class='op'>)</span>,
         line_type <span class='op'>=</span> <span class='fu'><a href='https://rdrr.io/r/base/c.html'>c</a></span><span class='op'>(</span><span class='st'>"too cheap"</span> <span class='op'>=</span> <span class='st'>"dotted"</span>,
                       <span class='st'>"not cheap"</span> <span class='op'>=</span> <span class='st'>"solid"</span>,
                       <span class='st'>"not expensive"</span> <span class='op'>=</span> <span class='st'>"solid"</span>,
                       <span class='st'>"too expensive"</span> <span class='op'>=</span> <span class='st'>"dotted"</span><span class='op'>)</span><span class='op'>)</span></pre>

    <h2 class="hasAnchor" id="arguments"><a class="anchor" href="#arguments"></a>Arguments</h2>
    <table class="ref-arguments">
    <colgroup><col class="name" /><col class="desc" /></colgroup>
    <tr>
      <th>psm_result</th>
      <td><p>Result of a Price Sensitivity Meter analysis,
  created by running <code><a href='psm_analysis.html'>psm_analysis()</a></code> or
  <code><a href='psm_analysis_weighted.html'>psm_analysis_weighted()</a></code>. (Object of class <code>"psm"</code>)</p></td>
    </tr>
    <tr>
      <th>shade_pricerange</th>
      <td><p>logical value. Determines if the
  acceptable price range is shown as a shaded area or not.</p></td>
    </tr>
    <tr>
      <th>line_toocheap</th>
      <td><p>logical value. Determines if the line
  for the "too cheap" price curve is shown or not.</p></td>
    </tr>
    <tr>
      <th>line_tooexpensive</th>
      <td><p>logical value. Determines if the
  line for the "too expensive" price curve is shown or not.</p></td>
    </tr>
    <tr>
      <th>line_notcheap</th>
      <td><p>logical value. Determines if the line
  for the "not cheap" price curve is shown or not.</p></td>
    </tr>
    <tr>
      <th>line_notexpensive</th>
      <td><p>logical value. Determines if the
  line for the "not expensive" price curve is shown or not.</p></td>
    </tr>
    <tr>
      <th>point_idp</th>
      <td><p>logical value. Determines if the
  Indifference Price Point is shown or not.</p></td>
    </tr>
    <tr>
      <th>point_color_idp</th>
      <td><p>character vector, specifying the
  color of the Optimal Price Point. Can be a hex color (e.g.
  "#7f7f7f") or one of R's built-in colors (e.g. "grey50").</p></td>
    </tr>
    <tr>
      <th>label_idp</th>
      <td><p>logical value. Determines if the label for
  the Indifference Price Point is shown or not.</p></td>
    </tr>
    <tr>
      <th>point_opp</th>
      <td><p>logical value. Determines if the Optimal
  Price Point is shown or not.</p></td>
    </tr>
    <tr>
      <th>point_color_opp</th>
      <td><p>character vector, specifying the
  color of the Optimal Price Point. Can be a hex color (e.g.
  "#7f7f7f") or one of R's built-in colors (e.g. "grey50").</p></td>
    </tr>
    <tr>
      <th>label_opp</th>
      <td><p>logical value. Determines if the label for
  the Optimal Price Point is shown or not.</p></td>
    </tr>
    <tr>
      <th>pricerange_color</th>
      <td><p>character, specifying the
  background color for the accepted price range. Can be a
  hex color (e.g. "#7f7f7f") or one of R's built-in colors
  (e.g. "grey50"). You can see all of R's built-in colors
  with the function <code><a href='https://rdrr.io/r/grDevices/colors.html'>colors()</a></code>. Is only applied if
  <code>shade_pricerange = TRUE</code></p></td>
    </tr>
    <tr>
      <th>pricerange_alpha</th>
      <td><p>numeric between 0 and 1,
  specifying the alpha transparency for the shaded area of
  the the accepted price range. Is only applied if
  <code>shade_pricerange = TRUE</code></p></td>
    </tr>
    <tr>
      <th>line_color</th>
      <td><p>character vector, specifying the line
  color for each of the price curves shown. Color
  definitions must match the lines you have defined via
  <code>line_toocheap, line_tooexpensive, line_notcheap</code>
  and <code>line_notexpensive</code>. Can be a hex color (e.g.
  "#7f7f7f") or one of R's built-in colors (e.g. "grey50").</p></td>
    </tr>
    <tr>
      <th>line_type</th>
      <td><p>vector, specifying the line type for each
  of the price curves shown. Definitions must match the lines
  you have defined via <code>line_toocheap, line_tooexpensive,
  line_notcheap</code> and <code>line_notexpensive</code>. Values must
  match ggplot2's expectations for line types: An integer (0:8),
  a name (blank, solid, dashed, dotted, dotdash, longdash,
  twodash), or a string with an even number (up to eight) of
  hexadecimal digits which give the lengths in consecutive
  positions in the string.</p></td>
    </tr>
    </table>

    <h2 class="hasAnchor" id="value"><a class="anchor" href="#value"></a>Value</h2>

    <p>The function output is a ggplot2 object.</p>
    <h2 class="hasAnchor" id="references"><a class="anchor" href="#references"></a>References</h2>

    <p>Van Westendorp, P (1976) "NSS-Price Sensitivity Meter (PSM) --
  A new approach to study consumer perception of price"
  <em>Proceedings of the ESOMAR 29th Congress</em>, 139--167. Online
  available at <a href='https://www.researchworld.com/a-new-approach-to-study-consumer-perception-of-price/'>https://www.researchworld.com/a-new-approach-to-study-consumer-perception-of-price/</a>.</p>
    <h2 class="hasAnchor" id="see-also"><a class="anchor" href="#see-also"></a>See also</h2>

    <div class='dont-index'><p>The vignette "Visualizing PSM Results" shows a similar way and more custom way to plot the data.</p></div>

    <h2 class="hasAnchor" id="examples"><a class="anchor" href="#examples"></a>Examples</h2>
    <pre class="examples"><div class='input'><span class='co'># set up example data and run psm_analysis()</span>

<span class='va'>tch</span> <span class='op'>&lt;-</span> <span class='fu'><a href='https://rdrr.io/r/base/Round.html'>round</a></span><span class='op'>(</span><span class='fu'><a href='https://rdrr.io/r/stats/Normal.html'>rnorm</a></span><span class='op'>(</span>n <span class='op'>=</span> <span class='fl'>250</span>, mean <span class='op'>=</span> <span class='fl'>5</span>, sd <span class='op'>=</span> <span class='fl'>0.5</span><span class='op'>)</span>, digits <span class='op'>=</span> <span class='fl'>2</span><span class='op'>)</span>
<span class='va'>ch</span> <span class='op'>&lt;-</span> <span class='fu'><a href='https://rdrr.io/r/base/Round.html'>round</a></span><span class='op'>(</span><span class='fu'><a href='https://rdrr.io/r/stats/Normal.html'>rnorm</a></span><span class='op'>(</span>n <span class='op'>=</span> <span class='fl'>250</span>, mean <span class='op'>=</span> <span class='fl'>8.5</span>, sd <span class='op'>=</span> <span class='fl'>0.5</span><span class='op'>)</span>, digits <span class='op'>=</span> <span class='fl'>2</span><span class='op'>)</span>
<span class='va'>ex</span> <span class='op'>&lt;-</span> <span class='fu'><a href='https://rdrr.io/r/base/Round.html'>round</a></span><span class='op'>(</span><span class='fu'><a href='https://rdrr.io/r/stats/Normal.html'>rnorm</a></span><span class='op'>(</span>n <span class='op'>=</span> <span class='fl'>250</span>, mean <span class='op'>=</span> <span class='fl'>13</span>, sd <span class='op'>=</span> <span class='fl'>0.75</span><span class='op'>)</span>, digits <span class='op'>=</span> <span class='fl'>2</span><span class='op'>)</span>
<span class='va'>tex</span> <span class='op'>&lt;-</span> <span class='fu'><a href='https://rdrr.io/r/base/Round.html'>round</a></span><span class='op'>(</span><span class='fu'><a href='https://rdrr.io/r/stats/Normal.html'>rnorm</a></span><span class='op'>(</span>n <span class='op'>=</span> <span class='fl'>250</span>, mean <span class='op'>=</span> <span class='fl'>17</span>, sd <span class='op'>=</span> <span class='fl'>1</span><span class='op'>)</span>, digits <span class='op'>=</span> <span class='fl'>2</span><span class='op'>)</span>

<span class='va'>output_psm_demo</span> <span class='op'>&lt;-</span> <span class='fu'><a href='psm_analysis.html'>psm_analysis</a></span><span class='op'>(</span>toocheap <span class='op'>=</span> <span class='va'>tch</span>,
  cheap <span class='op'>=</span> <span class='va'>ch</span>,
  expensive <span class='op'>=</span> <span class='va'>ex</span>,
  tooexpensive <span class='op'>=</span> <span class='va'>tex</span><span class='op'>)</span>

<span class='co'># create the plot (note that ggplot's convention</span>
<span class='co'># is to *not* show it by default)</span>
<span class='kw'>if</span> <span class='op'>(</span><span class='cn'>FALSE</span><span class='op'>)</span> <span class='va'>psm_result_plot</span> <span class='op'>&lt;-</span> <span class='fu'>psm_plot</span><span class='op'>(</span><span class='va'>output_psm_demo</span><span class='op'>)</span>

<span class='co'># to show the plot, call the object (and maybe</span>
<span class='co'># additional ggplot functions if you like)</span>
<span class='va'>psm_result_plot</span> <span class='op'>+</span> <span class='fu'>ggplot2</span><span class='fu'>::</span><span class='fu'><a href='https://ggplot2.tidyverse.org/reference/ggtheme.html'>theme_minimal</a></span><span class='op'>(</span><span class='op'>)</span>
</div><div class='output co'>#&gt; <span class='error'>Error in eval(expr, envir, enclos): object 'psm_result_plot' not found</span></div></pre>
  </div>
  <div class="col-md-3 hidden-xs hidden-sm" id="pkgdown-sidebar">
    <nav id="toc" data-toggle="toc" class="sticky-top">
      <h2 data-toc-skip>Contents</h2>
    </nav>
  </div>
</div>


      <footer>
      <div class="copyright">
  <p>Developed by Max Alletsee.</p>
</div>

<div class="pkgdown">
  <p>Site built with <a href="https://pkgdown.r-lib.org/">pkgdown</a> 1.6.1.</p>
</div>

      </footer>
   </div>

  


  </body>
</html>


