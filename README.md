estgrowth
===============================================================================

### Valero *et al*. 2014. Evaluating the impacts of fixing or estimating
growth parameters, across life histories and data availability.


### Summary
Simulations with `ss3sim` to guide users when it is best to estimate growth
within Stock Synthesis compared to using external estimates.

### Abstract
Monte Carlo simulations were used to determine when it is better to estimate
growth internally in a stock assessment model compared to using external estimates.
Simulations were ran for a variety of data combinations as well as
multiple life-history types.

### Cases
 * Growth:
     * Fixed at true values
     * Internal estimation (all 5 parameters)
     * External estimation (all 5 parameters)
 * Data types:
     * Length
     * Age
     * Conditional length-at-age
     * Mean length-at-age
 * Natural mortality (*M*):
     * Fixed at true
     * Fixed above and below the truth for a few scenarios
 * Fishing pattern:
     * Two-way trip
     * Constant @ *F_MSY*
       * True *F_MSY*
       * 1.10 * *F_MSY* to create a truncated age-structure
       * 0.80 * *F_MSY* for life histories with higher *M* b/c true *F_MSY* leads to convergence issues
 * Selectivity (OM) parameterized as a double normal:
     * asymptotic

### Metrics
 * *K*
 * *L_min*
 * *L_max*
 * *CV_young*
 * *CV_old*
 * *M*
 * *SSB_terminal*
 * *F_terminal*


### Todo:
* Standardize model
* Run scenarios
* Choose which scenarios to introduce miss-specification in *M*
* Create pltos
* Finish manuscript
* Submit to Fisheries
