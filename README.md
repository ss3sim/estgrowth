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
     * Estimated
 * Fishing pattern:
     * Two-way trip
     * Constant @ *F~MSY~* - or perhaps something a bit more like 1.10 * *F~MSY~* to
     try and create a truncated age-structure
 * Selectivity (OM) parameterized as a double normal:
     * asymptotic
     * dome

### Metrics
 * *K*
 * *L~min~*
 * *L~max~*
 * *CV~young~*
 * *CV~old~*
 * *M*
 * *SSB~terminal~*
 * *F~terminal~*
 * ?Forecast


### Todo:
* Change bounds of col model
* Change A1 to a different number so that A and L cases align
* Run models with flatfish
* Create rockfish models
