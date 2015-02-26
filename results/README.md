Results
===============================================================================

## Convergence
Convergence, defined as a maximum gradient less than 0.01,
was an issue for scenarios that did not have age composition data.
Particularly when fishing exhibited a two-way trip rather than a constant
pattern. Approximately 70\% of the runs did not converge when no age data were
present and convergence only increased to 50\% when length data were available
from both the fishery and the survey but still no age data were present.

## No age data
The lack of age composition data was less of an issue for the longer-lived life history
(yelloweye) than shorter-lived life histories. Although, estimates of
*CV~old~* were equally biased and imprecise for both the long- and medium-lived
life histories.

Estimates of *CV~young~* for the long-lived life history were fairly precise and
relatively unbiased for all scenarios, except when age data were not available.
When the model had no age data to inform *CV~young~* the estimates were positively
biased. Conversely the medium-lived life history displayed a negative bias in
estimates of  *CV~young~* when no age data were present.

Without age composition data, estimates of length at A~max~ were extremely
imprecise and positively biased.

Estimates of *K* were negatively biased across all data-type scenarios except
for when conditional age-at-length data were present. The bias was indifferent
to the availability of age data, in fact estimates of *K* were slightly more
biased when age data were present compared to when there was no age data for those
scenarios where all parameters were estimated internally in the model.

## Internal versus external
For the medium-lived life history, internal estimates of *CV~young~* were negatively
biased and highly variable, with the bias decreasing as more age composition data
was added to the model. Although, the bias decreased with added age composition data,
the variance was still larger than the longer-lived life history.
External estimates of *CV~young~* for the medium-lived life history were less biased
and more precise than when the parameter was estimated internally, except for when
conditional age-at-length data were present.

For the medium-lived life history, internal estimates of length at A~max~ were
less variable than external estimates, though both were relatively unbiased.
Even without age composition data, estimates of length at A~max~ were relatively
unbiased and precise for the long-lived life history.

External estimates of length at A~min~ were more than 50\% positively biased
for all life histories.

## Model misspecification
When natural mortality is specified at a value larger than the true value used
in the operating model, external estimates of length at A~max~ are highly
variable.

## Questions
* Why is *K* more negatively biased for yelloweye when the model has both
survey and fishery length composition data compared to when only fishery length
composition data are available?