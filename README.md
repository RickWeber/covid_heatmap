# Covid visualization replication

This is a quick attempt to replicate the work of [@aventura71]( https://twitter.com/aventura71/status/1272567972113059842).

heatmap.R contains the code that ingests data from the [Johns Hopkins Covid data](https://github.com/CSSEGISandData/COVID-19/), processes it, and generates the plots in heatmap_cases.png and heatmap_deaths.png.

In both cases, the data is state-wise normalized (so we're seeing what's abnormal for each state). The underlying data are 7-day rolling means of new cases and deaths.
