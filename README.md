# Development and validation of a prediction model for failure of transfemoral approach of endovascular treatment for large vessel occlusion acute ischemic stroke (in progress)

Authors: Ghislaine Holswilder, Julia van Hees, Stijn Kremer, Edouard F. Bonneville, Hine van Os, Nyika D. Kruyt, Marieke J.H. Wermer, Marianne A.A. van Walderveen, on behalf of the MR CLEAN Registry investigators

## Usage

![](analysis/targets-pipeline.png)

Given the data, reproduce the analysis using

``` r
targets::tar_make()
```

or otherwise use parallel computation with

``` r
targets::tar_make_future(workers = future::availableCores())
```