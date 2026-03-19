# Calculate the expected mortality risks from a survival matrix

Many methods can be used to reduce a discrete survival distribution
prediction (i.e. matrix) to a relative risk / ranking prediction, see
Sonabend et al. (2022).

This function calculates a relative risk score as the sum of the
predicted cumulative hazard function, also called **ensemble/expected
mortality**. This risk score can be loosely interpreted as the expected
number of deaths for patients with similar characteristics, see Ishwaran
et al. (2008) and has no model or survival distribution assumptions.

## Usage

``` r
get_mortality(x, times = NULL, eps = 1e-12, check = TRUE)
```

## Arguments

- x:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  A survival matrix. Rows correspond to observations and columns
  correspond to time points.

- times:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)\|`NULL`)  
  Optional numeric vector of time points corresponding to the columns of
  `x`.

- eps:

  (`numeric(1)`)  
  Small positive constant used to bound survival probabilities away from
  zero and prevent infinite cumulative hazards, as `H(t) = -log(S(t))`.

- check:

  (`logical(1)`)  
  If `TRUE` (default), uses
  [assert_prob](https://survdistr.mlr-org.com/reference/assert_prob.html)
  to perform several checks on the survival matrix. Set to `FALSE` to
  skip checks (NOT recommended for external use).

## Value

a `numeric` vector of the mortality risk scores, one per row of the
input survival matrix.

## References

Sonabend, Raphael, Bender, Andreas, Vollmer, Sebastian (2022). “Avoiding
C-hacking when evaluating survival distribution predictions with
discrimination measures.” *Bioinformatics*. ISSN 1367-4803,
[doi:10.1093/BIOINFORMATICS/BTAC451](https://doi.org/10.1093/BIOINFORMATICS/BTAC451)
,
<https://academic.oup.com/bioinformatics/advance-article/doi/10.1093/bioinformatics/btac451/6640155>.

Ishwaran, Hemant, Kogalur, B U, Blackstone, H E, Lauer, S M, others
(2008). “Random survival forests.” *The Annals of applied statistics*,
**2**(3), 841–860.

## Examples

``` r
n = 10 # number of observations
k = 50 # time points

# Create the matrix with random values between 0 and 1
mat = matrix(runif(n * k, min = 0, max = 1), nrow = n, ncol = k)

# transform it to a survival matrix
x = t(apply(mat, 1L, function(row) sort(row, decreasing = TRUE)))
colnames(x) = 1:k # time points

# get mortality scores (larger values correspond to higher risk scores)
mort = get_mortality(x)
mort
#>  [1] 55.66627 42.58805 56.20021 46.24583 43.40607 59.96400 49.41797 40.02692
#>  [9] 58.13115 47.63091
```
