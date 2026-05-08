# Making repeat-sales indexes

``` r

library(rsmatrix)
library(Matrix)
```

The repeat-sales method is an approach to construct property price
indexes by exploiting multiple sales for the same property over time to
control for time-invariant differences in quality between properties. In
practice, repeat-sales indexes come in a number of different flavors.
There are two broad classes of repeat-sales price indexes—the geometric
repeat-sales index (GRS index) and the arithmetic repeat-sales index
(ARS index)—along with various weighting schemes that can be used to
weight the prices in the index calculation. In all cases these indexes
can be calculated as a linear estimator using a few different matrices.
The purpose of the **rsmatrix** package is to make it easy to construct
these matrices and compute a repeat-sales index.

## Some data

Let’s start with some fictitious data on house sales for five cities
over a ten-year period. These data won’t make an interesting price
index, but they’re broadly representative of the type of data used to
make a housing price index in practice.

``` r

set.seed(15243)

periods <- seq(as.Date("2010-01-01"), as.Date("2019-12-31"), "day")

prices <- data.frame(
  sale = sample(periods, 5e5, TRUE),
  property = factor(sprintf("%05d", sample(1:5e4, 5e5, TRUE))),
  city = factor(sample(1:5, 1e5, TRUE)),
  price = round(rlnorm(5e5) * 5e5, -3)
)

prices <- prices[order(prices$city, prices$property, prices$sale), ]
row.names(prices) <- NULL

head(prices)
#>         sale property city   price
#> 1 2012-11-18    00001    1 2831000
#> 2 2018-10-05    00001    1  290000
#> 3 2010-09-20    00002    1 1519000
#> 4 2019-12-19    00002    1  269000
#> 5 2012-06-25    00003    1  712000
#> 6 2016-10-15    00003    1  520000
```

## Preparing to make the matrices

The repeat-sales method uses multiple sales for the same property over
time to identify the change in housing prices. The major step to prepare
the data for this method is to make pairs of consecutive sales (sales
pairs). Note that this will remove the sizable chunk of properties for
which there is only one sale.

``` r

interaction(prices$city, prices$property, drop = TRUE) |>
  tabulate() |>
  quantile()
#>   0%  25%  50%  75% 100% 
#>    1    1    2    3   11
```

We’ll be making a monthly index, and it’s useful to turn sale dates into
year months before constructing sales pairs. (Using, say, the `yearmon`
class from **zoo** instead of a factor means this can be done after
making sales pairs.)

``` r

prices$period <- cut(prices$sale, "month")
```

The
[`rs_pairs()`](https://marberts.github.io/rsmatrix/reference/rs_pairs.md)
function gives the position of the previous sale for each property in
each city in the data.

``` r

sales_pairs <- rs_pairs(prices$sale, interaction(prices$city, prices$property))
prices[c("price_prev", "period_prev")] <- prices[
  sales_pairs,
  c("price", "period")
]

head(prices)
#>         sale property city   price     period price_prev period_prev
#> 1 2012-11-18    00001    1 2831000 2012-11-01    2831000  2012-11-01
#> 2 2018-10-05    00001    1  290000 2018-10-01    2831000  2012-11-01
#> 3 2010-09-20    00002    1 1519000 2010-09-01    1519000  2010-09-01
#> 4 2019-12-19    00002    1  269000 2019-12-01    1519000  2010-09-01
#> 5 2012-06-25    00003    1  712000 2012-06-01     712000  2012-06-01
#> 6 2016-10-15    00003    1  520000 2016-10-01     712000  2012-06-01
```

Now that the data are oriented as sales pairs, we can remove pairs of
transactions that don’t belong in the calculation. The goal is to help
motivate the assumption that it is only time-invariant differences in
quality that confound a change in housing prices with a change in the
composition of housing that sells over time.

It’s normal to remove pairs of transactions that are close together
(e.g., less than two months apart), as these properties may be flips or
the result of financial difficulty that is not indicative of what the
property would have otherwise sold for. Note that this filters out
properties that only sell once.

``` r

prices$holding_period <- with(
  prices,
  as.numeric(period) - as.numeric(period_prev)
)

prices <- subset(prices, holding_period > 2)
```

Properties with a very large change in price may have undergone major
improvements or depreciation that similarly undermine the assumptions of
the repeat-sales method. There are many ways to try and filter out these
transactions, but a simple approach is to remove pairs of sales where
the monthly return for a property is greater than, say, 2.5 median
absolute deviations from the median. (The **gpindex** package has
several other methods for dealing with extreme changes in prices.)

``` r

library(gpindex)
monthly_return <- with(prices, (price / price_prev)^(1 / holding_period))

robust_z <- grouped(robust_z)
prices <- subset(prices, !robust_z(monthly_return, group = city))

head(prices)
#>          sale property city  price     period price_prev period_prev
#> 2  2018-10-05    00001    1 290000 2018-10-01    2831000  2012-11-01
#> 4  2019-12-19    00002    1 269000 2019-12-01    1519000  2010-09-01
#> 6  2016-10-15    00003    1 520000 2016-10-01     712000  2012-06-01
#> 8  2011-07-18    00004    1 305000 2011-07-01      90000  2010-07-01
#> 9  2013-12-03    00004    1 768000 2013-12-01     305000  2011-07-01
#> 10 2018-08-02    00004    1 121000 2018-08-01     768000  2013-12-01
#>    holding_period
#> 2              71
#> 4             111
#> 6              52
#> 8              12
#> 9              29
#> 10             56
```

## Calculating a repeat-sales index

With the data in the right form, and potentially problematic
transactions gone, we can now make the repeat-sales matrices and
calculate a housing price index. The strategy is to make a constructor
function based on the sales-pair data that can be used to make the
repeat-sales matrices. These matrices are naturally sparse, and the
large volume of transactions means we’ll benefit for using sparse
matrices.

``` r

matrices <- with(
  prices,
  rs_matrix(period, period_prev, price, price_prev, city, sparse = TRUE)
)
```

The simplest repeat-sales index in the geometric repeat sales (GRS)
index.[^1] This index uses the \\Z\\ and \\y\\ matrices.

``` r

Z <- matrices("Z")
y <- matrices("y")

grs <- exp(solve(crossprod(Z), crossprod(Z, y)))
head(grs)
#> 6 x 1 Matrix of class "dgeMatrix"
#>                   [,1]
#> 1.2010-02-01 0.9319471
#> 2.2010-02-01 1.0682105
#> 3.2010-02-01 1.0434833
#> 4.2010-02-01 1.0185787
#> 5.2010-02-01 0.9715417
#> 1.2010-03-01 1.0499673
```

There are various inverse-variance (or interval) weighting schemes found
in the literature. These weights are the result of regressing the
residuals from the GRS model against a function of the holding period.
The most well-known of these weights, the Case-Shiller weights, model
variance as a linear function of holding period.

``` r

grs_resid <- y - Z %*% log(grs)

mdl <- lm(as.numeric(grs_resid)^2 ~ prices$holding_period)
W <- Diagonal(x = 1 / fitted.values(mdl))

grs_cs <- exp(solve(crossprod(Z, W %*% Z), crossprod(Z, W %*% y)))
head(grs_cs)
#> 6 x 1 Matrix of class "dgeMatrix"
#>                   [,1]
#> 1.2010-02-01 0.9216778
#> 2.2010-02-01 1.0395309
#> 3.2010-02-01 1.0265610
#> 4.2010-02-01 0.9885539
#> 5.2010-02-01 0.9611430
#> 1.2010-03-01 1.0373087
```

Adding the square of the holding period to the model for the variance,
with or without an intercept, is a common variation for the interval
weights. These weights can all be extended to account for the same house
selling more than twice.

Another type of repeat-sales index is the arithmetic repeat sales (ARS)
index. This index uses the \\Z\\, \\X\\, and \\Y\\ matrices.

``` r

X <- matrices("X")
Y <- matrices("Y")

ars <- 1 / solve(crossprod(Z, X), crossprod(Z, Y))
head(ars)
#> 6 x 1 Matrix of class "dgeMatrix"
#>                   [,1]
#> 1.2010-02-01 0.9049071
#> 2.2010-02-01 1.1567195
#> 3.2010-02-01 1.0093530
#> 4.2010-02-01 1.0107540
#> 5.2010-02-01 0.9458214
#> 1.2010-03-01 0.9840927
```

Like the GRS index, the ARS index can be calculated with
inverse-variance weights.

``` r

ars_resid <- Y - X %*% (1 / ars)

mdl <- lm(as.numeric(ars_resid)^2 ~ prices$holding_period)
W <- Diagonal(x = 1 / fitted.values(mdl))

ars_cs <- 1 / solve(crossprod(Z, W %*% X), crossprod(Z, W %*% Y))
head(ars_cs)
#> 6 x 1 Matrix of class "dgeMatrix"
#>                   [,1]
#> 1.2010-02-01 0.9040179
#> 2.2010-02-01 1.1194659
#> 3.2010-02-01 0.9667189
#> 4.2010-02-01 0.9549480
#> 5.2010-02-01 0.9161033
#> 1.2010-03-01 0.9856805
```

Dividing the \\X\\ and \\Y\\ matrices by the price of the first sale for
each row produces an equally-weighted arithmetic index (as opposed to
value-weighted index).

``` r

ars_ew <- with(
  prices,
  1 / solve(crossprod(Z, X / price_prev), crossprod(Z, Y / price_prev))
)

head(ars_ew)
#> 6 x 1 Matrix of class "dgeMatrix"
#>                   [,1]
#> 1.2010-02-01 0.9831875
#> 2.2010-02-01 1.1454796
#> 3.2010-02-01 1.1332362
#> 4.2010-02-01 0.9957358
#> 5.2010-02-01 0.8859613
#> 1.2010-03-01 0.9510087
```

Once the index is calculated, it’s often easier to turn into a
matrix-like object with cities as rows and time periods as columns. The
easiest way to do this is with the **piar** package.

``` r

library(piar)

dimensions <- do.call(rbind, strsplit(rownames(grs), ".", fixed = TRUE))
grs_piar <- elementary_index(
  grs,
  period = dimensions[, 2],
  ea = dimensions[, 1],
  chainable = FALSE
)

grs_piar[1:5, 1:5]
#> Fixed-base price index for 5 levels over 5 time periods 
#>       time
#> levels 2010-02-01 2010-03-01 2010-04-01 2010-05-01 2010-06-01
#>      1  0.9319471   1.049967  1.0312130  0.9332345  1.0074811
#>      2  1.0682105   0.984812  1.0840525  1.0113186  0.9645851
#>      3  1.0434833   1.044671  0.9526141  0.9330665  1.0234300
#>      4  1.0185787   1.048738  1.0757727  1.1258297  1.0099260
#>      5  0.9715417   1.025133  1.0648382  1.0387825  1.0930171
```

## Sales pair contributions

Repeat-sales indexes have a simple interpretation as a type of efficient
back-price imputation. It’s best to include the base period of the index
prior to doing such an imputation.

``` r

grs <- c(setNames(rep(1, 5), paste(1:5, "2010-01-01", sep = ".")), grs[, 1])
ars <- c(setNames(rep(1, 5), paste(1:5, "2010-01-01", sep = ".")), ars[, 1])
```

The GRS index at a point in time is simply the geometric mean of imputed
price relatives. This makes it easy to calculate the percent-change
contribution of each property at each point in time.

``` r

grs_contributions <- Map(
  \(df, df_prev) {
    impute_back <- with(
      df,
      price_prev / grs[paste(city, period_prev, sep = ".")]
    )
    names(impute_back) <- row.names(df)
    impute_forward <- with(df_prev, price / grs[paste(city, period, sep = ".")])
    names(impute_forward) <- row.names(df_prev)
    geometric_contributions(
      c(df$price / impute_back, df_prev$price_prev / impute_forward)
    )
  },
  split(prices, interaction(prices$city, prices$period)),
  split(prices, interaction(prices$city, prices$period_prev))
)

all.equal(sapply(grs_contributions, sum) + 1, grs)
#> [1] TRUE

range(unlist(grs_contributions))
#> [1] -0.008215347  0.009111019
```

The same applies to the ARS index, noting that it is a weighted
arithmetic mean of imputed price relatives.

``` r

ars_contributions <- Map(
  \(df, df_prev) {
    impute_back <- with(
      df,
      price_prev / ars[paste(city, period_prev, sep = ".")]
    )
    names(impute_back) <- row.names(df)
    impute_forward <- with(df_prev, price / ars[paste(city, period, sep = ".")])
    names(impute_forward) <- row.names(df_prev)
    arithmetic_contributions(
      c(df$price / impute_back, df_prev$price_prev / impute_forward),
      c(impute_back, impute_forward)
    )
  },
  split(prices, interaction(prices$city, prices$period)),
  split(prices, interaction(prices$city, prices$period_prev))
)

all.equal(sapply(ars_contributions, sum) + 1, ars)
#> [1] TRUE

range(unlist(ars_contributions))
#> [1] -0.07041332  0.08316015
```

[^1]: The GRS index is simply the first-difference estimator for the
    linear model of log price with property fixed effects. This is easy
    enough to compute with a panel-data package like **plm**, but the
    repeat-sales matrices are more convenient when working with sales
    pairs.
