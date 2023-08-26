## ----setup--------------------------------------------------------------------
library(rsmatrix)
library(Matrix)

## ----data---------------------------------------------------------------------
set.seed(15243)

periods <- seq(as.Date("2010-01-01"), as.Date("2019-12-31"), "day")

prices <- data.frame(sale = sample(periods, 5e5, TRUE),
                     property = factor(sprintf("%05d", sample(1:5e4, 5e5, TRUE))),
                     city = factor(sample(1:5, 1e5, TRUE)),
                     price = round(rlnorm(5e5) * 5e5, -3))

prices <- prices[order(prices$city, prices$property, prices$sale), ]
row.names(prices) <- NULL

head(prices)

## ----duplicates---------------------------------------------------------------
interaction(prices$city, prices$property, drop = TRUE) |>
  tabulate() |>
  quantile()

## ----yearmon------------------------------------------------------------------
prices$period <- cut(prices$sale, "month")

## ----pairs--------------------------------------------------------------------
sales_pairs <- rs_pairs(prices$sale, interaction(prices$city, prices$property))
prices[c("price_prev", "period_prev")] <- prices[sales_pairs, c("price", "period")]

head(prices)

## ----removal1-----------------------------------------------------------------
prices$holding_period <- with(prices, as.numeric(period) - as.numeric(period_prev))

prices <- subset(prices, holding_period > 2)

## ----removal2-----------------------------------------------------------------
library(gpindex)
monthly_return <- with(prices, (price / price_prev)^(1 / holding_period))

robust_z <- grouped(robust_z)
prices <- subset(prices, !robust_z(monthly_return, group = city))

head(prices)

## ----matrices-----------------------------------------------------------------
matrices <- with(
  prices,
  rs_matrix(period, period_prev, price, price_prev, city, sparse = TRUE)
)

## ----grs----------------------------------------------------------------------
Z <- matrices("Z")
y <- matrices("y")

grs <- exp(solve(crossprod(Z), crossprod(Z, y)))
head(grs)

## ----weights------------------------------------------------------------------
grs_resid <- y - Z %*% log(grs)

mdl <- lm(as.numeric(grs_resid)^2 ~ prices$holding_period)
W <- Diagonal(x = 1 / fitted.values(mdl))

grs_cs <- exp(solve(crossprod(Z, W %*% Z), crossprod(Z, W %*% y)))
head(grs_cs)

## ----ars----------------------------------------------------------------------
X <- matrices("X")
Y <- matrices("Y")

ars <- 1 / solve(crossprod(Z, X), crossprod(Z, Y))
head(ars)

## ----weights2-----------------------------------------------------------------
ars_resid <- Y - X %*% (1 / ars)

mdl <- lm(as.numeric(ars_resid)^2 ~ prices$holding_period)
W <- Diagonal(x = 1 / fitted.values(mdl))

ars_cs <- 1 / solve(crossprod(Z, W %*% X), crossprod(Z, W %*% Y))
head(ars_cs)

## ----weights3-----------------------------------------------------------------
ars_ew <- with(
  prices,
  1 / solve(crossprod(Z, X / price_prev), crossprod(Z, Y / price_prev))
)

head(ars_ew)

## ----contrib------------------------------------------------------------------
grs <- c(setNames(rep(1, 5), paste(1:5, "2010-01-01", sep = ".")), grs[, 1])
ars <- c(setNames(rep(1, 5), paste(1:5, "2010-01-01", sep = ".")), ars[, 1])

## ---- contrib_grs-------------------------------------------------------------
grs_contributions <- Map(
  \(df, df_prev) {
    impute_back <- with(df, price_prev / grs[paste(city, period_prev, sep = ".")])
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

range(unlist(grs_contributions))

## ---- contrib_ars-------------------------------------------------------------
ars_contributions <- Map(
  \(df, df_prev) {
    impute_back <- with(df, price_prev / ars[paste(city, period_prev, sep = ".")])
    names(impute_back) <- row.names(df)
    impute_forward <- with(df_prev, price / ars[paste(city, period, sep = ".")])
    names(impute_forward) <- row.names(df_prev)
    arithmetic_contributions(
      c(df$price / impute_back, df_prev$price_prev / impute_forward),
      c(impute_back, impute_forward))
  },
  split(prices, interaction(prices$city, prices$period)),
  split(prices, interaction(prices$city, prices$period_prev))
)

all.equal(sapply(ars_contributions, sum) + 1, ars)

range(unlist(ars_contributions))

