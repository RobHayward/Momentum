Momentum Strategy
========================================================

This about momentum strategy.  It is taken from [Ross Bennett's blog](http://rbresearch.wordpress.com/2012/08/23/momentum-with-r-part-1/)  


```r
require(quantstrat)

# Load ETFs from yahoo
currency("USD")
```

```
## [1] "USD"
```

```r
symbols = c("XLY", "XLP", "XLE", "XLF")
stock(symbols, currency = "USD", multiplier = 1)
```

```
## [1] "XLY" "XLP" "XLE" "XLF"
```

```r
getSymbols(symbols, src = "yahoo", index.class = c("POSIXt", "POSIXct"), from = "2000-01-01")
```

```
## [1] "XLY" "XLP" "XLE" "XLF"
```

```r
# Convert to monthly and drop all columns except Adjusted Close
for (symbol in symbols) {
    x <- get(symbol)
    x <- to.monthly(x, indexAt = "lastof", drop.time = TRUE)
    indexFormat(x) <- "%Y-%m-%d"
    colnames(x) <- gsub("x", symbol, colnames(x))
    x <- x[, 6]  #drops all columns except Adjusted Close which is 6th column
    assign(symbol, x)
}
```

There are now four objects that have an adjusted close.Merge the symbols into a single object with just the close prices

```r
symbols_close <- do.call(merge, lapply(symbols, get))
head(symbols_close)
```

```
##            XLY.Adjusted XLP.Adjusted XLE.Adjusted XLF.Adjusted
## 2000-01-31        23.75        17.94        22.57        17.84
## 2000-02-29        22.43        15.84        21.62        15.93
## 2000-03-29        25.56        16.39        24.22        18.78
## 2000-04-28        25.03        17.25        23.86        18.96
## 2000-05-29        23.67        18.49        26.67        19.38
## 2000-06-28        22.39        19.53        25.19        18.44
```

Now calculate the three period rate of change so that the returns can be ranked ot see where the momentum lies. 

```r
roc <- ROC(symbols_close, n = 3, type = "discrete")
head(roc)
```

```
##            XLY.Adjusted XLP.Adjusted XLE.Adjusted XLF.Adjusted
## 2000-01-31           NA           NA           NA           NA
## 2000-02-29           NA           NA           NA           NA
## 2000-03-29           NA           NA           NA           NA
## 2000-04-28      0.05389     -0.03846      0.05716      0.06278
## 2000-05-29      0.05528      0.16730      0.23358      0.21657
## 2000-06-28     -0.12402      0.19158      0.04005     -0.01810
```

Now apply the rank function across each column. The symbol with the highest return has the rank one for each month. This will be an xts object with ranks. 

```r
r <- as.xts(t(apply(-roc, 1, rank)))
head(r)
```

```
##            XLY.Adjusted XLP.Adjusted XLE.Adjusted XLF.Adjusted
## 2000-01-31            1            2            3            4
## 2000-02-29            1            2            3            4
## 2000-03-29            1            2            3            4
## 2000-04-28            3            4            2            1
## 2000-05-29            4            3            1            2
## 2000-06-28            4            1            2            3
```





```r
plot(cars)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


