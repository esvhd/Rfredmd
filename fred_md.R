library(caret)
library(dplyr)
library(tibble)

# pre-processing functions

log.diff <- function(x) {
  # log returns function
  if (is.data.frame(x)) {
    # 2D data types
    diff <- log(x[-1,]) - log(x[-nrow(x),])
  } 
  else {
    # 1D data types
    diff <- log(x[-1]) - log(x[-length(x)])
  }
  return(diff)
}

pct.change <- function(x) {
  if (is.data.frame(x)) {
    pct <- x[-1, ] / x[-nrow(x),]
  }
  else {
    pct <- x[-1] / x[-length(x)]
  }
  return(pct)
}

trans.func <- function(i) {
  # depending on the transform code, return the associated function.
  # first row has the transformation code. Meaning of code can be found here:
  # https://s3.amazonaws.com/files.fred.stlouisfed.org/fred-md/Appendix_Tables_Update.pdf
  # 1: no transform
  # 2: difference
  # 3: squared difference
  # 4: log()
  # 5: log diff
  # 6: squared log diff
  # 7: percentage change
  func <- switch (i,
                  function(x) x,
                  base::diff,
                  function(x) diff(x)^2,
                  function(x) log(x),
                  log.diff,
                  function(x) log.diff(x)^2,
                  pct.change
  )
  return(func)
}

# load raw data
data.file <- '~/data/fred_md_2018_07.csv'
raw <- read.csv(data.file, row.names = 1)
str(raw)

# extract transform code
trans.code <- t(raw[1,])
trans.df <- as.data.frame(trans.code)
colnames(trans.df) <- c('transform')
trans.df <- rownames_to_column(trans.df, 'symbol')

codes <- unique(trans.df$transform)

# code.5 <- trans.df %>%
#   filter(transform == 5)

# take out transform code
fred <- raw[-1, ]

for (c in codes) {
  print(paste('Transform code: ', c))
  # go through each transform code and apply
  f <- trans.func(c)
  
  # find columns that use this transform
  trans <- filter(trans.df, transform == c)
  count <- nrow(fred)
  for (h in trans$symbol) {
    print(paste('Apply to: ', h))
    new.var <- f(fred[,h])
    num <- count - length(new.var)
    new.var <- c(rep(NaN, num), new.var)
    fred[,h] <- new.var
  }
}
# remove first row as it contains NaN for change 
fred <- fred[-1, ]

# save preproc results
write.csv(fred, file='~/data/fred_md_preproc.csv')

## Preproc END

# show nan count by column
na.check <- apply(is.na(fred), 2, sum)
na.check[na.check > 0]

# since 399:712 (1992/4/1), only s&p pe is missing 4x, 
na.check <- apply(is.na(fred[399:nrow(fred)-1,]), 2, sum)
na.check[na.check > 0]
fred[399:403, 1:3]

# check column
fred[is.na(fred$HWI),]

library(ggplot2)
ggplot(raw, aes(y=RPI, x=1:nrow(raw))) + geom_line()


# TODO remove outliers. Paper ignores NaN values and an observation is 
# considered as an outlier if abs(x - median) > 10 * interquantile_range.
# outlier is replayced with NaN