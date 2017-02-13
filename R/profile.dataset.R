library(reshape2)
library(ggplot2)

single.class <- function(x) {
  y <- class(x)
  return(y[length(y)])
}

profile.data.frame <- function (pdf) {

  density <- sapply(pdf, function(y) sum(length(which(!is.na(y)))))
  sparsity <- sapply(pdf, function(y) sum(length(which(is.na(y)))))
  unique.vals <- sapply(pdf, function(y) length(unique(y)))

  profile <- data.frame(density,sparsity,unique.vals)
  profile$cardnality <- round((profile$unique.vals / (profile$density + profile$sparsity)),5)
  profile$class <- sapply(pdf, single.class )

  profile$NonNumbers <- sapply(pdf, function(y) sum(length(which(is.nan(y)))))
  profile$InfinateValues <- sapply(pdf, function(y) sum(length(which(is.infinite(y)))))


  return(profile)
}

data.set <- data.frame(diamonds, stringsAsFactors = FALSE)

data.profile <- profile.data.frame(data.set)

numeric.attributes <- row.names(data.profile[data.profile$class %in% c('integer','numeric'), ])

d <- melt(data.set[ , numeric.attributes])

ggplot(d, aes(x = value)) +
facet_wrap(~variable, scales = 'free_x', ncol=3) +
geom_histogram(bins = 30)

