bins = c(20, 6, 20, 20, 15, 10, 30,30)
names(bins) = colnames(X)

means = colMeans(X, na.rm=T)
devs = apply(X, 2, sd, na.rm=T)
ranges = apply(X, 2, range, na.rm=T)

lp_hist <- plyr::llply(colnames(X), function(c) {
  geom_histogram(data = X %>%
                   gather() %>%
                   filter(key == c),
                 aes(x = value),
                 bins = bins[c])
})

p_hist <- Reduce("+", lp_hist, init = ggplot())

p_hist + facet_wrap(~key, scales = "free")
