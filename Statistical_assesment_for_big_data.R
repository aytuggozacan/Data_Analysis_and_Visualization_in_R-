library(ggplot2) 
library(data.table) 
library(magrittr)
library(tidyr) 
library(dplyr) 
library(patchwork)

genotype <-fread("extdata/eqtl/genotype.txt") 
growth_rate <-fread("extdata/eqtl/growth.txt")
marker <-fread("extdata/eqtl/marker.txt")

setnames(marker, "id", "marker") 
genotype <- genotype %>%
  melt(id.vars = "strain", variable.name = "marker", value.name = "genotype")

#Section 1

set.seed(10)
# rnorm allows you to draw random values from a normal distribution 
rnorm(10) # 10 random draws

# pnorm gives the cumulative probability from a normal
# i.e. pnorm(x) = p(X < x) where X is the random variable 
pnorm(0)

# it finds x so that pnorm(x) = p
qnorm(0.5)

# qnorm can be used to find different types of quantiles
qnorm(seq(0.25,0.75,0.25)) # quartiles of the normal

qnorm(seq(0.1,0.9,0.1)) # deciles of the normal

set.seed(10)
n <- 100
dt <- data.table(
  observed = rnorm(n) )
# we write functions to plot the histogram
# and the qq-plot
plot_qq <- function(dt, observed_quantiles){
  histo <- ggplot(dt, aes(get(observed_quantiles))) + 
    geom_histogram(bins=20) + 
    xlim(-6,6) +
    labs(x = observed_quantiles)
  
  qq <- ggplot(data = dt, aes(sample = get(observed_quantiles))) + 
    geom_qq(distribution = stats::qnorm) + 
    #geom_qq_line(distribution = stats::qnorm) +
    geom_abline(slope=1,intercept=0) +
    xlim(-6,6) 
  
  histo + qq
  
}

plot_qq(dt, "observed")

dt[, rshift := rnorm(n, mean=4)] 
plot_qq(dt, "rshift")

# Above we can observe, that the observed values cover a larger range than expected.
# Changing the standard deviation of the distribution changes the slope of the Q-Q plot.
# You can find the correct standard deviation by experimenting with values larger than 1.
dt[, rbroad := sort(rnorm(n, sd=2.5))] 
plot_qq(dt, "rbroad")

#Section 2

# we first need to merge the genotype and the growth rate tables.
genotype_growth <- merge(genotype, growth_rate, by = 'strain')
# We can then execute the test for each marker inside a data.table function and extract th
test_res <- genotype_growth[, .(pval=wilcox.test(YPMalt ~ genotype)$p.value), 
                            by='marker'] 
ggplot(test_res, aes(pval)) + 
  geom_histogram(boundary = TRUE, bins=50)

ggplot(test_res[order(pval)], aes(-log10(ppoints(pval)), -log10(pval))) + 
  geom_point() + geom_abline()

# We need to correct for multiple testing, as we just made 1000 tests.
# To do so we use the Benjamini-Hochberg method implemented in the p.adjust function. 
test_res[, padj:=p.adjust(pval, method='BH')]
test_res[padj <= 0.05][order(padj)]

# get marker position
marker_pval <- merge(marker, test_res, by = "marker") ggplot(marker_pval, aes(start, -log10(pval))) +
  geom_point() +
  facet_wrap(~chrom, scales = 'free_x', nrow = 2) + theme_bw() +
  theme(axis.text.x = element_blank())

marker_pval[pval <= 0.05, .N]
marker_pval[padj <= 0.05, .N]

#Section 3 
cars <- as.data.table(mtcars)

# The following variables look quantitative
# mpg, disp, hp, drat, wt, qsec
# The rest are binary or categorical
# we want to test everything with everything
# so we need to go through all the pairs
# Here is ONE way of doing this
# using nested for loops
cols <- c("mpg","disp","hp","drat","wt","qsec") 
n_cols <- length(cols)
n_pairs <- choose(n_cols,2) # the number of pairs # we pre-allocate the table
pvals_table <- data.table(
  var1 = rep("x",n_pairs), 
  var2 = rep("x",n_pairs), 
  corr = rep(0,n_pairs), 
  pval = rep(0,n_pairs)
)
row_idx = 1 # we need to remember where to insert 
for (i in 1:n_cols) {
col1 <- cols[i] # get the first column
# since the order of pairs does not matter
# i.e. cor(disp,hp) = cor(hp,disp)
# the second loop considers only indices > i 
j=i+1
while (j <= n_cols) {
  col2 <- cols[j] # get the second column # add the info to the table pvals_table[row_idx,1] <- col1 pvals_table[row_idx,2] <- col2 pvals_table[row_idx,3] <-
  cars[,cor(get(col1),get(col2),method="spearman")] 
  pvals_table[row_idx,4] <-
    cars[,cor.test(get(col1),get(col2),method="spearman")]$p.value # increment
  j=j+1
  row_idx = row_idx + 1
} }
# We want only significant associations # Let's display the resulting table 
pvals_table[pval < 0.05]

pvals_table[,pval_BH := p.adjust(pval,method="BH")]
pvals_table[pval_BH < 0.05]

# This question asks us to control the FWER
pvals_table[,pval_Bonf := p.adjust(pval,method="bonferroni")] 
pvals_table[pval_Bonf < 0.05]

#Section 4

simulate_norm_groups <- function(sample_size=50, N_experiments=10000, mu_x=0, mu_y=0){ sapply(seq(N_experiments), function(i){
  x <- rnorm(sample_size, mu_x)
  y <- rnorm(sample_size, mu_y)
  t.test(x, y, alternative="two.sided")$p.value
}) }
plot_pval <- function(pvals, title="p-val distribution"){
  pval_dt <- data.table(pvals=pvals)
  histo <- ggplot(pval_dt, aes(pvals)) + geom_histogram(boundary = TRUE) +
    labs(title = title)
  qq <- ggplot(data = pval_dt, aes(sample = pvals)) +
    geom_qq(distribution = stats::qunif, dparams = list(min = 0, max = 1)) + geom_abline(a=0,b=1) +
    ylim(c(0,1)) +
    labs(title = title)
  histo + qq }

set.seed(10)
pvals0 <- simulate_norm_groups(sample_size=50)

plot_pval(pvals0)
pvals0_B <- p.adjust(pvals0, m = 'bonferroni') 
plot_pval(pvals0_B, title = "\nBonferroni adj p-values")

pvals0_BH <- p.adjust(pvals0, m = 'BH')
plot_pval(pvals0_BH, title = "\nBenjamini-Hochberg adj p-values")

#Section 5


plot_pval(simulate_norm_groups(sample_size=10, mu_y=0.5), 
          title="H1: sample size 10")
plot_pval(simulate_norm_groups(sample_size=100, mu_y=0.5), 
          title="H1: sample size 100")
plot_pval(simulate_norm_groups(sample_size=1000, mu_y=0.5),
          title="H1: sample size 1000")

plot_pval_log10 <- function(pvals, title="p val distribution"){ n <- length(pvals)
dt <- data.table(
  observed = -log10(sort(pvals)), expected = -log10(ppoints(n))
)
ggplot(dt) +
  geom_point(aes(expected, observed)) + geom_abline(intercept = 0, slope = 1)
}

pvals <- c(
  simulate_norm_groups(sample_size = 50, N_experiments = 10000), simulate_norm_groups(sample_size = 50, N_experiments = 1000, mu_y=0.5))
plot_pval(pvals, title = "10000 H0 with 1000 H1")

plot_pval_log10(pvals, title = "10000 H0 with 1000 H1")

error_analysis <- function(method='BH', sample_size=50, cut=0.05){ pvals <- c(
  simulate_norm_groups(sample_size = sample_size, N_experiments = 10000), simulate_norm_groups(sample_size = sample_size, N_experiments = 1000, mu_y=0.5))
names(pvals) <- rep(c("H0", "H1"), c(10000, 1000))
pvals_adj <- p.adjust(pvals, method=method)
table(ifelse(pvals_adj < cut, "significant", "not significant"), names(pvals)) }

set.seed(10) 
error_analysis(sample_size = 10)
error_analysis(method="bonferroni", sample_size = 10)
error_analysis(sample_size = 30)
error_analysis(method="bonferroni", sample_size = 30)
error_analysis(sample_size = 50)
error_analysis(method="bonferroni", sample_size = 50)
error_analysis(sample_size = 100)
error_analysis(method="bonferroni", sample_size = 100)
error_analysis(sample_size = 1000)
error_analysis(method="bonferroni", sample_size = 1000)



