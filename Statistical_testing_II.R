library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(datasets)

gene <- fread("./extdata/eqtl/gene.txt")
genotype <- fread("./extdata/eqtl/genotype.txt")
genotype <- melt(genotype, id.vars = 'strain', variable.name = 'marker',
                 value.name = 'genotype')
growth <- fread("./extdata/eqtl/growth.txt")
growth <- melt(growth, id.vars = "strain", variable.name = 'media',
               value.name = 'growth_rate')
marker <- fread("./extdata/eqtl/marker.txt")

#Section 1
#No coding needed

#Section 2
getMaltoseDt <- function(mrk){
  growth_mrk <- merge(growth, genotype[marker == mrk, .(strain, genotype)],
                      by = 'strain')
  growth_mrk[media == "YPMalt"]
}
# boxplot
plot_growth_one_mk <- function(mk){ ggplot(getMaltoseDt(mk), aes(genotype, growth_rate)) + geom_boxplot() +
    labs(title = mk) + theme_bw(base_size = 16) + theme(plot.title = element_text(hjust = 0.5))
}
plot_growth_one_mk("mrk_5211")

## We can use either Wilcoxon or the t-test (with or without Welch's correction)
m_dt <- getMaltoseDt('mrk_5211')
## Because the growth can be affected in both directions,
## we choose double-sided (default).
## Student's t-Test with Welch correction
### NB: in this case, using an equal variance t-test will give indistinguishable results 
### But generally, using the Welch test is usually the better bet
### As variances are unlikely to be equal for most interesting comparisons
tt <- t.test(alternative="two.sided", growth_rate ~ genotype, m_dt)
### NB: instead of the formula, one can also supply the groups manually:
#tt <- t.test(alternative="two.sided",
#    m_dt[genotype == 'Lab strain', growth_rate],
#    m_dt[genotype == 'Wild isolate', growth_rate]) 
tt

## Wilcoxon Rank Sum Test
w <- wilcox.test(alternative="two.sided", growth_rate ~ genotype, m_dt)
w

test_growth <- function(mk, test){
  m_dt <- getMaltoseDt(mk)
  if(test == 'wilcoxon') {
    pval <- wilcox.test(alternative="two.sided", growth_rate ~ genotype, m_dt)$p.val
  } else {
    pval <- t.test(alternative="two.sided", growth_rate ~ genotype, m_dt)$p.value
  }
  return(pval)
}
# A solution using switch(test, ..) is more elegant
test_growth('mrk_1653', test = 'wilcoxon')

test_growth('mrk_1653', test = 't')

test_growth('mrk_5091', test = 'wilcoxon')

test_growth('mrk_5091', test = 't')

#Section 3

data(iris)
cor_value <- cor.test(iris$Sepal.Length, iris$Sepal.Width, method = "pearson")
cor_value

# cor_value = cor.test(iris$Sepal.Length, iris$Sepal.Width, method = "spearman")
# cor_value
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point() +
  geom_smooth(method=lm)

iris_dt <- as.data.table(iris)
corr_dt <- iris_dt[, cor.test(Sepal.Length, Sepal.Width, method="pearson"),
                   by = Species]
corr_dt

# The code above will not work with method="spearman"
# This is because data table tries to decompose
# the object returned by cor.test into columns
# But for spearman, the attribute "parameter"
# is set to NULL, and data.table does not like that
# Wrapper for spearman, to avoid NULL issue
# spearman_wrapper <- function(x,y) {
#   t <- cor.test(x, y, method="spearman")
#   t$parameter <- "na"
# return(t)
#}
# corr_dt <- iris_dt[, spearman_wrapper(Sepal.Length, Sepal.Width),
#                    by = Species]
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point() +
  geom_smooth(method=lm) +
  #  geom_label(data = corr_dt, mapping = aes(x = 6.5, y = 4.2,
  #    label=paste('Cor = ', round(estimate, 2)))) +
  facet_grid(. ~ Species) + theme_bw(base_size = 18)

#Section 4

## load the data
dt <- fread("extdata/stats-pitfalls.csv")
x <- dt$group1
y <- dt$group2
## Use the boxplot to compare two groups

ggplot(melt(dt), aes(variable, value)) + geom_boxplot() +
  theme_bw(base_size = 18)

## Use the histogram to investigate the different distributions per group
ggplot(melt(dt), aes(value)) + geom_histogram() +
  facet_wrap(~variable, scales = 'free') +
  geom_vline(aes(xintercept=mean(value))) +
  theme_bw(base_size = 18)

# The second group doesn't seem to follow a Normal distribution
## Apply the Wilcoxon test and the t-test
wilcox.test(x,y)$p.value # significant

t.test(x,y)$p.value # not significant

# Which one is correct?
## The boxplot uses the median and not the mean. Therefore we add the mean to it.
ggplot(melt(dt), aes(variable, value)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", col="darkred") +
  theme_bw(base_size = 18)

ex_dt <- fread("extdata/exam_correlation.tsv")
# compute the correlation
per_cor <- ex_dt[,cor.test(attendance, achieved_points, method="pearson")]
per_cor

spr_cor <- ex_dt[,cor.test(attendance, achieved_points, method="spearman")]
spr_cor

# visualize the data and the correlation
ggplot(ex_dt, aes(x=attendance, y=achieved_points)) +
  geom_point() +
  geom_smooth(method=lm) +
  # geom_label(x = 20, y = 80, label = paste('Pearson = ', round(per_cor$estimate, 2))) + # geom_label(x = 20, y = 90, label = paste('Spearman = ', round(spr_cor$estimate, 2)))
  ylim(0, 100)
                   
#Section 5

## First we load the dataset and construct the variables
cars <- as.data.table(mtcars)
cars[, cyl_4 := cyl > 4]
cars[, gear_3 := gear > 3]
cars <- cars[, .(cyl_4, gear_3)]
## To be able to choose a test
## We need to see what kind of data is in our variables ## Clearly both variables are binary
## We thus know we need a Fisher test!
## In R, we use fisher.test
## This expects a contingency table
cont_tbl <- table(cars)
fisher.test(cont_tbl)
## we need to extract the p-value and round it
signif(fisher.test(cont_tbl)$p.value,digits=2)

## For two binary variables
## A positive association occurs
## when one being true implies
## the other is likely true too
## To test for a positive association
## we thus need "alternative = greater".
signif(fisher.test(cont_tbl, alternative = "greater")$p.value,digits=2)

#Section 6

marker_test <- function(marker1, marker2){
  # extract the relevant info from the genotype table 
  mks_geno <- genotype[marker %in% c(marker1, marker2)] %>%
  spread(marker, genotype)
  # convert to a contigency table
  table_markers <- table(mks_geno[, 2:3])
  # run the test
  pval <- fisher.test(table_markers, alternative = 'two.sided')$p.value 
  return(pval)
}
marker_test('mrk_1','mrk_13314')

# get all other markers
other_markers <- marker[id != 'mrk_1']
other_markers[, pval := sapply(id , marker_test, 'mrk_1')]

ggplot(other_markers, aes(start, -log10(pval))) +
  geom_point() +
  facet_wrap(~chrom, scales = 'free_x', nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_blank())

other_markers[, on_chr_1 := .(chrom == "chr01")]
ggplot(other_markers, aes(pval)) +
  geom_histogram(bins=20) +
  facet_wrap(~on_chr_1, scales = 'free_y') +
  theme_bw()

nrow(other_markers[pval < 0.05 & on_chr_1])/nrow(other_markers[(on_chr_1)])
nrow(other_markers[pval < 0.05 & !on_chr_1])/nrow(other_markers[(!on_chr_1)])




