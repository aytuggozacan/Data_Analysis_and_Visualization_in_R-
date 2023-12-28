library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork) # optional, makes plots 

genotype <- fread("./extdata/eqtl/genotype.txt")
genotype <- melt(genotype, id.vars = "strain", variable.name = "marker",
                 value.name = "genotype")
growth <- fread("./extdata/eqtl/growth.txt")
growth <- melt(growth, id.vars = "strain", variable.name = "media",
               value.name = "growth_rate")
marker <- fread("./extdata/eqtl/marker.txt")

#Section 1

# Plotting the growth rate difference
getMaltoseDt = function(mrk){
  growth_mrk <- merge(growth, genotype[marker %in% mrk, .(strain, genotype, marker)],
                      by = 'strain', allow.cartesian = TRUE)
  growth_mrk[media == "YPMalt"]
}
# boxplot
plot_growth_one_mk <- function(mk){ ggplot(getMaltoseDt(mk), aes(genotype, growth_rate)) + geom_boxplot() +
    labs(title = mk) + theme_bw(base_size = 16)
}
plot_growth_one_mk("mrk_5211")
# Function to calculate the difference of the medians of two genotypes
median_diff <- function(dt){
  dt[genotype == 'Wild isolate', median(growth_rate, na.rm=T)] -
    dt[genotype == 'Lab strain', median(growth_rate, na.rm=T)]
}

# Function to permute the table, plot the resulting histogram # and compute a p-value
p_val_medians <- function(dt, N_permu = 1000){
  # It will return both a pvalue and plot a histogram of T_star
  T_ref <- median_diff(dt)
  T_star <- sapply(1:N_permu, function(x){
    median_diff(dt[, genotype := sample(genotype)]) }) # Plot
  g <- ggplot(data = data.table(T_star = T_star), aes(T_star)) + geom_histogram() +
    geom_vline(aes(xintercept=T_ref, color="T_ref")) + xlim(-3,3)
  print(g) # Needed to render plot inside function call
  # Compute and return the p value
  # First compute each tail seperately
  p_val_right <- (sum(T_star >= T_ref) + 1) / (N_permu + 1)
  p_val_left <- (sum(T_star <= T_ref) + 1) / (N_permu + 1)
  # Then combine the above to obtain the double sided p-value.
  p_val <- 2 * min(p_val_right, p_val_left)
  return(p_val)
}
# Calling the function:
p_val_medians(getMaltoseDt("mrk_5211"))

plot_growth_one_mk("mrk_1653")
p_val_medians(getMaltoseDt("mrk_1653"))

plot_growth_one_mk("mrk_5091")
p_val_medians(getMaltoseDt("mrk_5091"))

#Section 2

mks_geno <- genotype[marker %in% c("mrk_5091", "mrk_5211")] %>%
  spread(marker, genotype)
head(mks_geno)

# Ho: Marker 5091 is not significantly associated with marker 5211
# T statistic: number of times both markers had the same genotype
# OR
# T statistic: number of times both markers had the same genotype / number of strains 
mks_geno <- genotype[marker %in% c("mrk_5091", "mrk_5211")] %>%
spread(marker, genotype)
# Compute the number of times both markers had the same genotype
#T_ref <- mks_geno[mrk_5091 == mrk_5211, .N] # First option
T_ref <- mks_geno[mrk_5091 == mrk_5211, .N]/nrow(mks_geno) # Second option
# permutation
N_permu <- 1000
T_star <- sapply(1:N_permu, function(x){
  mks_geno[mrk_5091 == sample(mrk_5211), .N]/nrow(mks_geno)}) # other alternative
# plot distribution
ggplot(data = data.table(T_star = T_star), aes(T_star)) + geom_histogram() +
  geom_vline(aes(xintercept=T_ref, color="T_ref")) + xlim(0,1)
# compute p-value
p_val <- (sum(T_star >= T_ref) + 1) / (N_permu + 1)
p_val

#Section 3

# Add growth in maltose (YPMalt) to the genotype data
conditioning_dt <- merge(mks_geno, growth[media == "YPMalt"], by = "strain")
a <- ggplot(conditioning_dt, aes(mrk_5211, growth_rate)) +
  geom_boxplot() +
  facet_wrap(~ mrk_5091) +
  labs(title="conditioned on marker 5091")
b <- ggplot(conditioning_dt, aes(mrk_5091, growth_rate)) +
  geom_boxplot() +
  facet_wrap(~ mrk_5211) +
  labs(title="conditioned on marker 5211")
a / b #Patchwork syntax to nicely align plots

p_val_condition_on <- function(test_mrk = "mrk_5078", condition_mrk = "mrk_5211", N_permu = 1000) {
                               # On the simple growth vs genotype case:
                               ## Ho: For each marker, the growth medians are the same for Lab and Wild
  ## Tref: median(growth on Wild) - median(growth on Lab), for each marker
  # On the growth vs genotype case, conditioned on another marker:
  ## Ho: For each marker, the growth medians are the same for Lab and Wild,
  ## no matter the conditioned marker
  ## Tref: mean across subgroups of {median(growth on Wild) - median(growth on Lab)},
  ## for each marker
  # Prepare data table
  conditioned_dt <- getMaltoseDt(c(test_mrk, condition_mrk)) %>%
    spread(marker, genotype)
  setnames(conditioned_dt, test_mrk, "test_mrk")
  setnames(conditioned_dt, condition_mrk, "condition_mrk")
  # Get T_ref
  median_ref <- conditioned_dt[, median(growth_rate, na.rm=T), 
                               by = c("test_mrk", "condition_mrk")] %>%
spread(test_mrk, V1)
  T_ref <- mean(median_ref[, `Wild isolate` - `Lab strain`]) # Do permutations conditioned on the other marker
  T_star <- numeric(N_permu)
  for(i in 1:N_permu){
    conditioned_dt[, test_mrk := sample(test_mrk), by = condition_mrk]
    medians <- conditioned_dt[, median(growth_rate, na.rm=T), 
                              by = c("test_mrk", "condition_mrk")] %>%
        spread(test_mrk, V1)
    T_star[i] <- mean(medians[, `Wild isolate` - `Lab strain`])
  }
  # Plot
  g <- ggplot(data = data.table(T_star = T_star), aes(T_star)) + geom_histogram() +
    geom_vline(aes(xintercept=T_ref, color="T_ref"))
  print(g)
  # P-value
  p_val <- (sum(T_star >= T_ref) + 1) / (N_permu + 1) 
  p_val
}
p_val_condition_on(test_mrk = "mrk_5091", condition_mrk = "mrk_5211")


p_val_condition_on(test_mrk = "mrk_5211", condition_mrk = "mrk_5091")

#Section 4

# Function to calculate the difference of the medians of two genotypes
median_diff <- function(dt){
  dt[genotype == 'Wild isolate', median(growth_rate, na.rm=T)] -
    dt[genotype == 'Lab strain', median(growth_rate, na.rm=T)]
}
# Bootstrap and compute some function func
boot <- function(x, func, B = 999){
  T_star <- sapply(1:B, function(i){
    xstar <- x[sample(1:.N, replace=TRUE)] 
    func(xstar)
  }
  )
  return(T_star)
}

confint <- function(Tstar, alpha = 0.05){ quantile(Tstar, c(alpha/2, 1-alpha/2))
}

conf_int_plot <- function(mrk){
  x <- getMaltoseDt(mrk)
  T_star <- boot(x , median_diff) # Bootstrap 999 times and compute the median (mystat) 
  T_ref <- median_diff(x)
  CI_lab <- confint(T_star)
  # Plot histogram, add median and confidence interval as vertical lines
  g <- ggplot(data = data.table(T_star = T_star), aes(T_star)) + geom_histogram() +
    geom_vline(data=data.table(T_ref), aes(xintercept=T_ref, color="T_ref")) +
    geom_vline(data=data.table(CI_lab),
               aes(xintercept=CI_lab[1], color="CI"), linetype="dashed") +
    geom_vline(data=data.table(CI_lab),
               aes(xintercept=CI_lab[2], color="CI"), linetype="dashed")
  g 
  
}

conf_int_plot("mrk_5211")
conf_int_plot("mrk_1653")

  
  