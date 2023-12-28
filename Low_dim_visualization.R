library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator 
library(tidyr)
library(ggrepel)

#Section 1
# No coding was needed

#Section 2
data(mpg)
mpg <- as.data.table(mpg)
ggplot(mpg, aes(cty, hwy, color=factor(year))) + 
  geom_point() + geom_smooth(method="lm")

#Section 3

iris <- as.data.table(iris) 
iris

iris_melt <- melt(iris, id.var=c("Species")) 
iris_melt %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~variable)

## With very few bins, we cannot show the bimodal distribution correctly.
iris_melt %>% ggplot(aes(value)) + geom_histogram(bins=5) + facet_wrap(~variable)

## With too many bins, the plot looks spiky
iris_melt %>% ggplot(aes(value)) + geom_histogram(bins=100) + facet_wrap(~variable)

ggplot(iris_melt, aes(variable, value)) + geom_boxplot()

# petal distributions are bimodal, boxplot cannot visualize this property.
p <- ggplot(iris_melt, aes(variable, value)) + geom_boxplot(outlier.shape = NA)
p + geom_jitter(width = 0.3, size = .5)

ggplot(iris_melt, aes(variable, value)) +
  geom_violin() +
  geom_boxplot(width=0.03) # Overlay boxplot to visualize median and IQR.

# We see that petal length and petal width are bimodal.
# As the iris data set has 3 species, the different belong
# to the different species, so we can color the dots by Species. ggplot(iris_melt, aes(variable, value, color = Species)) +
geom_dotplot(binaxis="y", stackdir="centerwhole", dotsize=0.3)

#Section 4
ggplot(iris,aes(Petal.Length,Petal.Width)) + geom_point()

## With coloring
ggplot(iris,aes(Petal.Length,Petal.Width, color=Species)) + geom_point() +
  labs(x = "Petal Length", y = "Petal Width",
       title = "Relationship between petal length and width") +
  theme(plot.title = element_text(hjust=0.5))

# With facets
ggplot(iris,aes(Petal.Length,Petal.Width)) + geom_point() +
  facet_wrap(~Species, scales = "free")

#Section 5

medals_dt <- fread("extdata/medals.csv") 
ggplot(medals_dt, aes(population, total)) + geom_point()

# Problem:
# There are two countries with much larger populations than the rest.
# This ‘distorts’ the plot somewhat, in that a lot of the remaining points are bunched tog
# Solution: log scaling
ggplot(medals_dt, aes(population, total)) + geom_point() + 
  scale_x_log10() + scale_y_log10()

# Overlapping labels
ggplot(medals_dt, aes(population, total)) + geom_point() + scale_x_log10() + 
  scale_y_log10() 

geom_text(aes(label=code))

# Non-overlapping labels with ggrepel
library(ggrepel)

ggplot(medals_dt, aes(population, total)) + geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_text_repel(aes(label=code))

#Section 6
anscombe_reshaped <- anscombe %>%
  as.data.table %>%
  .[, ID := seq(nrow(.))] %>% melt(id.var=c("ID")) %>%
  separate(variable, c("xy", "group"), sep=1) %>% dcast(... ~ xy) %>%
  .[, group := paste0("dataset_", group)]

# Use the functions ‘mean()‘ and ‘sd()‘ and create new columns
anscombe_reshaped[, .(x_mean = mean(x), y_mean = mean(y),
                      x_sd = sd(x),
                      y_sd = sd(y)), by = "group"]

# Group by ‘group‘ and use the function ‘cor()‘
anscombe_reshaped[, .(correlation = cor(x, y)), by = "group"]

# It’s always important to plot the raw data!
# Different distributions can have the same mean and sd. 
ggplot(anscombe_reshaped, aes(x, y)) + 
  geom_point() + facet_wrap(~ group)

boxplots_dt <- fread("extdata/boxplots.csv")
melt(boxplots_dt) %>% ggplot(aes(variable, value)) + geom_boxplot()

melt(boxplots_dt) %>% ggplot(aes(variable, value)) + geom_violin()

#Section 7 

mtcars <- data.table(mtcars)
ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()

## First compute median
mtcars[, medians := median(mpg), by = cyl] ## Quantiles
mtcars[, c("lq", "uq") := .(quantile(mpg, 0.25), quantile(mpg, 0.75)), by = cyl]
## Whiskers
mtcars[, IQR := 1.5 * IQR(mpg), by = cyl]
mtcars[, c("up_IQR", "down_IQR") := .(IQR + uq, lq - IQR)]
## Get the most extreme value within 1.5*IQR
mtcars[mpg < up_IQR, up_whisker := max(mpg),
       by = "cyl"] 
mtcars[mpg > down_IQR,
       down_whisker := min(mpg), by = "cyl"]
## Compute outliers
mtcars[, outlier := (mpg < down_IQR | mpg > up_IQR), by = "cyl"]

## Make the plot
ggplot(mtcars, aes(cyl, medians, ymax = uq, ymin = lq)) +
  geom_crossbar(fill = "white", width = 1.3) +
  geom_segment(aes(cyl, down_whisker, xend = cyl, yend = lq)) + 
  geom_segment(aes(cyl, uq, xend = cyl, yend = up_whisker)) +
  geom_point(data = mtcars[outlier == TRUE], aes(cyl, mpg)) + 
  labs(y = "mpg")











