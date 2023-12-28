library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork) 
library(cowplot)

heights <- fread("extdata/height.csv") %>% na.omit() %>% .[, sex:=as.factor(toupper(sex))]
heights$sex <- replace(heights$sex, heights$sex =='W', 'F') # only one entry for 'A'
heights <- heights[sex!='A']
heights

m <- heights[, lm(height ~ sex + mother + father)]
summary(m)

prediction = data.table(prediction = predict(m), residuals = residuals(m))
ggplot(prediction, aes(prediction, residuals)) + geom_point() + geom_hline(yintercept = 0)

ggplot(prediction, aes(sample = residuals)) + geom_qq() + geom_qq_line()

heights_male <- heights[sex == 'M']
m1 <- heights_male[, lm(height ~ father)]
m1

m2 <- heights_male[, lm(father ~ height)]
m2

heights_male[, predicted_student_height := predict(m1) ] 
heights_male[, predicted_father_height := predict(m2)]
head(heights_male)

ggplot(heights_male) +
  geom_point(aes(father, height)) +
  geom_line(aes(father, predicted_student_height, color = 'predicted_student')) +
  geom_line(aes(predicted_father_height, height, color = 'predicted_father'))

pca_obj <- princomp(heights_male[, .(height, father)])
slope <- pca_obj$loadings["height","Comp.1"] / pca_obj$loadings["father","Comp.1"]
intercept <- pca_obj$center['height'] - pca_obj$center['father'] * slope

ggplot(heights_male) +
  geom_point(aes(father, height)) +
  geom_line(aes(father, predicted_student_height, color = 'predicted_student')) +
  geom_line(aes(predicted_father_height, height, color = 'predicted_father')) +
  geom_abline(aes(intercept = intercept, slope = slope, color = 'pc_1'))

#Section 2

growth <- fread(file.path("extdata/eqtl", "growth.txt"))
growth <- growth %>% melt(id.vars="strain", variable.name='media', 
value.name= "growth_rate")

growth <- growth[media=="YPMalt"]

genotype <- fread(file.path("extdata/eqtl", "genotype.txt"))
genotype <- genotype[, .(strain, mrk_5211, mrk_5091)]

head(genotype)
head(growth)

table <- merge(growth, genotype)
full <- table[, lm(growth_rate ~ mrk_5211 + mrk_5091)]
summary(full)

reduced <- table[, lm(growth_rate ~ mrk_5211)]
anova(reduced, full)

#Section 3

library(datasets)
library(ggplot2)
data(iris)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point()

# Base model
base_model <- lm(Sepal.Width ~ Sepal.Length, data=iris)
base_model

# Different intercepts
model_species_intercept <- lm(Sepal.Width ~ Sepal.Length + Species, data=iris)
model_species_intercept

# Different slopes and intercepts per species
model_species_intercept_slope <- lm(Sepal.Width ~ Sepal.Length*Species, data=iris)
model_species_intercept_slope

# Base model
iris$base_preds <- predict(base_model, newdata=iris)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point() + 
  geom_line(aes(x=Sepal.Length, y=base_preds, color=Species)) +
  facet_grid(~ Species) + ggtitle("Base Model")

# Different intercepts
iris$preds <- predict(model_species_intercept, newdata=iris)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point() +
  geom_line(aes(x=Sepal.Length, y=preds, color=Species)) +
  facet_grid(~ Species) + ggtitle("Different intercepts, same slope")

# Different slopes and intercepts per species
iris$preds <- predict(model_species_intercept_slope, newdata=iris)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point() +
  geom_line(aes(x=Sepal.Length, y=preds, color=Species))+
  facet_grid(~ Species) + ggtitle("Different intercepts and slopes")

# base vs second model
anova(base_model, model_species_intercept)


# base vs third model
anova(model_species_intercept, model_species_intercept_slope)
