library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator 
library(tidyr)

#Section 1
#No coding needed

#Section 2
#No coding needed

#Section 3

coffee_dt <- fread("./extdata/coffee_sim.csv") 
coffee_dt

summary(coffee_dt)

# Taken by itself, the plot seems consistent with a causal effect on datavizitis.
ggplot(coffee_dt, aes(coffee_cups_per_day, datavizitis_risk)) + geom_boxplot() +
  labs(x = "Cups of coffee per day",
       y = "Deaths per 1,000")

# This is the way it looks for smoking
ggplot(coffee_dt, aes(packs_cigarettes_per_day, datavizitis_risk)) + geom_boxplot() +
  labs(x = "Packs of cigarette per day",
       y = "Deaths per 1,000")

# and this is the proper way to look at it,
# coffee effects are always the same within each smoking group.
ggplot(coffee_dt, aes(packs_cigarettes_per_day, datavizitis_risk, fill = coffee_cups_per_day)) +
  geom_boxplot() +
  labs(x = "Packs of cigarette per day", y = "Deaths per 1,000") + 
  guides(fill = guide_legend(title = "Cups of coffee"))

# But the effect of smoking is not the same within each
# coffee consumption group.
ggplot(coffee_dt, aes(coffee_cups_per_day, datavizitis_risk, fill = packs_cigarettes_per_day)) + 
  geom_boxplot() + 
  labs(x = "Cups of coffee per day", y = "Deaths per 1,000") + 
  guides(fill = guide_legend(title = "Packs of cigarettes"))

#Section 4
# simulate data
dt <- data.table(pro_uptake = c(rnorm(3, 10100, 300), rnorm(4, 12100, 300), 
                                rnorm(3, 9850, 300), rnorm(4, 11100, 300),
                                rnorm(4,8300, 300), rnorm(3,10050, 300), 
                                rnorm(3, 12000, 300), rnorm(3, 10020, 300), 
                                rnorm(3, 10080, 300), rnorm(3, 10070, 300) ),
                 mutants = c(rep("WT",3), rep("T49A",4), rep("K227N",3), 
                             rep("A400V",4), rep("L421P",4), rep("I500T",3), 
                             rep("N591D",3), rep("A601T",3), rep("E684D",3), 
                             rep("G710R",3) ) )

# sort by median
dt[, median_per_mut := median(pro_uptake), by = mutants]
wt_med = unique(dt[mutants == "WT", median_per_mut])
dt[, mutants:= factor(mutants, levels=unique(dt[order(median_per_mut), mutants]))]

# assign color by relation to WT
dt[, rel_to_wt := ifelse(median_per_mut < wt_med, "Smaller than WT", "Larger than WT"), 
   by = mutants]
dt[mutants == "WT", rel_to_wt := "WT"]

p <- ggplot(dt, aes(mutants, pro_uptake, fill = rel_to_wt)) + geom_boxplot() +
  geom_jitter(width = 0.4) +
  labs(y = "Proline Uptake") +
  theme(axis.text.x = element_text(size=6))
# ggplotly(p)
p

# Another solution with bar plot:
summary_dt <- dt[, .(mean = mean(pro_uptake), sd = sd(pro_uptake)),
                 by = "mutants"]
x_order <- summary_dt[order(mean), mutants]
summary_dt[, mutants := factor(mutants, levels = x_order)] 
dt[, mutants := factor(mutants, levels = x_order)]
# get wt mean
wt <- summary_dt[mutants == "WT", mean]
# group mutants to larger and smaller than wt
summary_dt[, color := ifelse(mean > wt, "Larger",
                             ifelse(mean == wt, "WT", "smaller"))]
ggplot(summary_dt) +
  geom_bar(aes(mutants, mean, fill = color), stat="identity") + 
  geom_errorbar(aes(mutants, ymax=mean+sd, ymin=mean-sd), width = 0.2) + 
  geom_jitter(data = dt, aes(mutants, pro_uptake)) +
  theme(axis.text.x = element_text(size=6))

#Section 5 

fatality_dt <- fread('extdata/belgium_infection_fatality_rate_june2020.csv') 
fatality_dt

fatality_dt <- melt(fatality_dt, id.vars ="age_group",
                    value.name = "fatality_rate", variable.name = "sex")
ggplot(fatality_dt, aes(age_group, fatality_rate, fill=sex)) + 
  geom_col(position="dodge") + 
  geom_text(aes(label=fatality_rate), 
            position=position_dodge(width=0.9), vjust=-0.25, size=2) +
  theme(axis.text.x = element_text(size=6))

#Section 6

datavizitis_smoking_dt <- fread("./extdata/datavizitis_smoking.csv") 
datavizitis_smoking_dt

ggplot(datavizitis_smoking_dt[hospitalized=="Yes"], 
       aes(cigarettes_per_day, datavizitis_severity)) +
  geom_point()+ 
  geom_smooth(method="lm")

ggplot(datavizitis_smoking_dt, aes(cigarettes_per_day, datavizitis_severity)) + 
  geom_point() +
  geom_smooth(method="lm")

ggplot(datavizitis_smoking_dt, aes(cigarettes_per_day, datavizitis_severity)) +
  geom_point(aes(color=hospitalized)) +
  geom_smooth(method="lm") +
  geom_smooth(method="lm", aes(color=hospitalized))

#Section 7

## Load data
titanic <- fread("./extdata/titanic.csv") 
titanic

summary(titanic)


titanic[, mean(survived)]

ggplot(titanic, aes(factor(survived), age)) + geom_boxplot()

ggplot(titanic, aes(x = factor(pclass), fill = factor(survived))) + geom_bar(position = "fill") 

ggplot(titanic, aes(factor(pclass), age)) + geom_boxplot()

library(cowplot)

p1 <- ggplot(titanic, aes(factor(survived), age)) +
  geom_boxplot() + facet_wrap(~factor(pclass))
n_class_surv_dt <- titanic[, .N,by=.(pclass, survived)] 
p2 <- ggplot(n_class_surv_dt, aes(factor(survived), N)) +
  geom_col() +
  facet_wrap(~factor(pclass)) 
plot_grid(
    p1, p2,
    labels = "AUTO", ncol = 1)

                      