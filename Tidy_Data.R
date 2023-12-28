#Exxercise 3

library(data.table)
library(magrittr)
library(tidyr)

#Section 1
product_dt <- fread(file.path("extdata", "example_product_data.csv"))

str(product_dt)
long_product_dt <- melt(product_dt,
     id.vars = "name",
     measure.vars = c("producta", "productb"),
     variable.name = "product",
     value.name = "quantity")

dcast(long_product_dt, ... ~ product, value.var = "quantity")
product_dt

#section 2
mtcars_dt <- as.data.table(mtcars) 
mtcars_dt[, carname := rownames(mtcars)]

dt1 <- mtcars_dt[5:25,.(carname, mpg, cyl)]
dt2 <- mtcars_dt[1:10, .(carname, gear)]

head(dt1, 3)
head(dt2, 3)

inner <-  merge(dt1, dt2, by = "carname", all = FALSE)
dim(inner)

left <-  merge(dt1, dt2, by = "carname", all.x = TRUE)
dim(left)

outer <- merge(dt1, dt2, by = "carname", all= TRUE)
dim(outer)

#Section 3

weather_dt <- fread(file.path("extdata", "weather.txt"))
head(weather_dt, 5)

long_weather_dt <- melt(weather_dt, 
                        id.vars = c("id", "year", "month", "element"),
                        variable.name = "day",
                        value.name = "temp")
head(long_weather_dt, 5)

unite_weather_dt <- unite(long_weather_dt, 
                          col = date, day, month, year, 
                          sep = " ")
head(unite_weather_dt, 5)

wide_weather_dt <-  dcast(unite_weather_dt, 
                          ... ~ element,
                          value.var = "temp")
head(wide_weather_dt, 5)

#Section 4

files <- list.files("extdata/baby-names", full.names = TRUE)
# See one file
head(fread(files[1]))

# name the list elements by the filenames
names(files) <- basename(files)

# read all files at once into a list of data.tables
tables <- lapply(files, fread)

# bind all tables into one using rbindlist,
# keeping the list names (the filenames) as an id column. 
dt <- rbindlist(tables, idcol = 'filename')

# The data is not tidy because one column contains both year and sex
dt <- separate(dt, col = "filename", into = c("year", "sex"), extra = "drop")
head(dt)

#Section 5

gt <- fread('extdata/eqtl/genotype.txt')
dim(gt)

head(gt[,1:5])

growth <- fread('extdata/eqtl/growth.txt')
head(growth)

head(dt)
summary(dt)

# melt both datasets
gt <- melt(gt, id.vars = 'strain', value.name = 'gt', variable.name='marker')
growth <- melt(growth, id.vars = 'strain', variable.name = 'media', value.name = 'growth_rate')

# merge them by strain. As every row gets merged with every row we 
#need to allow cartesian
dt <- merge(growth, gt, by='strain', allow.cartesian = TRUE)
# convert the categorical entries to factors.
# (Measuring the object size before and after shows that factors require less storage space.)

#object.size(dt)
dt[,gt:= as.factor(gt)] 
dt[,strain:= as.factor(strain)] 
dt[,marker:= as.factor(marker)] 
#object.size(dt)
head(dt)

summary(dt)

library(ggplot2)
ggplot(dt[marker %in% c('mrk_5211', 'mrk_1653')], aes(marker, growth_rate, color=gt)) +
  geom_boxplot() + facet_wrap(~media)





