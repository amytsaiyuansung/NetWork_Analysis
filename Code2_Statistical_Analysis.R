# Packages -----
rm(list = ls(all = TRUE))

library(dplyr)
library(igraph)
library(purrr)
library(data.table)
library(nnet)
library(plm)
library(dummies)
library(ggplot2)
library(scales)
library(qgraph)
library(plyr)
library(VennDiagram)
library(wesanderson)

# Read data -----
compound <- fread("compound.csv")
category <- fread("ingredients_category.csv")
recipe_pairs <- fread("recipe_pairs.csv")
recipe_single <- fread("recipe_single.csv")
recipe <- fread("srep00196-s3.csv")

#1 Country ~ number of shared compounds per recipe ----
##North American and Western European cuisines exhibit a statistically significant tendency 
##towards recipes whose ingredients share flavor compounds. By contrast, East Asian and 
##Southern European cuisines avoid ,recipes whose ingredients share flavor compounds.
countries <- c("NorthAmerican", "SouthernEuropean", "LatinAmerican", "WesternEuropean", "EastAsian")
mean_id <- recipe_pairs[, .(shared = mean(weight)), .(recipe_id, country)]
count_id <- recipe_single[, .(count = .N), .(recipe_id)]
shared_cmp <- merge(mean_id, count_id, by = "recipe_id", all = TRUE)

model <- multinom(as.factor(country) ~ shared + count, data = shared_cmp)
summary(model)

z <- summary(model)$coefficients/summary(model)$standard.errors
z_top <- z[c(1,3,4,9,10),]
sig <- (1 - pnorm(abs(z), 0, 1)) * 2
result <- merge(summary(model)$coefficients, sig, by = 0, all = TRUE)
colnames(result) <- c("country","intercept","sharedcmp","count", "intercept_sig","sharedcmp_sig", "count_sig")
result_top <- result[result$country %in% countries,]
result_top

#2 Prevalence ~ shared compounds ----
##2-1 Overall model for shared compounds and prevalence
prevalence <- recipe_pairs[, .(prevalence = .N), .(I1, I2, weight)]

model_lm <- lm(prevalence ~ weight, data = prevalence)
summary(model_lm)

##2-2 Prevalence ~ shared compounds (each country)
prevalence_cty <- recipe_pairs[, .(prevalence = .N), .(I1, I2, country,weight)]

model_try <- lm(prevalence ~ weight, data = prevalence_cty[country == "EastAsian",])
summary(model_try)

data <- prevalence_cty[weight != 0 & country == "NorthAmerican",]
graph <- ggplot(data = data, aes(x = weight, y = prevalence)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
graph

#3 probability appear in recipe ~ shared compounds (each country) ----- 
##in North American recipes, the more compounds are shared by two ingredients, the more likely 
##they appear in recipes. By contrast, in East Asian cuisine the more flavor compounds two
##ingredients share, the less likely they are used together.

##3-1 prob using unique weight
weight_unique <- recipe_pairs[, .(pairs_count = length(unique(recipe_id))), .(weight, country)]
total_country <- unique(recipe_pairs[,c("recipe_id", "country")])[, .(total_count = .N), .(country)]
weight_count <- merge(weight_unique, total_country, by = "country", all.x = TRUE)
weight_count$prob <- weight_count$pairs_count/weight_count$total_count
weight_count <- weight_count[weight_count$id_count >5]

graph <- ggplot(data = weight_count[country == "NorthAmerican"], aes(x = weight, y = prob)) + geom_point()
graph

model_lm_NA <- lm(prob ~ weight, data = weight_count[country == "NorthAmerican"])
summary(model_lm_NA)

##3-2 only consider pairs that all have weights
zero_id <- as.data.frame(unique(recipe_pairs[weight == 0]$recipe_id))
colnames(zero_id) <- "recipe_id_zero"
recipe_pairs_nozero <- recipe_pairs[!recipe_id %in% zero_id$recipe_id_zero]

weight_unique <- recipe_pairs_nozero[, .(pairs_count = length(unique(recipe_id))), .(weight, country)]
total_country <- unique(recipe_pairs_nozero[,c("recipe_id", "country")])[, .(total_count = .N), .(country)]
weight_count <- merge(weight_unique, total_country, by = "country", all.x = TRUE)
weight_count$prob <- weight_count$pairs_count/weight_count$total_count
weight_count <- weight_count[weight_count$pairs_count >5]

graph <- ggplot(data = weight_count[country == "NorthAmerican"], aes(x = weight, y = prob)) + geom_point()
graph

model_all <- lm(prob ~ weight, data = weight_count[country == "NorthAmerican"])
summary(model_all)