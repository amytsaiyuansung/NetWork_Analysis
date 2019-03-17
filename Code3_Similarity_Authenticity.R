# Packages -----
library(ggplot2)
library(data.table)
library(igraph)
library(reshape2)
library(proxy)
library(gridExtra)

# Read Data -----
recipe_single = fread('recipe_single.csv')
compound <- fread('compound.csv')
recipe_pairs = fread("recipe_pairs.csv")

# Part 1: 6 most authentic ingredients -----
length(unique(recipe_single$recipe_id)) # 56498

recipe_single[, prevalence_all := uniqueN(recipe_id)/56498, by = I]
recipe_single[, country_recipe := uniqueN(recipe_id), by = country]

recipe_single[, prevalence_country := uniqueN(recipe_id), by = .(I, country)]
recipe_single[, prevalence_country := prevalence_country / country_recipe]
recipe_single[, prevalence_gap := prevalence_country - prevalence_all]

eastasian = unique(recipe_single[country == 'EastAsian', .(I, prevalence_gap)])
latinamerica = unique(recipe_single[country == 'LatinAmerican', .(I, prevalence_gap)])
northamerican = unique(recipe_single[country == 'NorthAmerican', .(I, prevalence_gap)])
southerneuropean = unique(recipe_single[country == 'SouthernEuropean', .(I, prevalence_gap)])
westerneuropean = unique(recipe_single[country == 'WesternEuropean', .(I, prevalence_gap)])

# Authenticity
unique(recipe_single$country)

# compare with network
net1_links = compound[I1 %in% recipe_single$I & I2 %in% recipe_single$I] # 41430
net1 = graph.data.frame(net1_links, directed = FALSE)

net1_degree = data.frame(degree(net1))
net1_degree$I = row.names(net1_degree)
colnames(net1_degree)[1] = 'degree'
rownames(net1_degree) <- 1:nrow(net1_degree)
net1_degree = net1_degree[,c('I', 'degree')]

net1_betweenness = data.frame(betweenness(net1))
net1_betweenness$I = row.names(net1_betweenness)
colnames(net1_betweenness)[1] = 'betweenness'
rownames(net1_betweenness) <- 1:nrow(net1_betweenness)
net1_betweenness = net1_betweenness[,c('I', 'betweenness')]

eastasian = merge(eastasian, net1_degree, by = 'I')
latinamerica = merge(latinamerica, net1_degree, by = 'I')
northamerican = merge(northamerican, net1_degree, by = 'I')
southerneuropean = merge(southerneuropean, net1_degree, by = 'I')
westerneuropean = merge(westerneuropean, net1_degree, by = 'I')

eastasian = merge(eastasian, net1_betweenness, by = 'I')
latinamerica = merge(latinamerica, net1_betweenness, by = 'I')
northamerican = merge(northamerican, net1_betweenness, by = 'I')
southerneuropean = merge(southerneuropean, net1_betweenness, by = 'I')
westerneuropean = merge(westerneuropean, net1_betweenness, by = 'I')

eastasian = eastasian[order(-prevalence_gap)]
latinamerica = latinamerica[order(-prevalence_gap)]
northamerican = northamerican[order(-prevalence_gap)]
southerneuropean = southerneuropean[order(-prevalence_gap)]
westerneuropean = westerneuropean[order(-prevalence_gap)]

head(eastasian,6)
head(latinamerica,6)
head(northamerican,6)
head(southerneuropean,6)
head(westerneuropean,6)

mean(eastasian$degree) # East Asian cuisines top 2 (authenticity: soy_saucec and scallion)
mean(latinamerica$degree)
mean(northamerican$degree)
mean(southerneuropean$degree)
mean(westerneuropean$degree)

# Recipe_pairs
length(unique(recipe_pairs$recipe_id)) # 56144
recipe_pairs[, prevalence_all := uniqueN(recipe_id)/56144, by = .(I1, I2)]
recipe_pairs[, country_recipe := uniqueN(recipe_id), by = country]

recipe_pairs[, prevalence_country := uniqueN(recipe_id), by = .(I1, I2, country)]
recipe_pairs[, prevalence_country := prevalence_country / country_recipe]
recipe_pairs[, prevalence_gap := prevalence_country - prevalence_all]
recipe_pairs

eastasian_pairs = unique(recipe_pairs[country == 'EastAsian', .(I1, I2, weight, prevalence_gap)])
latinamerica_pairs = unique(recipe_pairs[country == 'LatinAmerican', .(I1, I2, weight, prevalence_gap)])
northamerican_pairs = unique(recipe_pairs[country == 'NorthAmerican', .(I1, I2, weight, prevalence_gap)])
southerneuropean_pairs = unique(recipe_pairs[country == 'SouthernEuropean', .(I1, I2, weight, prevalence_gap)])
westerneuropean_pairs = unique(recipe_pairs[country == 'WesternEuropean', .(I1, I2, weight, prevalence_gap)])

eastasian_pairs = eastasian_pairs[order(-prevalence_gap)]
latinamerica_pairs = latinamerica_pairs[order(-prevalence_gap)]
northamerican_pairs = northamerican_pairs[order(-prevalence_gap)]
southerneuropean_pairs = southerneuropean_pairs[order(-prevalence_gap)]
westerneuropean_pairs = westerneuropean_pairs[order(-prevalence_gap)]

head(eastasian_pairs,6)
head(latinamerica_pairs,6)
head(northamerican_pairs,6)
head(southerneuropean_pairs,6)
head(westerneuropean_pairs,6)

# Part 2: Similarity measure -----
# Jaccard and Cosine Similarity

# Recipe data
recipe_single[, list(ingredients = uniqueN(I)), by = country]

# country ingredients
# 1:          African         197
# 2:        EastAsian         242
# 3:  EasternEuropean         198
# 4:    LatinAmerican         260
# 5:    MiddleEastern         227
# 6:    NorthAmerican         354
# 7: NorthernEuropean         175
# 8:       SouthAsian         205
# 9:   SoutheastAsian         184
# 10: SouthernEuropean         290
# 11:  WesternEuropean         309

tmp = unique(recipe_single[,.(country, I, prevalence_country)])
unique(tmp$country)
m1 = acast(tmp, country~I, value.var="prevalence_country")
m1[is.na(m1)] <- 0
adj = dist(m1, method = "jaccard")
dist = cmdscale(adj, 2)

dist = data.frame(dist)
colnames(dist)[c(1,2)] <- c('Coordinate 1', 'Coordinate 2')
dist['country'] = rownames(dist)

p1 = ggplot(dist, aes(x = `Coordinate 1`, y = `Coordinate 2`, color = country, label = country)) + 
  geom_text(aes(label = country), nudge_x = 0, nudge_y = 0, size = 3) + 
  xlim(-0.5, 0.5) +
  labs(title = 'Jaccard') +
  theme_minimal() + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

adj_cos = dist(m1, method = "cosine")
dist_cos = cmdscale(adj_cos, 2)

dist_cos = data.frame(dist_cos)
colnames(dist_cos)[c(1,2)] <- c('Coordinate 1', 'Coordinate 2')
dist_cos['country'] = rownames(dist_cos)

p2 = ggplot(dist_cos, aes(x = `Coordinate 1`, y = `Coordinate 2`, color = country, label = country)) + 
  geom_text(aes(label = country), nudge_x = 0, nudge_y = 0, size = 3) + 
  xlim(-0.5, 0.5) +
  labs(title = 'Cosine: Ingredient Prevalence Matters') +
  theme_minimal() + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2, nrow = 1)