# Packages -----
rm(list = ls(all = TRUE))

library(data.table)
library(igraph)
library(purrr)
library(ggplot2)

# Understand First Dataset having each flavor pairs -----
compound = fread("srep00196-s2.csv")
colnames(compound) = c('I1', 'I2', 'weight')
write.csv(compound, 'compound.csv', row.names = FALSE)

length(unique(compound$I1)) # 1455
length(unique(compound$I2)) # 1448

intersect(compound$I1, compound$I2)
unique(compound[, .(I1, I2)])
compound[I1 > I2] # the edgelist has an order between I1 and I2

# Understand Second Datset having recipes -----
# 381 ingredients used in recipes throughout the world
# The analysis only based on the ingredients that appear in any recipe
recipe = fread("srep00196-s3.csv")
unique(recipe$V1)

recipe[recipe == ""] <- NA

possible_pairs = lapply(seq_len(nrow(recipe)), function(i) t(recipe[i, ]))
# possible_pairs

for(i in seq_along(possible_pairs)){
  possible_pairs[[i]] = possible_pairs[[i]][!is.na(possible_pairs[[i]])]
}

# extract one recipe_single and recipe_pairs
recipe_single = lapply(seq_along(possible_pairs), function(i) tryCatch(cbind(i, possible_pairs[[i]][1], t(combn(possible_pairs[[i]][-1], 1))), error = function(e) NULL))
recipe_pairs = lapply(seq_along(possible_pairs), function(i) tryCatch(cbind(i, possible_pairs[[i]][1], t(combn(sort(possible_pairs[[i]][-1]), 2))), error = function(e) NULL))

recipe_single = data.table(do.call(rbind, recipe_single))
recipe_pairs = data.table(do.call(rbind, recipe_pairs))

colnames(recipe_single) <- c('recipe_id', 'country', 'I')
colnames(recipe_pairs) <- c('recipe_id', 'country', 'I1', 'I2')

# Ingredient Category Dataset
category = fread('ingredients_category.csv')
colnames(category) = c('I', 'category') # 'others' in category means I am not sure of its category

recipe_single = merge(recipe_single, category, by = 'I')
write.csv(recipe_single, 'recipe_single.csv', row.names = FALSE)

## Merge recipe pairs dataset with compound dataset
data = merge(recipe_pairs, compound, by = c('I1', 'I2'), all.x = TRUE)
data = data[, .(recipe_id, country, I1, I2, weight)]
data[is.na(weight), weight := 0]
write.csv(data, 'recipe_pairs.csv', row.names = FALSE)

# Create networks -----
compound <- fread('compound.csv')
recipe_pairs = fread("recipe_pairs.csv")
recipe_single = fread("recipe_single.csv")

net1_links = compound[I1 %in% recipe_single$I & I2 %in% recipe_single$I] # 41430
net1 = graph.data.frame(net1_links, directed = FALSE)

recipe_pairs[, prevalence := uniqueN(recipe_id), by = .(I1, I2)]
net2 = unique(recipe_pairs[, .(I1, I2, prevalence)])
colnames(net2)[3] <- 'weight'
net2 = graph.data.frame(net2, directed = FALSE)

# Degree measurement -----
net1_degree = data.frame(degree(net1))
net1_degree$I = row.names(net1_degree)
colnames(net1_degree)[1] = 'degree'
rownames(net1_degree) <- 1:nrow(net1_degree)
net1_degree = net1_degree[,c('I', 'degree')]

net2_degree = data.frame(degree(net2))
net2_degree$I = row.names(net2_degree)
colnames(net2_degree)[1] = 'degree'
rownames(net2_degree) <- 1:nrow(net2_degree)
net2_degree = net2_degree[,c('I', 'degree')]
degree = merge(net1_degree, net2_degree, by = 'I')

cor(degree[2], degree[3])
ggplot(degree, aes(x = degree.x, y = degree.y)) + geom_point() + labs(x = 'compound degree', y = 'recipe degree')

degree[order(-degree[2]), 'I'][1:10] # compound 
degree[order(-degree[3]), 'I'][1:10] # recipe

# closeness measurement -----
net1_closeness = data.frame(closeness(net1)) 
net1_closeness$I = row.names(net1_closeness)
colnames(net1_closeness)[1] = 'closeness'
rownames(net1_closeness) <- 1:nrow(net1_closeness)
net1_closeness = net1_closeness[,c('I', 'closeness')]

net2_closeness = data.frame(closeness(net2))
net2_closeness$I = row.names(net2_closeness)
colnames(net2_closeness)[1] = 'closeness'
rownames(net2_closeness) <- 1:nrow(net2_closeness)
net2_closeness = net2_closeness[,c('I', 'closeness')]

closeness = merge(net1_closeness, net2_closeness, by = 'I')

cor(closeness[2], closeness[3]) # 0.01945944
plot(closeness$closeness.x, closeness$closeness.y)

# Betweenness measurement -----
net1_betweenness = data.frame(betweenness(net1)) 
net1_betweenness$I = row.names(net1_betweenness)
colnames(net1_betweenness)[1] = 'betweenness'
rownames(net1_betweenness) <- 1:nrow(net1_betweenness)
net1_betweenness = net1_betweenness[,c('I', 'betweenness')]

net2_betweenness = data.frame(betweenness(net2))
net2_betweenness$I = row.names(net2_betweenness)
colnames(net2_betweenness)[1] = 'betweenness'
rownames(net2_betweenness) <- 1:nrow(net2_betweenness)
net2_betweenness = net2_betweenness[,c('I', 'betweenness')]

betweenness = merge(net1_betweenness, net2_betweenness, by = 'I')

cor(betweenness[2], betweenness[3]) # 
plot(betweenness$betweenness.x, betweenness$betweenness.y)