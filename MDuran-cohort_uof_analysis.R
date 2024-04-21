rm( list = ls() )

library( sna )
library( network )
library( dplyr ) 

data.2021 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/final%20data%20(2017-2022).csv"
data.raw.2021 <- read.csv(data.2021, header = TRUE, row.names = NULL)

data.raw.2021 <- data.raw.2021 %>%
  filter(year == 2022)
data.raw.2021 <- data.raw.2021 %>%
  #filter(off_race >2) # excludes Alaskan Native/ and Asian pacificlander for ERGM


edgelist <- cbind( data.raw.2021[,2], data.raw.2021[,1] )
uof.2021.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ) 
)
# quality check
length( unique( edgelist[,1] ) )  # number of officers
length( unique( edgelist[,2] ) )  # number of incidents
dim( as.matrix( uof.2021.net ) )  # these should match

mat.2021 <- as.matrix(uof.2021.net) # coerce network into a matrix

actor_ids <- rownames(mat.2021)
event_ids <- colnames(mat.2021)


# Bipartite Network Statisticssumm

# Caculating network statistics


# Density
matrix <- as.matrix(uof.2021.net)
# identify the number of edges in the graph
L <- sum( matrix )
# identify the number of actors in the example
N <- dim( matrix )[1]
# identify the number of events in the example
M <- dim( matrix )[2]
# calculate the density
density.matrix <- L / ( N * M )
density.matrix


actor.deg <- rowSums( mat.2021 )
actor.deg # first set of nodes

event.deg <- colSums( mat.2021 )
event.deg # second set of nodes

# mean degree for actors
mean.actor.deg <- L / N
# mean degree for events
mean.event.deg <- L / M
mean( actor.deg )
mean( event.deg )

# define the standardized scores for actors
actor.size <- actor.deg / M
actor.size
# define the standardized scores for events
event.size <- event.deg / N
event.size

mean( actor.size )
mean( event.size ) # shouldn't these values be different?

# Check the dyadic clustering
#library(tnet)
#uof.2021.tnet <- as.tnet( as.matrix( uof.2021.net ) )
#reinforcement_tm(uof.2021.tnet)


### Bipartite graph for export
# Mapping attributes to objects
officer_genders <- data.raw.2021$off_gender[match(actor_ids, data.raw.2021$off_id)]
officer_uof_count <- data.raw.2021$off_uof_count[match(actor_ids, data.raw.2021$off_id)]
officer_ages <- data.raw.2021$off_age[match(actor_ids, data.raw.2021$off_id)]
officer_yos <- data.raw.2021$off_yos[match(actor_ids, data.raw.2021$off_id)]
officer_uof_count <- data.raw.2021$off_uof_count[match(actor_ids, data.raw.2021$off_id)]
inc_tot_off <- data.raw.2021$inc_off_count[match(event_ids, data.raw.2021$inc_id)]
inc_coh_hom <- data.raw.2021$inc_coh_homophily_ratio[match(event_ids, data.raw.2021$inc_id)]

# Rescale function for sizing nodes
rescale <- function( nchar, low, high ){
  min_d <- min( nchar )
  max_d <- max( nchar )
  rscl  <- ( ( high - low )*( nchar - min_d ) ) / ( max_d - min_d ) + low
  rscl
}

# Attributes objects
v.size.age <- c( officer_ages, rep(0.5, length(event_ids))) 
v.size.yos <- c( officer_yos, rep(0.5, length(event_ids))) 
v.size.uof <- c( officer_uof_count, rep(1, length(event_ids)))
v.size.inc <- c(rep(1, length(actor_ids)), rep(inc_tot_off, length(event_ids)))
v.size.inc.hratio <- c(rep(0.25, length(actor_ids)), rescale(rep(inc_coh_hom, length(event_ids)), 0.5, 2.75))


# Map the colors to the nodes; change attribute object as needed.
cols.gender <- ifelse(officer_genders == 1, "#FFB310", "#990033") # 1 equals females
event_color <- "darkgreen"
vertex_colors <- c(cols.gender, rep(event_color, length(event_ids)))

set.seed(500)
gplot(
  uof.2021.net,
  gmode = "twomode",
  usearrows = FALSE,
  edge.col = "grey",           # note the usage here
  vertex.col = vertex_colors,
  vertex.cex = v.size.inc.hratio,
)


# identify the number of police officers
N <- dim( mat.2021 )[1]

# identify the number of incidents
M <- dim( mat.2021 )[2]

# create the "person" matrix
mat.2021.P <- mat.2021 %*% t( mat.2021 )

# create the "group" matrix
mat.2021.G <- t( mat.2021 ) %*% mat.2021


# Calculate the degree, closeness, betweenness centrality scores for each actor
mat.2021.P.net <-as.network(
  mat.2021.P,
  directed = FALSE)



deg <- degree(mat.2021.P.net, gmode = "graph", cmode = "degree")
# set the number of nodes in the network
g <- dim( as.matrix( mat.2021.P.net ) )[1]
# raw closeness centrality
close <- closeness( mat.2021.P.net, gmode="graph" ) / ( g - 1 )
# raw betweenness centrality
btwn <- round(betweenness( 
  mat.2021.P.net, 
  gmode="graph", 2)
)

cent.dat <- data.frame(
  actor_ids,
  degree = deg,
  closeness = close,
  betweenness = btwn
)

# use the mean() function to calculate the means
mean.deg <- mean( deg )
mean.close <- mean( close )
mean.btwn <- mean( btwn )
# create a table that is easy to read
cent.tab <- data.frame( 
  degree = round( mean.deg, 3 ),
  close = round( mean.close, 3 ),
  between = round( mean.btwn, 3 )
)

# print the table
cent.tab
max(deg) 

min(deg) # shouldn't the minimum degree be 1? Single-officer incidents were removed
# from the final sample, so every officers should be connected with at least one other person.

density(mat.2021.P) # Is this the network density? 


rescale <- function( nchar, low, high ){
  min_d <- min( nchar )
  max_d <- max( nchar )
  rscl  <- ( ( high - low )*( nchar - min_d ) ) / ( max_d - min_d ) + low
  rscl
}

edge.rescale <- function( uniMat, low, high ){
  diag( uniMat ) <- 0
  min_w <- min( uniMat[uniMat != 0] )
  max_w <- max( uniMat[uniMat != 0] )
  rscl <- ( ( high-low )  * ( uniMat[uniMat != 0] - min_w ) ) / ( max_w - min_w ) + low
  rscl
}

edge.shade <- function( uniMat ){
  net.edges <- edge.rescale( uniMat, 0.01, 1 )
  vec.to.color <- as.vector( abs( net.edges ) )
  vec.to.color <- 1 - vec.to.color # subtract 1 to flip the grey function scale.
  edge.cols <- grey( vec.to.color )
  return( edge.cols )
}



set.seed(500)
gplot(
  mat.2021.P.net,
  gmode = "graph",
  edge.col = "lightgrey",           # note the usage here
  vertex.col = "brown",
  vertex.cex = 0.5
  #vertex.cex = rescale(deg, 0.5, 2),
)

set.seed(500)
gplot(
  mat.2021.P.net,
  gmode = "graph",
  edge.col = edge.shade( mat.2021.P ),           # note the usage here
  edge.lwd = edge.rescale( mat.2021.P, 0.3, 10 ), # note the usage here
  vertex.col = "brown",
  vertex.cex = 0.5
  #vertex.cex = rescale(deg, 0.5, 2),
)



# Assigning attributes to the actcors projection 

# Adding attributes to the network

mat.2021.P.net %v% "gender" <- data.raw.2021$off_gender[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "race" <- data.raw.2021$off_race[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "age" <- data.raw.2021$off_age[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "yos" <- data.raw.2021$off_yos[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "uof.count" <- data.raw.2021$off_uof_count[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "off_id" <- data.raw.2021$off_id[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "cohort" <- data.raw.2021$off_cohort[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]

officer_genders <- data.raw.2021$off_gender[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
officer_races   <- data.raw.2021$off_race[match(actor_ids, data.raw.2021$off_id)]
cols.gender <- ifelse(officer_genders == 1, "#FFB310", "#990033") # 1 equals females
id.labels <- mat.2021.P.net %v% "off_id"


set.seed(500)
gplot(
  mat.2021.P.net,
  gmode = "graph",
  edge.col = edge.shade( mat.2021.P ),           # note the usage here
  edge.lwd = edge.rescale( mat.2021.P, 0.3, 10 ), # note the usage here
  vertex.col = cols.gender,
  vertex.cex = rescale(get.vertex.attribute(mat.2021.P.net, "uof.count"), 0.5, 2),
)


# Hold off on making "partner" networks.
# Your nodes represent pairs of officers.

combinations <- which(mat.2021.P > 1, arr.ind = TRUE)
actor_pairs <- data.frame(
  Actor1 = actor_ids[combinations[, 1]],
  Actor2 = actor_ids[combinations[, 2]],
  Ties = mat.2021.P[combinations]
)

# Remove duplicate and self-tie combinations
actor_pairs <- actor_pairs[actor_pairs$Actor1 != actor_pairs$Actor2, ]
actor_pairs2 <- actor_pairs[!duplicated(t(apply(actor_pairs[, 1:2], 1, sort))), ]

# Create a vector of edge attributes (ties)
edge_attrs <- actor_pairs2$Ties

# Create an edgelist from actor_pairs
edgelist <- as.matrix(actor_pairs2[, c("Actor1", "Actor2")])

# Create a network object with edge attributes
net <- as.network(edgelist, matrix.type = "edgelist", directed = FALSE)

# Create a vector of edge attributes (ties)
set.edge.attribute(net, "Ties", edge_attrs)

# Check if the edge attributes are assigned; checks out!
print(get.edge.attribute(net, "Ties"))


# Adding attributes to the network

net %v% "gender" <- data.raw.2021$off_gender[match(network.vertex.names(net), data.raw.2021$off_id)]
net %v% "race" <- data.raw.2021$off_race[match(network.vertex.names(net), data.raw.2021$off_id)]
net %v% "age" <- data.raw.2021$off_age[match(network.vertex.names(net), data.raw.2021$off_id)]
net %v% "yos" <- data.raw.2021$off_yos[match(network.vertex.names(net), data.raw.2021$off_id)]
net %v% "uof.count" <- data.raw.2021$off_uof_count[match(network.vertex.names(net), data.raw.2021$off_id)]
net %v% "off_id" <- data.raw.2021$off_id[match(network.vertex.names(net), data.raw.2021$off_id)]
net %v% "cohort" <- data.raw.2021$off_cohort[match(network.vertex.names(net), data.raw.2021$off_id)]

officer_genders <- data.raw.2021$off_gender[match(network.vertex.names(net), data.raw.2021$off_id)]
officer_races   <- data.raw.2021$off_race[match(actor_ids, data.raw.2021$off_id)]
cols.gender <- ifelse(officer_genders == 1, "red", "blue") # 1 equals females
id.labels <- net %v% "off_id"


# Define colors for race
cols.race <- officer_races
cols.race[officer_races == 1] <- "blue" # Alaskan Native/Native American
cols.race[officer_races == 2] <- "lightgreen" # Asian/Pacific Islander
cols.race[officer_races == 3] <- "lightblue"  # Black
cols.race[officer_races == 4] <- "orange"   # Hispanic
cols.race[officer_races == 5] <- "yellow" # White

# Plot the shared incident partner networks

# Create a function to rescale the continuous edge attribute for color and line width
edge.rescale <- function(attr, low, high) {
  min_attr <- min(attr)
  max_attr <- max(attr)
  rescaled <- (high - low) * (attr - min_attr) / (max_attr - min_attr) + low
  return(rescaled)
}

edge.shade <- function( uniMat ){
  net.edges <- edge.rescale( uniMat, 0.01, 1 )
  vec.to.color <- as.vector( abs( net.edges ) )
  vec.to.color <- 1 - vec.to.color # subtract 1 to flip the grey function scale.
  edge.cols <- grey( vec.to.color )
  return( edge.cols )
}

# Rescale the continuous edge attribute for color and line width
edge_colors <- edge.shade(get.edge.attribute(net, "Ties"))
edge_widths <- edge.rescale(get.edge.attribute(net, "Ties"), 1, 10)


# Network descriptive statistics

deg <- degree(
  net,
  gmode = "graph",
  cmode = "degree"
)

# set the number of nodes in the network
g <- dim( as.matrix( net ) )[1]
# raw closeness centrality
close <- closeness( net, gmode="graph" ) / ( g - 1 )
# raw betweenness centrality
btwn <- round(betweenness( 
  net, 
  gmode="graph", 2)
)

cent.dat <- data.frame(
  id.labels,
  degree = deg,
  closeness = close,
  betweenness = btwn
)


set.seed(500)
gplot(
  net,
  gmode = "graph",
  edge.col = edge_colors,
  edge.lwd = edge_widths,
  vertex.col = cols.gender,
  vertex.cex = 0.5,
  label = get.vertex.attribute(net,),
  main = "Shared Use of Force Incidents in 2021 (sized by degree centrality)",
)

set.seed(500)
gplot(
  net,
  gmode = "graph",
  edge.col = edge_colors,
  edge.lwd = edge_widths,
  vertex.col = cols.gender,
  vertex.cex = rescale(btwn, 0.5, 2),
  main = "Shared Use of Force Incidents in 2021 (sized by betweenness centrality)",
)

# use the mean() function to calculate the means
mean.deg <- mean( deg )
mean.close <- mean( close )
mean.btwn <- mean( btwn )
# create a table that is easy to read
cent.tab <- data.frame( 
  degree = round( mean.deg, 3 ),
  close = round( mean.close, 3 ),
  between = round( mean.btwn, 3 )
)


# Identify edges with highest "Ties" values
top_ties_edges <- order(net %e% "Ties", decreasing = TRUE)[1:10]

# Extract nodes connected by the top "Ties" edges
edge_list <- as.matrix(net)
top_ties_tail <- edge_list[, 1][top_ties_edges]
top_ties_head <- edge_list[, 2][top_ties_edges]
top_ties_nodes <- unique(c(top_ties_tail, top_ties_head))

# Store the top 10 ties nodes as an object
top_ties_nodes_object <- top_ties_nodes

# Print the object
print(top_ties_nodes_object)






# Code to run ERGM analysis
# Degree distribution for gender. This compares the mean degree for each group
round( mean( degree( mat.2021.P.net, gmode = "graph", cmode = "degree" )[mat.2021.P.net %v% "gender" == 1] ), 2 ) # female
round( mean( degree( mat.2021.P.net, gmode = "graph", cmode = "degree" )[mat.2021.P.net %v% "gender" == 0] ), 2 ) # male

# Degree distribution for race This compares the mean degree for each group
round( mean( degree( mat.2021.P.net, gmode = "graph", cmode = "degree" )[mat.2021.P.net %v% "race" == 1] ), 2 ) # Alask/Nat
round( mean( degree( mat.2021.P.net, gmode = "graph", cmode = "degree" )[mat.2021.P.net %v% "race" == 2] ), 2 ) # Asian/Paci
round( mean( degree( mat.2021.P.net, gmode = "graph", cmode = "degree" )[mat.2021.P.net %v% "race" == 3] ), 2 ) # Black
round( mean( degree( mat.2021.P.net, gmode = "graph", cmode = "degree" )[mat.2021.P.net %v% "race" == 4] ), 2 ) # Hispanic
round( mean( degree( mat.2021.P.net, gmode = "graph", cmode = "degree" )[mat.2021.P.net %v% "race" == 5] ), 2 ) # White



# Analysis


age <- mat.2021.P.net %v% "age"
yos <- mat.2021.P.net %v% "yos"
cohort <- mat.2021.P.net %v% "cohort"
uof  <- mat.2021.P.net %v% "uof.count"
gender <- mat.2021.P.net%v% "gender"
race <- mat.2021.P.net%v% "race"


# Create a data frame from node attributes
net_data <- data.frame(age, yos, cohort, uof, gender)
cor <- cor(net_data) #
print(cor) # high correlations among age, yos, and cohort.

# Pick one that is conceptually interesting for topic; run sensitivity analyses

# Model that is exported
library(broom)
library(ergm)
m1 <- ergm(
  mat.2021.P.net ~ edges,
  control = control.ergm(
    seed = 605 ) 
) 

summary(m1)
tidy_results <- tidy(m1)
write.csv(tidy_results, "m1-edges only.csv", row.names = FALSE)


m2 <- ergm(
  mat.2021.P.net ~ edges
  + nodefactor( "gender" )
  + nodematch( "race" )
  + nodecov( "uof.count")
  + absdiff( "cohort" )
,
  control = control.ergm(
    seed = 605 ) 
) 

summary(m2)
tidy_results <- tidy(m2)
write.csv(tidy_results, "m2-best-model.csv", row.names = FALSE)



# This takes too long to run! Anyway to speed it up?
m3 <- ergm(
  mat.2021.P.net ~ edges
  + nodefactor( "gender")
  + nodematch( "race")
  + nodecov( "uof.count" )
  + nodecov( "yos" )
  + nodecov( "age" )
  + absdiff( "cohort" ),
#  + gwesp( decay = 0.25, fixed = TRUE),
  control = control.ergm(
    seed = 605 ) 
) 

summary(m3)
tidy_results <- tidy(m3)
write.csv(tidy_results, "m3-full-model.csv", row.names = FALSE)


m4 <- ergm(
  mat.2021.P.net ~ edges
  + nodefactor( "gender")
  + nodematch( "race")
  + nodecov( "uof.count" )
  + absdiff( "cohort" )
  + gwesp( decay = 0.25, fixed = TRUE),
  control = control.ergm(
    seed = 605 ) 
) 

summary(m4)
tidy_results <- tidy(m3)
write.csv(tidy_results, "m4-gwesp-model.csv", row.names = FALSE)

###### 

# Predicting the number of ties between i and j

library( ergm.count )
mat.2021.P.net <- as.network( mat.2021.P,
                              ignore.eval = FALSE,     # tells function to not ignore the values of the input matrix
                              names.eval = "incidents" # sets the name for the edge weights
)
mat.2021.P.net %v% "gender" <- data.raw.2021$off_gender[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "race" <- data.raw.2021$off_race[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "age" <- data.raw.2021$off_age[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "yos" <- data.raw.2021$off_yos[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "uof.count" <- data.raw.2021$off_uof_count[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "off_id" <- data.raw.2021$off_id[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]
mat.2021.P.net %v% "cohort" <- data.raw.2021$off_cohort[match(network.vertex.names(mat.2021.P.net), data.raw.2021$off_id)]


valueergm <- ergm( mat.2021.P.net 
                    ~ sum 
                    + nodefactor( "gender")
                    + nodefactor( "race")
                    + nodecov( "yos" )
                    + nodecov( "age" )
                    + absdiff( "uof.count")
                    + absdiff( "cohort" ),
                    response = "incidents",
                    reference = ~Poisson, 
                    control = control.ergm(
                      seed = 605 )  )
summary(valueergm)

summary(valueergm)
tidy_results <- tidy(valueergm)
write.csv(tidy_results, "m4.csv", row.names = FALSE)




library(ergm)

summary( 
  mat.2021.P.net            # the network we want summary info on 
  ~ edges           # give the edges; note the ~
  + degree( 0:15 )   # count of degrees 0 through 5
  + triangle        # count of triangles
)

sim2 <- simulate(
  m2,                # the model to simulate from
  nsim = 500,               # ask for 500 simulations
  monitor = ~ triangle,     # we want it to focus on the triangles
  output = "stats",           # just keep the stats, not the networks
  seed = 605                # set the seed to reproduce the results
)

hist( 
  sim2[,"triangle"],  # plot the triangle counts from each simulation
  main = "Simulations \n (X shows observed network)", # add a title
  xlim = c(300,1000 ), # set the x axis limits
  ylim = c( 0,500 ), # set the y axis limits
  xlab = "Number of triangles",  # label the x axis
  ylab = "Number of simulated networks" #label the y axes 
)

# add a mark where the observed count is
points(
  summary( 
    mat.2021.P.net ~ triangle ),
  4, 
  pch="X", 
  cex=2, 
  col="red"
)

library(ergm)
m1.gof <- gof(
  m2,               # our estimated model
  GOF = ~degree            # the degree distribution 
  + espartners           # the edge-wise shared partners distribution
  + distance,            # the geodesic distance distribution
  verbose = TRUE,          # we want it to tell us what it is doing while it is doing it
  control = control.gof.ergm( seed = 605 ) # set the seed to reproduce results 
)

par( mfrow = c( 2,2 ) )

plot( m1.gof )












