rm( list = ls() )

library( sna )
library( network )

# Jacob, the below code is what I'm working on. I'm trying to assign attributes to the police officers and incidents.

data.2017 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/analytic%20sample-2017.csv"
data.raw.2017 <- read.csv(data.2017, header = TRUE, row.names = NULL)
edgelist <- cbind( data.raw.2017[,2], data.raw.2017[,1] )
uof.2017.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ) 
)

mat.2017 <- as.matrix(uof.2017.net) # coerce network into a matrix

N <- dim( mat.2017 )[1] # number of police officers -- looks good
M <- dim( mat.2017 )[2] # number of incidents  -- looks good

uof.2017.net <- as.network(
  mat.2017,
  bipartite = N
)


off.attrb.2017.data <- "https://raw.githubusercontent.com/mattduran12/SAND/main/2017-officer-attributes.csv"
off.attrb.2017 <- read.csv(off.attrb.2017.data, header = TRUE, row.names = NULL)

inc.attrb.2017.data <- "https://raw.githubusercontent.com/mattduran12/SAND/main/2017-incident-attributes.csv"
inc.attrb.2017 <- read.csv(inc.attrb.2017.data, header = TRUE, row.names = NULL)

actor_ids <- rownames(mat.2017)
event_ids <- colnames(mat.2017)

actor_gender <- off.attrb.2017$off_gender[match(actor_ids, off.attrb.2017$off_id)]
set.vertex.attribute(uof.2017.net, "actor_gender", actor_gender)



# Define colors for males and females


officer_genders <- off.attrb.2017$off_gender[match(actor_ids, off.attrb.2017$off_id)]

# Define colors for males and females
officer_colors <- ifelse(officer_genders == "Male", "lightblue", "red")

# Define a color for events
event_color <- "darkgrey"

# Combine colors for officers and events
vertex_colors <- c(officer_colors, rep(event_color, length(event_ids)))

# Plot the network with node colors based on gender
gplot(
  uof.2017.net,
  gmode = "twomode",
  usearrows = FALSE,
  vertex.col = vertex_colors,
  vertex.cex = 1.5,
  label.cex = 1.2,
  main = "2017"
)





# Codes that didn't work but might come in handy later.


off.gender <- off.attrb.2017[,2]
off.race   <- off.attrb.2017[,3]

uof.2017.net %v% "Gender" <- attrs.2017[,3]
uof.2017.net %v% "Race" <- attrs.2017[,4]
uof.2017.net %v% "Age" <- attrs.2017[,5]
uof.2017.net %v% "Years of service" <- attrs.2017[,6]
uof.2017.net %v% "Cohort" <- attrs.2017[,7]
uof.2017.net %v% "Number of officers involved per incidnet" <- attrs.2017[,8]
uof.2017.net %v% "UoF count per officer" <- attrs.2017[,9]
uof.2017.net %v% "Cohort homophily ratio per incident" <- attrs.2017[,10]

