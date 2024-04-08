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

N <- dim( mat.2017 )[1] # number of police officers
M <- dim( mat.2017 )[2] # number of incidents

uof.2017.net <- as.network(
  mat.2017,
  bipartite = N
)


# assign the attributes to the network

attrs.2017 <- data.raw.2017

uof.2017.net %v% "Gender" <- attrs.2017[,3]
uof.2017.net %v% "Race" <- attrs.2017[,4]
uof.2017.net %v% "Age" <- attrs.2017[,5]
uof.2017.net %v% "Years of Service" <- attrs.2017[,6]
uof.2017.net %v% "Cohort" <- attrs.2017[,7]
uof.2017.net %v% "Number of officers involved" <- attrs.2017[,8]
uof.2017.net %v% "UoF Count" <- attrs.2017[,9]

