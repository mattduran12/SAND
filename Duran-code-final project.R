rm( list = ls() )

library( sna )
library( network )

# Loads the data 

data.2017 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/analytic%20sample-2017.csv"
data.raw.2017 <- read.csv(data.2017, header = TRUE, row.names = NULL)
edgelist <- cbind( data.raw.2017[,2], data.raw.2017[,1] )
uof.2017.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ) 
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

# Events attributes


data.2018 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/analytic%20sample-2018.csv"
data.raw.2018 <- read.csv(data.2018, header = TRUE, row.names = NULL)
edgelist <- cbind( data.raw.2018[,2], data.raw.2018[,1] )
uof.2018.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ) 
)


data.2019 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/analytic%20sample-2019.csv"
data.raw.2019 <- read.csv(data.2019, header = TRUE, row.names = NULL)
edgelist <- cbind( data.raw.2019[,2], data.raw.2019[,1] )
uof.2019.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ) 
)

data.2020 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/analytic%20sample-2020.csv"
data.raw.2020 <- read.csv(data.2020, header = TRUE, row.names = NULL)
edgelist <- cbind( data.raw.2020[,2], data.raw.2020[,1] )
uof.2020.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ) 
)

data.2021 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/analytic%20sample-2021.csv"
data.raw.2021 <- read.csv(data.2021, header = TRUE, row.names = NULL)
edgelist <- cbind( data.raw.2021[,2], data.raw.2021[,1] )
uof.2021.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ) 
)

data.2022 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/analytic%20sample-2022.csv"
data.raw.2022 <- read.csv(data.2022, header = TRUE, row.names = NULL)
edgelist <- cbind( data.raw.2022[,2], data.raw.2022[,1] )
uof.2022.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ) 
)

# Plots

set.seed( 605 )

gplot(
  uof.2017.net,                                  # our network to plot
  gmode = "twomode",                                      # indicate it is two modes
  usearrows = FALSE,
  vertex.cex=1.5,                                           # size the nodes     
  label.cex=1.2,                                          # size the labels
  main="2017"                 # add a title
)


gplot(
  uof.2018.net,                                  # our network to plot
  gmode = "twomode",                                      # indicate it is two modes
  usearrows = FALSE,
  vertex.cex=1.5,                                           # size the nodes     
  label.cex=1.2,                                          # size the labels
  main="2018"                 # add a title
)


gplot(
  uof.2019.net,                                  # our network to plot
  gmode = "twomode",                                      # indicate it is two modes
  usearrows = FALSE,
  vertex.cex=1.5,                                           # size the nodes     
  label.cex=1.2,                                          # size the labels
  main="2019"                 # add a title
)

gplot(
  uof.2020.net,                                  # our network to plot
  gmode = "twomode",                                      # indicate it is two modes
  usearrows = FALSE,
  vertex.cex=1.5,                                           # size the nodes     
  label.cex=1.2,                                          # size the labels
  main="2020"                 # add a title
)

gplot(
  uof.2021.net,                                  # our network to plot
  gmode = "twomode",                                      # indicate it is two modes
  usearrows = FALSE,
  vertex.cex=1.5,                                           # size the nodes     
  label.cex=1.2,                                          # size the labels
  main="2021"                 # add a title
)

gplot(
  uof.2022.net,                                  # our network to plot
  gmode = "twomode",                                      # indicate it is two modes
  usearrows = FALSE,
  vertex.cex=1.5,                                           # size the nodes     
  label.cex=1.2,                                          # size the labels
  main="2022"                 # add a title
)
























