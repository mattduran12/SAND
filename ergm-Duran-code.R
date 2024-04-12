rm( list = ls() )

library( sna )
library( network )
library( dplyr ) 

data.2021 <-"https://raw.githubusercontent.com/mattduran12/SAND/main/analytic%20sample-2021.csv"
data.raw.2021 <- read.csv(data.2021, header = TRUE, row.names = NULL)
edgelist <- cbind( data.raw.2021[,2], data.raw.2021[,1] )
uof.2021.net <- as.network( 
  edgelist, 
  bipartite = length(      # the bipartite count is equal to the length of 
    unique( edgelist[,1] ) # the number of unique ids for officers
  ),
  directed = FALSE
)

# Quality assurance; everything matches!
length( unique( edgelist[,1] ) )  # number of officers
length( unique( edgelist[,2] ) )  # number of incidents
dim( as.matrix( uof.2021.net ) )  # these should match

mat.2021 <- as.matrix(uof.2021.net) # coerce network into a matrix



# Adding attributes to the network

#gender
attrs<- data.raw.2021 %>% 
  select( off_id, off_gender ) %>% 
  group_by( off_id ) %>% 
  arrange( off_id )

uof.2021.net %v% "gender" <- c( 
  attrs$off_gender, 
  rep( NA, dim( as.matrix( uof.2021.net ) )[2] ) # repeat NA to assign to the events
)

#officer cohort
attrs<- data.raw.2021 %>% 
  select( off_id, off_cohort ) %>% 
  group_by( off_id ) %>% 
  arrange( off_id )

uof.2021.net %v% "off.cohort" <- c( 
  attrs$off_cohort, 
  rep( NA, dim( as.matrix( uof.2021.net ) )[2] ) # repeat NA to assign to the events
)

#race
attrs<- data.raw.2021 %>% 
  select( off_id, off_race ) %>% 
  group_by( off_id ) %>% 
  arrange( off_id )

uof.2021.net %v% "race" <- c( 
  attrs$off_race, 
  rep( NA, dim( as.matrix( uof.2021.net ) )[2] ) # repeat NA to assign to the events
)

#age
attrs<- data.raw.2021 %>% 
  select( off_id, off_age ) %>% 
  group_by( off_id ) %>% 
  arrange( off_id )

uof.2021.net %v% "age" <- c( 
  attrs$off_age, 
  rep( NA, dim( as.matrix( uof.2021.net ) )[2] ) # repeat NA to assign to the events
)

#years of service
attrs<- data.raw.2021 %>% 
  select( off_id, off_yos ) %>% 
  group_by( off_id ) %>% 
  arrange( off_id )

uof.2021.net %v% "yos" <- c( 
  attrs$off_yos, 
  rep( NA, dim( as.matrix( uof.2021.net ) )[2] ) # repeat NA to assign to the events
)

#total use of force incidents per officer
attrs<- data.raw.2021 %>% 
  select( off_id, off_uof_count ) %>% 
  group_by( off_id ) %>% 
  arrange( off_id )

uof.2021.net %v% "total.uof" <- c( 
  attrs$off_uof_count, 
  rep( NA, dim( as.matrix( uof.2021.net ) )[2] ) # repeat NA to assign to the events
)

#total officers per incident
attrs<- data.raw.2021 %>% 
  select( inc_id, inc_off_count ) %>% 
  group_by( inc_id ) %>% 
  arrange( inc_id )

uof.2021.net %v% "total.officers.inc" <- c( 
  attrs$inc_off_count, 
  rep( NA, dim( as.matrix( uof.2021.net ) )[1] ) # repeat NA to assign to the events
)

#cohort homophily ratio
attrs<- data.raw.2021 %>% 
  select( inc_id, inc_coh_homophily_ratio ) %>% 
  group_by( inc_id ) %>% 
  arrange( inc_id )

uof.2021.net %v% "coh.homophily.ratio" <- c( 
  attrs$inc_coh_homophily_ratio, 
  rep( NA, dim( as.matrix( uof.2021.net ) )[1] ) # repeat NA to assign to the events
)

uof.2021.net

# Code to run ERGM analysis

library( ergm )

mod <- ergm( 
  uof.2021.net ~ edges 
  + b1cov( "age" )              # degree effect for age
  + b1factor( "gender" )        # degree effect for gender
  + b1factor( "off.cohort" )    # degree effect for cohort
  )

summary( mod )

