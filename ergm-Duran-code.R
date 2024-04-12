

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


length( unique( edgelist[,1] ) )  == dim( as.matrix( uof.2021.net ) )[1] # Should be true   
length( unique( edgelist[,2] ) )  == dim( as.matrix( uof.2021.net ) )[2] # Should be true   


mat.2021 <- as.matrix(uof.2021.net) # coerce network into a matrix


#----
# write loop to execute the attribute assignment


# first create the data object with the aggregated attributes
attrs <- data.raw.2021 %>% 
  
  # variables you want to have attributes for 
  select( off_id, off_gender, off_cohort, off_race, off_age ) %>% 
  group_by( off_id ) %>% 
  arrange( off_id ) 


# now define the objects to loop through
attrNames <- c( "off_id", "gender", "off.cohort", "race", "age" )


# loop through the attributes list to assign to the network object
for( i in 1: length( attrNames ) ){

  uof.2021.net %v% attrNames[i] <- c( 
    attrs[i], 
    rep( NA, dim( as.matrix( uof.2021.net ) )[2] ) # repeat NA to assign to the events
  )

}

# AND SO ON WITH THE OTHER ATTRIBUTES





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

