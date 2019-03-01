###################################################
#Clusters for batching pickups and drop offs
###################################################
# Get data from DB

install.packages("RMySQL")

require(RMySQL)

mydb = dbConnect(MySQL(), user='ucfnhke', password='baleyakuli', dbname='ucfnhke', host='dev.spatialdatacapture.org')

# dbSendQuery builds the query string
# fetch executes the query
# dbClearResults() frees memory resources associated with the query, not clear that its diferent from rm()

test_query = dbSendQuery(mydb, paste("SELECT * FROM cities"))
results = dbFetch(test_query, n=10)
dbClearResult(test_query)

# or?
test_query_1 = dbGetQuery(mydb, paste("SELECT * FROM cities"))


# info about database

names_query_4 = dbSendQuery(mydb, paste("SHOW TABLES;"))
results_4 = dbFetch(names_query_4, n = -1)
dbClearResult(names_query_4)

names_query_5 = dbGetQuery(mydb, paste("Select * FROM cities"))

######################################################################

# convert to BNG


######################################################################
# Do Clustering analysis

my_data = scale(results_4)

fit <- hclust(my_data, method="ward")

plot(fit)

groups <- cutree(fit, k =5)

rect.hclust(fit, k = 5, border = "red")

###############################################################

# Add Cluster ID attribute to dataframe


