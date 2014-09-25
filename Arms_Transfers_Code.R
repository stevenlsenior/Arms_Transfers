library(igraph)

## Import arms transfer data
read.csv("\trade_register_report", stringsAsFactors = FALSE)

## Set up vectors for data frame
suppliers <- c()
recipients <- c()
weights <- c()

## Set up first row
suppliers[1] <- trade_register_report$Supplier[1]
recipients[1] <- trade_register_report$Recipient[1]
weights[1] <- 1

## For remaining rows, first check that the new row is not a transfer between already listed pair
for(i in 2:length(trade_register_report[,1])){
  if(suppliers[length(suppliers)] == trade_register_report$Supplier[i] && recipients[length(recipients)] == trade_register_report$Recipient[i]){
## If so, increase weight on that pair by 1
    weights[length(weights)] <- (weights[length(weights)] + 1)
  }
## Otherwise add the new supplier to list of suppliers and recipient to list of recipients, set weight to 1
  else{
    suppliers[length(suppliers) + 1] <- trade_register_report$Supplier[i]
    recipients[length(recipients) + 1] <- trade_register_report$Recipient[i]
    weights[length(recipients) + 1] <- 1
  }
}

## Create data frame and igraph object
graph.data <- data.frame(suppliers, recipients, weights, stringsAsFactors = FALSE)
transfers.graph <- graph.data.frame(graph.data)

## Export as a GML file for visualisation and analysis in Gephi
write.graph(at.graph, file = "Arms Transfers", format = "gml")

## Calculate summary statistics for degree
mean(degree(at.graph, mode = "in"))
sd(degree(at.graph, mode = "in"))
mean(graph.strength(at.graph, mode = "in", weights = E(at.graph)$weights))
sd(graph.strength(at.graph, mode = "in", weights = E(at.graph)$weights))

## Plot weighted and unweighted distributions for in and out degree
hist(degree(at.graph, mode = "out"), main = "Out-Degree Distribution", xlab = "Out Degree")
hist(degree(at.graph, mode = "in"), main = "In-Degree Distribution", xlab = "In Degree")
hist(graph.strength(at.graph, mode = "out", weights = E(at.graph)$weights), main = "Weighted Out Degree Distribution", xlab = "Weighted Out Degree")
hist(graph.strength(at.graph, mode = "in", weights = E(at.graph)$weights), main = "Weighted In Degree Distribution", xlab = "Weighted In Degree")