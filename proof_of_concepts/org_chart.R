library(ggraph)
library(tidygraph)

# Define edges and nodes
edges <- data.frame(
  from = c(
    "Alex", "Taylor", "Taylor", 
    "Jordan",  "Taylor", 
    "Pat", "Jordan", "Jordan",  "Jordan"
  ),
  to = c(
    "Taylor", "Jordan", "Jordan", 
    "Chris",  "Pat", 
    "Jamie", "Morgan", "Blake", "Casey"
  )
)

nodes <- data.frame(
  name = c(
    "Alex", "Taylor", "Jordan", 
    "Chris", "Pat", "Jamie", 
    "Morgan", "Blake", "Casey"
  ),
  role = c(
    "Chief", "Assistant Chief", "Captain", 
    "FF", "Captain", "FF", 
    "FF", "FF", "FF"
  ),
  team = c(
    "Admin", "Admin", "Crew A", 
    "Crew A", "Crew B", "Crew B", 
    "Crew A", "Crew A", "Crew A"
  ),
  rank = c(1, 2, 3, 4, 3, 4, 4, 4, 4)
)

# Convert to a tidygraph object
graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# Plot using ggraph
ggraph(graph, layout = "tree") +  
  geom_edge_link() +  # No arrows specified
  geom_node_point(aes(color = team), size = 6) +
  geom_node_text(aes(label = paste(name, "\n", role), size = 5)) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(color = "Team")
