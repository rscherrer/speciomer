# Function to add a line
add_line <- function(data, y) geom_line(data = data, mapping = aes(y = get(y)))
