# Function to remove strips from facets
rm_strips <- function() {

  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_blank()
  )

}
