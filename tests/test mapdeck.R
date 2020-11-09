install.packages("mapdeck")
library(mapdeck)

set_token("pk.eyJ1IjoieHRpbWJlYXUiLCJhIjoiY2tnMHhiNnAwMGJyaTJzcXdqbXU1c3Y0MiJ9.ydGev8EOzUGtIUHeLlZqtQ")
mapdeck(style = mapdeck_style("light"))
mapdeck_style("light")

source("dvf.r")

