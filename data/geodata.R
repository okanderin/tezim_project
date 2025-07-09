install.packages("devtools")
library(geodata)


library(geodata)

devtools::install_github("rspatial/geodata")

# ACCESS-CM2, SSP2-4.5, 2021–2040 ortalama sıcaklık verisi (10 dakikalık çözünürlük)
tavg_ssp245 <- cmip6_world(
  model = "ACCESS-CM2",
  ssp = "ssp245",
  var = "tavg",
  time = "2021-2040",  # << güncel argüman bu!
  res = 10,
  path = tempdir()
)

