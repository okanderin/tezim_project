library(tidyverse)
library(ncdf4)
library(lubridate)
library(purrr)

# 5 örnek istasyon tanımı
stations <- tibble(
  location = c("Hopa", "Zonguldak", "Kartal", "Eğirdir", "Fethiye"),
  lat = c(41.4, 41.5, 40.9, 37.9, 36.6),
  lon = c(41.4, 31.8, 29.2, 30.9, 29.1),
  altitude = c(20, 60, 10, 920, 4)
) %>%
  mutate(lon_adj = ifelse(lon < 0, lon + 360, lon))


extract_nc_var <- function(nc_path, varname, value_transform = identity) {
  nc <- nc_open(nc_path)
  
  lat_vals <- ncvar_get(nc, "lat")
  lon_vals <- ncvar_get(nc, "lon")
  time_vals <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  origin <- as.Date(sub("days since ", "", time_units))
  date_seq <- origin + time_vals
  
  data_array <- ncvar_get(nc, varname)
  nc_close(nc)
  
  get_series <- function(lat_pt, lon_pt) {
    lat_idx <- which.min(abs(lat_vals - lat_pt))
    lon_idx <- which.min(abs(lon_vals - lon_pt))
    value_transform(data_array[lon_idx, lat_idx, ])
  }
  
  stations %>%
    mutate(series = map2(lat, lon_adj, get_series)) %>%
    unnest(series) %>%
    group_by(location) %>%
    mutate(date = date_seq) %>%
    ungroup()
}


tas_df <- extract_nc_var("C:/Users/oderin/Documents/tezim/tn_data/tas.nc", "tas", function(x) x - 273.15) %>%
  rename(tas_C = series)

tasmin_df <- extract_nc_var("C:/Users/oderin/Documents/tezim/tn_data/tasmin.nc", "tasmin", function(x) x - 273.15) %>%
  rename(tasmin_C = series)

tasmax_df <- extract_nc_var("C:/Users/oderin/Documents/tezim/tn_data/tasmax.nc", "tasmax", function(x) x - 273.15) %>%
  rename(tasmax_C = series)

pr_df <- extract_nc_var("C:/Users/oderin/Documents/tezim/tn_data/pr.nc", "pr", function(x) x * 86400) %>%
  rename(pr_mm = series)



hur_nc_path <- "C:/Users/oderin/Documents/tezim/tn_data/hur.nc"
hur_nc <- nc_open(hur_nc_path)
plev_vals <- ncvar_get(hur_nc, "plev")
plev_index <- which.min(abs(plev_vals - 50000))  # 500 hPa

lat_vals <- ncvar_get(hur_nc, "lat")
lon_vals <- ncvar_get(hur_nc, "lon")
time_vals <- ncvar_get(hur_nc, "time")
time_units <- ncatt_get(hur_nc, "time", "units")$value
origin <- as.Date(sub("days since ", "", time_units))
date_seq <- origin + time_vals

hur_array <- ncvar_get(hur_nc, "hur")
nc_close(hur_nc)

get_hur_for_point <- function(lat_pt, lon_pt) {
  lat_idx <- which.min(abs(lat_vals - lat_pt))
  lon_idx <- which.min(abs(lon_vals - lon_pt))
  hur_array[lon_idx, lat_idx, plev_index, ]
}

hur_df <- stations %>%
  mutate(hur_series = map2(lat, lon_adj, get_hur_for_point)) %>%
  unnest(hur_series) %>%
  group_by(location) %>%
  mutate(date = date_seq) %>%
  ungroup() %>%
  rename(hur_percent = hur_series)


full_df <- tas_df %>%
  left_join(tasmin_df, by = c("location", "date")) %>%
  left_join(tasmax_df, by = c("location", "date")) %>%
  left_join(pr_df, by = c("location", "date")) %>%
  left_join(hur_df, by = c("location", "date")) %>%
  select(location, lat = lat.x, lon = lon.x, altitude, date, tas_C, tasmin_C, tasmax_C, pr_mm, hur_percent)

full_df <-  full_df %>%
  mutate(year = lubridate::year(date)) %>% 
  mutate (month = lubridate::month(date)) %>% 
  select(-date)

write.csv(full_df, "C:/Users/oderin/Documents/tezim/tn_data/climate_data.csv", row.names = FALSE)

# POPULATION DATA 

# 1. TÜİK ana senaryo nüfus verisi
nufus_df <- tibble(
  year = 2024:2100,
  total_population = c(85811876, 86239200, 86654276, 87057013, 87447192,
                       87824415, 88188221, 88547948, 88903143, 89253465,
                       89598704, 89938773, 90271639, 90597328, 90915745,
                       91226644, 91529614, 91821961, 92102888, 92371407,
                       92626291, 92866062, 93089166, 93293803, 93477776,
                       93638766, 93774618, 93871269, 93927889, 93944306,
                       93921074, 93859478, 93761603, 93630071, 93467512,
                       93276656, 93060398, 92821683, 92563299, 92287648,
                       91996937, 91693159, 91378435, 91054731, 90723540,
                       90386103, 90043609, 89697193, 89347761, 88995959,
                       88642217, 88286941, 87930099, 87571096, 87209121,
                       86843237, 86472296, 86089309, 85693643, 85284760,
                       84862063, 84425232, 83974302, 83509663, 83032083,
                       82542653, 82042735, 81533869, 81017641, 80495611,
                       79969305, 79440314, 78910082, 78379782, 77850510,
                       77323366, 76799416)  
)

# 2. İlçe oranları
ilce_oran <- tibble(
  location = c("Hopa", "Zonguldak", "Kartal", "Eğirdir", "Fethiye"),
  oran = c(25000, 110000, 500000, 20000, 180000) / 83500000
)

# 3. İlçe bazlı projeksiyon hesaplama
nufus_ilce_df <- expand.grid(year = 2024:2100, location = ilce_oran$location) %>%
  left_join(nufus_df, by = "year") %>%
  left_join(ilce_oran, by = "location") %>%
  mutate(ilce_nufus = round(total_population * oran))



# 4. İlçe bazlı projeksiyon verisini full_df ile birleştirme
full_df <- full_df %>%
  left_join(nufus_ilce_df, by = c("location", "year")) %>%
  select(location, lat, lon, altitude, year, month, tas_C, tasmin_C, tasmax_C, pr_mm, hur_percent, ilce_nufus) %>% 
  mutate(
    tas_C = round(tas_C, 2),
    tasmin_C = round(tasmin_C, 2),
    tasmax_C = round(tasmax_C, 2),
    pr_mm = round(pr_mm, 2),
    hur_percent = round(hur_percent, 2)
  ) 

write_csv(full_df, "full_climate_nufus.csv")



library(dplyr)

full_df <- full_df %>%
  mutate(
    # 1. Sivrisineğin sıcaklığa bağlı ölüm oranı (Brady et al., 2013)
    mu_v = exp(0.05 * (tas_C - 30)) / 10,
    
    # 2. Isırma oranı (a(T)) - Mordecai et al., 2017 fonksiyonu benzeri
    a_T = ifelse(tas_C >= 10 & tas_C <= 35,
                 0.0002 * tas_C * (tas_C - 10) * sqrt(35 - tas_C),
                 0),
    
    # 3. Ekstrinsik inkübasyon periyodu (EIP) ters çevrilmiş hali (1/EIP)
    gamma_v = ifelse(tas_C > 10 & tas_C <= 38,
                     1 / (exp(6.203 - 0.1133 * tas_C + 0.0007 * tas_C^2)),
                     0),
    
    # 4. Nem etkisini de içeren düzeltilmiş yaşam süresi (L_adj)
    L_adj = ifelse(tas_C > 0 & tas_C < 45 & hur_percent > 0,
                   1 / mu_v * (hur_percent / 100),
                   NA)
  )

