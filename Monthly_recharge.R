
library(terra)
library(tidyverse)
library(lubridate)
library(glue)

dir.create(path = 'temp', showWarnings = FALSE)
terraOptions(tempdir = 'temp')

# # Create results folder
# dir.create(path = 'Results/Rain_m', showWarnings = FALSE)
# dir.create(path = 'Results/Rain_d_res', showWarnings = FALSE)
# dir.create(path = 'Results/Recharge', showWarnings = FALSE)
# dir.create(path = 'Results/CN_res', showWarnings = FALSE)
# dir.create(path = 'Results/Runoff', showWarnings = FALSE)
# dir.create(path = 'Results/Rff_d', showWarnings = FALSE)
# dir.create(path = 'Results/Evapo_res', showWarnings = FALSE)

# Grid to clip layers
grid = terra::rast(xmin = -53.1, xmax = -47, ymin = -23.5, ymax = -19.7, resolution = c(0.0025, 0.0025))
mask = terra::rast('Data/Mask/Mask.tif') %>% terra::resample(grid) %>% terra::trim()

# Temporal information, adjust according with you temporal data set
st <- as.Date('2001/01/01')
en <- as.Date('2021/12/31')
Date <- seq(from = st, to = en, by= 'day')
Date2 <- seq(from = st, to = en, by= 'month')

Year <- seq(from = 2001, to = 2021)

###### Step 1a
###### Organize the daily data set

rain_day_lst = list.files(path = 'Data/Rain_d', pattern = '.nc4', full.names = TRUE)

for(i in 1:length(rain_day_lst)) {
rain1 =     terra::rast(rain_day_lst[i]) %>%
            terra::t() %>%
            terra::flip(direction = 'horizontal') %>%
            terra::flip(direction = 'vertical') %>%
            terra::resample(mask) %>%
            terra::mask(mask)
print(rain1)
file_rf1 = glue('Results/Rain_d_res/Rainfall_GPM_{Date[i]}.tif')
terra::writeRaster(rain1, filename = file_rf1 , overwrite = TRUE)
}

###### Step 1b
###### Convert daily rainfall in monthly
rain_day_lst_res = list.files(path = 'Results/Rain_d_res', pattern = '.tif$', full.names = TRUE)

table_day_rain <- as_tibble(data.frame(Date, rain_day_lst_res)) %>%
                  mutate(Year =  lubridate::year(Date))

table_date     <- as_tibble(data.frame(Date2))%>%
                  mutate(Year =  lubridate::year(Date2))

for(i in 1:length(Year)) {
Y       <- Year[i]
sel_rn1 <- table_day_rain %>% filter(Year == Y)
rain =  terra::rast(sel_rn1$rain_day_lst_res)
d <- time(rain)
m <- as.numeric(format(d, "%m"))
y <- as.numeric(format(d, "%Y"))
ym <- paste0(y, m)
rain_m <- tapp(rain, ym, sum)
sel_dt <- table_date %>% filter(Year == Y)
names(rain_m) = sel_dt$Date2
file_rf = glue('Results/Rain_m/Rainfall_GPM_{sel_dt$Date2}.tif')
terra::writeRaster(rain_m, filename = file_rf , overwrite = TRUE)
print(sel_dt$Date2)
}

gc()

###### Step 2
###### Resample Curve Number

cn_lst1 = list.files(path = 'Data/CN', pattern = 'CN', full.names = TRUE)
cn1 <- terra::rast(cn_lst1) %>% terra::resample(mask) %>%
       terra::mask(mask) %>% terra::as.int() %>% terra::trim()

file_cn = glue('Results/CN_res/CN_res_{Year}.tif')
terra::writeRaster(cn1, filename = file_cn , overwrite = TRUE)

gc()

###### Step 3
###### Runoff calculation

rain_day_lst_res = list.files(path = 'Results/Rain_d_res', pattern = '.tif$', full.names = TRUE)

cn_lst = list.files(path = 'Results/CN_res', pattern = 'CN', full.names = TRUE)

table_day_rain <- as_tibble(data.frame(Date, rain_day_lst_res)) %>%
                  mutate(Year =  lubridate::year(Date))

table_merge    <- as_tibble(data.frame(Year, cn_lst)) %>%
                  merge(table_day_rain, by = 'Year')

#If you need stat again or use another interval can change this object
#Year <- seq(from = 2017, to = 2021)

for(i in 1:length(Date)) {
  cn_lst    <- table_merge$cn_lst
  cn        <- terra::rast(cn_lst[i])
  rain_lst  <- table_merge$rain_day_lst_res
  rainfall  <- terra::rast(rain_lst[i]) %>%
               terra::resample(cn)
  s = (25400 / cn) - 254
  ia = 0.2*s
  runoff    <- ((rainfall - ia)^2)/(rainfall - ia + s)
  runoff[ia > rainfall] = 0
  name_rf   <- Date
  names(runoff) <- name_rf[i]
  file_rf = glue('Results/Rff_d/Runoff_{name_rf[i]}.tif')
  terra::writeRaster(runoff, filename = file_rf , overwrite = TRUE)
  print(runoff)
}


roff_day_lst_res = list.files(path = 'Results/Rff_d', pattern = '.tif$', full.names = TRUE)

roff =  terra::rast(roff_day_lst_res)
d <- time(roff)
m <- as.numeric(format(d, "%m"))
y <- as.numeric(format(d, "%Y"))
ym <- paste0(y, m)
roff_m <- tapp(roff, ym, sum)
names(roff_m) = Date2

file_rf = glue('Results/runoff/Runoff_{Date2}.tif')
terra::writeRaster(roff_m, filename = file_rf , overwrite = TRUE)

gc()


###### Step 4
###### Monthly recharge calculation

et_m_lst   = list.files(path = 'Data/Evapo/GLDAS/', pattern = '.SUB', full.names = TRUE)
rain_m_lst = list.files(path = 'Results/Rain_m', pattern = '.tif', full.names = TRUE)
roff_m_lst = list.files(path = 'Results/Runoff', pattern = '.tif', full.names = TRUE)

Rff <- list()
Etp <- list()
Prc <- list()

# Import results and resample all layer
for(i in 1:length(Date2)) {
  Rff[[i]] <- terra::rast(roff_m_lst[i]) %>%
    terra::resample(mask) %>% terra::mask(mask)
  print(names(Rff[[i]]))
  Etp[[i]] <- terra::rast(et_m_lst[i]) %>%
    terra::resample(mask) %>% terra::mask(mask)
  print(names(Etp[[i]]))
  Prc[[i]] <- terra::rast(rain_m_lst[i]) %>%
    terra::resample(mask) %>% terra::mask(mask)
  print(names(Prc[[i]]))
}

# Recharge calculation
for(i in 1:length(Date2)) {
  PRC <- terra::rast(Prc[i])
  ETP <- terra::rast(Etp[i])
  ETP <- ETP*(60*60*24*30)
  file_etp = glue('Results/Evapo_res/Evapo_{Date2[i]}.tif')
  terra::writeRaster(ETP, filename = file_etp , overwrite = TRUE)
  RFF <- terra::rast(Rff[i])
  Recharge =  PRC - (ETP + RFF)
  #Recharge[Recharge < 0] = NA
  print(Recharge)
  file_rc = glue('Results/Recharge/Recharge_{Date2[i]}.tif')
  terra::writeRaster(Recharge, filename = file_rc , overwrite = TRUE)
  print('Save!')
}

gc()

#########

unlink(x = list.files('temp', full.names = TRUE))
