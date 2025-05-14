############################# Labor & Demographic ##############################
################################# EPGE/FGV #####################################
########################### Midterm Problem Set ################################
######### Authors: Gabriel Tabak, Guilherme Cabrine & Gustavo Henrique #########

#### Loading/Installing needed packages ####
pacman::p_load(tidyverse,
               estimatr,
               stats,
               stargazer,
               haven,
               here,
               patchwork,
               readxl)

#### Importing raw databases ####
# SDEM data
sdem2010 = read.csv(here("Data/Raw", "SDEMT110.csv")) %>% 
  mutate(year = 2010)
sdem2011 = read.csv(here("Data/Raw", "SDEMT111.csv")) %>% 
  mutate(year = 2011)
sdem2012 = read.csv(here("Data/Raw", "SDEMT112.csv")) %>% 
  mutate(year = 2012)
sdem2013 = read.csv(here("Data/Raw", "SDEMT113.csv")) %>% 
  mutate(year = 2013)
sdem2014 = read.csv(here("Data/Raw", "SDEMT114.csv")) %>% 
  mutate(year = 2014)
sdem2015 = read.csv(here("Data/Raw", "SDEMT115.csv")) %>% 
  mutate(year = 2015)
sdem2016 = read.csv(here("Data/Raw", "SDEMT116.csv")) %>% 
  mutate(year = 2016)
sdem2017 = read.csv(here("Data/Raw", "SDEMT117.csv")) %>% 
  mutate(year = 2017)
sdem2018 = read.csv(here("Data/Raw", "SDEMT118.csv")) %>% 
  mutate(year = 2018)
sdem2019 = read.csv(here("Data/Raw", "SDEMT119.csv")) %>% 
  mutate(year = 2019)
sdem2020 = read.csv(here("Data/Raw", "ENOE_SDEMT120.csv")) %>% 
  mutate(year = 2020)

# COE2 Data
cols = c("cd_a",
         "ent",
         "con",
         "v_sel",
         "n_hog",
         "h_mud",
         "n_ren",
         "n_pro_viv",
         "year",
         "p11_h2",
         "p11_m2",
         "p11_h7",
         "p11_m7" ,
         "p11_h5",
         "p11_m5")
coe22010 = read.csv(here("Data/Raw", "COE2T110.csv")) %>% 
  mutate(year = 2010) %>% 
  select(all_of(cols))
coe22011 = read.csv(here("Data/Raw", "COE2T111.csv")) %>% 
  mutate(year = 2011) %>% 
  select(all_of(cols))
coe22012 = read.csv(here("Data/Raw", "COE2T112.csv")) %>% 
  mutate(year = 2012) %>% 
  select(all_of(cols))
coe22013 = read.csv(here("Data/Raw", "COE2T113.csv")) %>% 
  mutate(year = 2013) %>% 
  select(all_of(cols))
coe22014 = read.csv(here("Data/Raw", "COE2T114.csv")) %>% 
  mutate(year = 2014) %>% 
  select(all_of(cols))
coe22015 = read.csv(here("Data/Raw", "COE2T115.csv")) %>% 
  mutate(year = 2015) %>% 
  select(all_of(cols))
coe22016 = read.csv(here("Data/Raw", "COE2T116.csv")) %>% 
  mutate(year = 2016) %>% 
  select(all_of(cols))
coe22017 = read.csv(here("Data/Raw", "COE2T117.csv")) %>% 
  mutate(year = 2017) %>% 
  select(all_of(cols))
coe22018 = read.csv(here("Data/Raw", "COE2T118.csv")) %>% 
  mutate(year = 2018) %>% 
  select(all_of(cols))
coe22019 = read.csv(here("Data/Raw", "COE2T119.csv")) %>% 
  mutate(year = 2019) %>% 
  select(all_of(cols))
coe22020 = read.csv(here("Data/Raw", "ENOE_COE2T120.csv")) %>% 
  mutate(year = 2020) %>% 
  select(all_of(cols))

# Cleaning memory
gc()

#### Cleaning raw databases ####
# Concatenating the SDEM datasets
sdem = rbind(sdem2010,
             sdem2011,
             sdem2012,
             sdem2013,
             sdem2014,
             sdem2015,
             sdem2016,
             sdem2017,
             sdem2018,
             sdem2019,
             sdem2020)

# Removing the individual SDEM datasets
rm(sdem2010,
   sdem2011,
   sdem2012,
   sdem2013,
   sdem2014,
   sdem2015,
   sdem2016,
   sdem2017,
   sdem2018,
   sdem2019,
   sdem2020)

# Cleaning memory
gc()

# Concatenating the COE2 datasets
coe2 = rbind(coe22010,
             coe22011,
             coe22012,
             coe22013,
             coe22014,
             coe22015,
             coe22016,
             coe22017,
             coe22018,
             coe22019,
             coe22020)

# Removing the individual COE2 datasets
rm(coe22010,
   coe22011,
   coe22012,
   coe22013,
   coe22014,
   coe22015,
   coe22016,
   coe22017,
   coe22018,
   coe22019,
   coe22020)

# Cleaning memory
gc()

# Merging the databases
sdem = left_join(sdem, coe2)

# Removing the concatenated COE2 dataset
rm(coe2)

# Cleaning memory
gc()

# Selecting needed columns
sdem = sdem %>% 
  select(cd_a,
         ent,
         con, 
         v_sel, 
         n_hog, 
         h_mud,
         n_ren,
         n_pro_viv,
         year,
         par_c,
         eda,
         hrsocup,
         ing_x_hrs,
         sex,
         n_hij,
         e_con,
         cs_p13_1,
         p11_h2,
         p11_m2,
         p11_h7,
         p11_m7,
         p11_h5,
         p11_m5)

# Exporting concatenated datasets
write_csv(sdem, here("Data/Final", "sdem.csv"))

# Cleaning memory
gc()
