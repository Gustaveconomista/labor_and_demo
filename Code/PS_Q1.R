library(ggplot2)
library(readr)
library(dplyr)
library(patchwork)
library(readxl)

################################################################################
# Ler os dados - SDE

SDE_10 <- read_csv("Data/2010trim1_csv/SDEMT110.csv")
SDE_10$Year <- 2010

SDE_11 <- read_csv("Data/2011trim1_csv/SDEMT111.csv")
SDE_11$Year <- 2011

SDE_12 <- read_csv("Data/2012trim1_csv/SDEMT112.csv")
SDE_12$Year <- 2012

SDE_13 <- read_csv("Data/2013trim1_csv/SDEMT113.csv")
SDE_13$Year <- 2013

SDE_14 <- read_csv("Data/2014trim1_csv/SDEMT114.csv")
SDE_14$Year <- 2014

SDE_15 <- read_csv("Data/2015trim1_csv/SDEMT115.csv")
SDE_15$Year <- 2015

SDE_16 <- read_csv("Data/2016trim1_csv/SDEMT116.csv")
SDE_16$Year <- 2016

SDE_17 <- read_csv("Data/2017trim1_csv/SDEMT117.csv")
SDE_17$Year <- 2017

SDE_18 <- read_csv("Data/2018trim1_csv/SDEMT118.csv")
SDE_18$Year <- 2018

SDE_19 <- read_csv("Data/2019trim1_csv/SDEMT119.csv")
SDE_19$Year <- 2019

SDE_20 <- read_csv("Data/2020trim1_csv/ENOE_SDEMT120.csv")
SDE_20$Year <- 2020

SDE_Total <- rbind(SDE_10,SDE_11,SDE_12,SDE_13,
                   SDE_14,SDE_15,SDE_16,SDE_17,
                   SDE_18,SDE_19,SDE_20)

rm(SDE_10,SDE_11,SDE_12,SDE_13,
   SDE_14,SDE_15,SDE_16,SDE_17,
   SDE_18,SDE_19,SDE_20)

################################################################################
# Ler dados - COE2
COE2_10 <- read_csv("Data/2010trim1_csv/COE2T110.csv")
COE2_10$Year <- 2010

COE2_11 <- read_csv("Data/2011trim1_csv/COE2T111.csv")
COE2_11$Year <- 2011

COE2_12 <- read_csv("Data/2012trim1_csv/COE2T112.csv")
COE2_12$Year <- 2012

COE2_13 <- read_csv("Data/2013trim1_csv/COE2T113.csv")
COE2_13$Year <- 2013

COE2_14 <- read_csv("Data/2014trim1_csv/COE2T114.csv")
COE2_14$Year <- 2014

COE2_15 <- read_csv("Data/2015trim1_csv/COE2T115.csv")
COE2_15$Year <- 2015

COE2_16 <- read_csv("Data/2016trim1_csv/COE2T116.csv")
COE2_16$Year <- 2016

COE2_17 <- read_csv("Data/2017trim1_csv/COE2T117.csv")
COE2_17$Year <- 2017

COE2_18 <- read_csv("Data/2018trim1_csv/COE2T118.csv")
COE2_18$Year <- 2018

COE2_19 <- read_csv("Data/2019trim1_csv/COE2T119.csv")
COE2_19$Year <- 2019

COE2_20 <- read_csv("Data/2020trim1_csv/ENOE_COE2T120.csv")
COE2_20$Year <- 2020

COE2_Total <- rbind(COE2_10,COE2_11,COE2_12,COE2_13,
                    COE2_14,COE2_15,COE2_16,COE2_17,
                    COE2_18,COE2_19,COE2_20)

rm(COE2_10,COE2_11,COE2_12,COE2_13,
   COE2_14,COE2_15,COE2_16,COE2_17,
   COE2_18,COE2_19,COE2_20)

################################################################################
# Ler dados - CPI 
MEXCPI <- read_excel("Data/MEXCPI.xlsx", 
                     sheet = "Annual")
MEXCPI$Year <-  as.numeric(format(MEXCPI$observation_date, "%Y"))
colnames(MEXCPI)[2] <- "CPI"
MEXCPI$observation_date <- NULL

################################################################################
# Q1-a

# Get some household variables 
SDE_Summ <- SDE_Total %>% group_by(cd_a, ent, con, v_sel,
                                   n_hog, h_mud, Year) %>%
  summarise(age_hhead = eda[which(par_c == 101)],
            age_hspouse = first(eda[par_c %in% c(201,202)]),
            chld_less16 = as.integer(sum(par_c %in% c(301,302,303) &
                                           eda < 16) > 0))
   

# Join with the data
SDE_Total <- SDE_Total %>% left_join(SDE_Summ, 
                                     by = c("cd_a", "ent", "con",
                                            "v_sel", "n_hog", "h_mud",
                                            "Year")
                                     )


# Employed 
# hrsocu > 0 and non empty, also, age_hhead > 13 and age_hhead <= 98

SDE_Total <- SDE_Total %>% mutate(Employed = ifelse((hrsocup >0) & 
                                                      (age_hhead <= 98) &
                                                      (age_hhead > 13),
                                                    1, 0))

# Get the conditional hours
SDE_Total$HourC <- ifelse(SDE_Total$Employed == 1,
                          SDE_Total$hrsocup, NA)

#Deflate the salary
SDE_Total <- SDE_Total %>% left_join(MEXCPI, by = c("Year"))
#SDE_Total <- SDE_Total %>% mutate(Wage = 12*ingocup*(100/CPI))
SDE_Total <- SDE_Total %>% mutate(Wage = 52*ing_x_hrs*hrsocup*(100/CPI))  

#ggplot(SDE_Total, aes(Wage)) + geom_histogram()

df1 <- SDE_Total %>% 
  filter(sex == 2, 
         eda %in% 25:65) %>% 
  group_by(Year) %>%
  summarise(EmplR = mean(Employed),
            YWage = mean(Wage),
            Hrs = mean(hrsocup),
            HrsC = mean(HourC, na.rm = T))
  
df1$Year <- as.factor(df1$Year)

g1 <- ggplot(df1, aes(Year,EmplR)) + geom_point() + 
  geom_line(aes(group = 1)) + theme_bw() +
  ylab("Employment")
#ggsave("plot1.png",dpi=600)

g2 <- ggplot(df1, aes(Year,YWage)) + geom_point() + 
  geom_line(aes(group = 1)) +theme_bw() +
  ylab("Mean Yearly Wage")
#ggsave("plot2.png",dpi=600)

g3 <- ggplot(df1, aes(Year,Hrs)) + geom_point() + 
  geom_line(aes(group = 1)) + theme_bw() +
  ylab("Hours Occupied by week Unconditionl")
#ggsave("plot3.png",dpi=600)

g4 <- ggplot(df1, aes(Year,HrsC)) + geom_point() + 
  geom_line(aes(group = 1)) + theme_bw() +
  ylab("Hours Occupied by week Conditionl")
#ggsave("plot4.png",dpi=600)

(g1 | g2) / (g3 | g4)
ggsave("plot1.png",dpi=600)

# SDE_Total$IngC <- ifelse(SDE_Total$Employed == 1,
#                          SDE_Total$ingocup, NA)
# ggplot(df1, aes(Year,YWageC)) + geom_point() +theme_bw() +
#   ylab("Mean Yearly Wage Conditional")
# ggsave("plot4.png",dpi=600)

rm(g1,g2,g3,g4,df1)

################################################################################
# Q1-b

df <- SDE_Total %>% filter(sex == 2,
                           eda %in% 25:65)


df$hj_peq <- ifelse(df$n_hij != 0 & df$chld_less16 != 0 &
                      (df$par_c %in% c(101,201)), 1, 0)


Tables <- function(df1, status){
  df1 <- df1 %>% filter(e_con == status)
  
  final <- matrix(0, 5, 8)
  final <- data.frame(final)
  
  # All
  final[1,c(1,5)] <- c(mean(df1$Employed), mean(df1$Wage/12))
  
  final[1,c(2,6)] <- df1 %>% filter(cs_p13_1 < 4) %>%
    summarise(mean(Employed), mean(Wage/12)) %>% as.numeric()
  
  final[1,c(3,7)] <- df1 %>% filter(cs_p13_1 == 4) %>% 
    summarise(mean(Employed), mean(Wage/12)) %>% as.numeric()
  
  final[1,c(4,8)] <- df1 %>% filter(cs_p13_1 %in% c(7,8,9)) %>% 
    summarise(mean(Employed), mean(Wage/12)) %>% as.numeric()
  
  
  # Less than 35 with young child
  final[2,c(1,5)] <- df1 %>% filter(hj_peq == 1, eda < 35) %>% 
    summarise(mean(Employed), mean(Wage/12)) %>% as.numeric()
  
  final[2,c(2,6)] <- df1 %>% filter(hj_peq == 1, eda < 35, cs_p13_1 < 4) %>% 
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[2,c(3,7)] <- df1 %>% filter(hj_peq == 1, eda < 35, cs_p13_1 == 4) %>% 
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[2,c(4,8)] <- df1 %>% filter(hj_peq == 1, eda < 35,cs_p13_1 %in% c(7,8,9)) %>%
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  
  # Less than 35 with no child
  final[3,c(1,5)] <- df1 %>% filter(hj_peq == 0, eda < 35) %>% 
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[3,c(2,6)] <- df1 %>% filter(hj_peq == 0, eda < 35,cs_p13_1 < 4) %>%
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[3,c(3,7)] <- df1 %>% filter(hj_peq == 0, eda < 35,cs_p13_1 == 4) %>%
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[3,c(4,8)] <- df1 %>% filter(hj_peq == 0, eda < 35,cs_p13_1 %in% c(7,8,9)) %>%
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  
  # Edad: 35-54
  final[4,c(1,5)] <- df1 %>% filter(eda >= 35, eda<=54) %>% summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[4,c(2,6)] <- df1 %>% filter(eda >= 35, eda<=54,cs_p13_1 < 4) %>% 
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[4,c(3,7)] <- df1 %>% filter(eda >= 35, eda<=54,cs_p13_1 == 4) %>%
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[4,c(4,8)] <- df1 %>% filter(eda >= 35, eda<=54,cs_p13_1 %in% c(7,8,9)) %>%
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  
  # Edad: 55+
  final[5,c(1,5)] <- df1 %>% filter(eda >= 55) %>% 
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[5,c(2,6)] <- df1 %>% filter(eda >= 55,cs_p13_1 < 4) %>% 
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[5,c(3,7)] <- df1 %>% filter(eda >= 55,cs_p13_1 == 4) %>%
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  final[5,c(4,8)] <- df1 %>% filter(eda >= 55,cs_p13_1 %in% c(7,8,9)) %>%
    summarise(mean(Employed), mean(Wage/12))%>% as.numeric()
  
  rownames(final) <- c("All", "Less 35 - Young Chld",
                       "Less 35 - No Chld", "35-54",
                       "55+")
  
  colnames(final) <- c("All", "Less HS", "HS", "College+",
                       "All - W", "Less HS - W", "HS - W", "College+ - W")  
  return(final)
}


# status = 5 (Married), 1 (Cohabiting), 6 (Single)

dfM <- Tables(df,5)
dfC <- Tables(df,1)
dfS <- Tables(df,6)

rm(dfM); rm(dfC); rm(dfS); rm(status)

################################################################################
# Q1-c

df2 <- COE2_Total %>% select("cd_a", "ent", "con",
                             "v_sel", "n_hog", "h_mud",
                             "n_ren", "n_pro_viv",
                             "Year", "p11_h2", "p11_m2", 
                             "p11_h7", "p11_m7" , "p11_h5", "p11_m5")


## household chores W
df <- SDE_Total %>% filter(e_con %in% c(1,5),
                           sex == 2,
                           eda %in% 25:65,
                           par_c %in% c(101,201))


# Remove pairs with 2 W
Aux <- df %>% group_by(cd_a, ent, con,
                       v_sel, n_hog, h_mud, Year) %>% 
  summarise(n = n()) %>% filter(n>1) 

X <- paste(df$cd_a, df$ent, df$con, df$v_sel,
           df$n_hog, df$h_mud, df$Year)
X2 <- paste(Aux$cd_a, Aux$ent, Aux$con, Aux$v_sel,
            Aux$n_hog, Aux$h_mud, Aux$Year)

df <- df[!X%in%X2,]

rm(X); rm(X2); rm(Aux)


# Join with the COE2
df <- left_join(df, df2 , by = c("cd_a", "ent", "con",
                                 "v_sel", "n_hog", "h_mud",
                                 "n_ren", "n_pro_viv",
                                 "Year"))


df$p11_h2 <- ifelse(is.na(df$p11_h2), 0, df$p11_h2)
df$p11_h5 <- ifelse(is.na(df$p11_h5), 0, df$p11_h5)
df$p11_h7 <- ifelse(is.na(df$p11_h7), 0, df$p11_h7)
df$p11_m2 <- ifelse(is.na(df$p11_m2), 0, df$p11_m2)
df$p11_m5 <- ifelse(is.na(df$p11_m5), 0, df$p11_m5)
df$p11_m7 <- ifelse(is.na(df$p11_m7), 0, df$p11_m7)


df <- df %>% filter(p11_h2 != 98, p11_h5 != 98, p11_h7 != 98)


df$p11_h2 <- ifelse(df$p11_h2 == 99 | is.na(df$p11_h2), 0, df$p11_h2)
df$p11_h5 <- ifelse(df$p11_h5 == 99 | is.na(df$p11_h5), 0, df$p11_h5)
df$p11_h7 <- ifelse(df$p11_h7 == 99 | is.na(df$p11_h7), 0, df$p11_h7)


df$hhChores_W <- ifelse(df$Year < 2013,  df$p11_h2*60 + df$p11_m2 +
                                         df$p11_h5*60 + df$p11_m5,0)
df$hhChores_W <- ifelse(df$Year >= 2013, df$p11_h2*60 + df$p11_m2 +
                                         df$p11_h7*60 + df$p11_m7, df$hhChores_W)


df %>% ggplot() + geom_histogram(mapping = aes(hhChores_W/60))



## household chores man

dfM <- SDE_Total %>% filter(e_con %in% c(1,5),
                            sex == 1,
                            par_c %in% c(101,201))


# Remove pairs with 2 M
Aux <- dfM %>% group_by(cd_a, ent, con,
                        v_sel, n_hog, h_mud, Year) %>% 
  summarise(n = n()) %>% filter(n>1) 

X <- paste(dfM$cd_a, dfM$ent, dfM$con, dfM$v_sel,
           dfM$n_hog, dfM$h_mud, dfM$Year)
X2 <- paste(Aux$cd_a, Aux$ent, Aux$con, Aux$v_sel,
            Aux$n_hog, Aux$h_mud, Aux$Year)

dfM <- dfM[!X%in%X2,]

rm(X); rm(X2); rm(Aux)


# Join with the COE2
dfM <- left_join(dfM, df2 , by = c("cd_a", "ent", "con",
                                   "v_sel", "n_hog", "h_mud",
                                   "n_ren", "n_pro_viv",
                                   "Year"))


dfM$p11_h2 <- ifelse(is.na(dfM$p11_h2), 0, dfM$p11_h2)
dfM$p11_h5 <- ifelse(is.na(dfM$p11_h5), 0, dfM$p11_h5)
dfM$p11_h7 <- ifelse(is.na(dfM$p11_h7), 0, dfM$p11_h7)
dfM$p11_m2 <- ifelse(is.na(dfM$p11_m2), 0, dfM$p11_m2)
dfM$p11_m5 <- ifelse(is.na(dfM$p11_m5), 0, dfM$p11_m5)
dfM$p11_m7 <- ifelse(is.na(dfM$p11_m7), 0, dfM$p11_m7)


dfM <- dfM %>% filter(p11_h2 != 98, p11_h5 != 98, p11_h7 != 98)


dfM$p11_h2 <- ifelse(dfM$p11_h2 == 99 | is.na(dfM$p11_h2), 0, dfM$p11_h2)
dfM$p11_h5 <- ifelse(dfM$p11_h5 == 99 | is.na(dfM$p11_h5), 0, dfM$p11_h5)
dfM$p11_h7 <- ifelse(dfM$p11_h7 == 99 | is.na(dfM$p11_h7), 0, dfM$p11_h7)


dfM$hhChores_M <- ifelse(dfM$Year < 2013,  dfM$p11_h2*60 + dfM$p11_m2 +
                           dfM$p11_h5*60 + dfM$p11_m5,0)
dfM$hhChores_M <- ifelse(dfM$Year >= 2013, dfM$p11_h2*60 + dfM$p11_m2 +
                           dfM$p11_h7*60 + dfM$p11_m7, dfM$hhChores_M)


dfM %>% ggplot() + geom_histogram(mapping = aes(hhChores_M/60))


dfM <- dfM %>% select(cd_a, ent, con, v_sel, n_hog, 
                      h_mud, n_ren, n_pro_viv, Year, hhChores_M)

#### Join- W + M

df <- df %>% left_join(dfM, by = c("cd_a", "ent", "con",
                                   "v_sel", "n_hog", "h_mud",
                                   "Year"))

df <- df[!is.na(df$hhChores_M),]

#ggplot(df, aes(hhChores_M, hhChores_M)) +geom_point()

df %>% filter(e_con == 1) %>%
  group_by(Year) %>% 
  summarise(Ratio = mean(hhChores_W/(hhChores_M + hhChores_W), na.rm = T))


df %>% filter(e_con == 5) %>%
  group_by(Year) %>% 
  summarise(Ratio = mean(hhChores_W/(hhChores_M + hhChores_W), na.rm = T))




df %>% 
  group_by(Year, e_con) %>% 
  summarise(Ratio = mean(hhChores_W/(hhChores_M + hhChores_W), na.rm = T)) %>%
  ggplot(mapping = aes(Year, Ratio, col = as.factor(e_con))) + geom_point() +
  geom_line() + theme_bw()


rm(df2); rm(dfM)

# 
# SDE_Total %>% filter(cd_a == 1, ent == 9, con == 40023,
#                      v_sel == 2, n_hog == 1, h_mud == 0) %>%
#   select(cd_a, ent, con, v_sel, n_hog, h_mud,Year,sex,
#          eda,par_c, n_hij, chld_less16) %>%
#   View()


################################################################################
# Q1-d

VivEni <- read_csv("Data/viviendas.csv")
ConcEni <- read_csv("Data/concentradohogar.csv")
PobEni <- read_csv("Data/poblacion.csv")
GasEni <- read_csv("Data/gastoshogar.csv")



# Get Home Ownership
VivEni <- VivEni %>% 
  filter(tenencia != 6) %>%
  mutate(homeownership = ifelse(tenencia %in% c(1,2,5), 0, 1))


# Get daycar, insurances, child care
GasEni <- GasEni %>% mutate(health_ins = ifelse(clave %in% c("J070", "J071", "J072"),
                                                gasto_tri, 0),
                            other_ins  = ifelse(clave %in% c("N008", "N009"),
                                               gasto_tri, 0),
                            child_care = ifelse(clave %in% c("E012"),
                                                gasto_tri, 0),
                            child_exp  = ifelse(!is.na(clave),
                                                as.integer(clave == "E012"), NA),
                            day_care = ifelse(clave %in% c("E008"),
                                                gasto_tri, 0),
                            day_exp  = ifelse(!is.na(clave),
                                                as.integer(clave == "E008"), NA)
                            )


GasH <- GasEni %>% group_by(folioviv, foliohog) %>%
  summarise(health_ins = sum(health_ins, na.rm = T),
            other_ins  = sum(other_ins, na.rm = T),
            child_care = sum(child_care, na.rm = T),
            child_exp  = sum(child_exp, na.rm = T),
            day_care   = sum(day_care, na.rm = T),
            day_exp    = sum(day_exp, na.rm = T))


PobH <- PobEni %>% group_by(folioviv, foliohog) %>%
  summarise(age_hhead = edad[which(parentesco == 101)],
            age_hspouse = first(edad[parentesco %in% c(201,202)]),
            married = edo_conyug[which(parentesco == 101)])




ConcEni <- ConcEni %>% left_join(VivEni, by = c("folioviv"))

ConcEni <- ConcEni %>% left_join(GasH, by = c("folioviv",
                                              "foliohog"))

ConcEni <- ConcEni %>% left_join(PobH, by = c("folioviv",
                                              "foliohog"))


ConcEni <- ConcEni %>% 
  filter(clase_hog == 2)


ConcEni <- ConcEni %>% mutate(Income = ing_cor - ingtrab,
                              Food =  ali_dentro + ali_fuera,
                              Transportation = transporte,
                              HealthService = salud,
                              Utilities = pred_cons + agua + energia,
                              Education = educacion,
                              HouseKeeping = cuidados,
                              Rent = ifelse(homeownership == 0, alquiler, estim_alqu),
                              Insurance = health_ins + other_ins,
                              Health_ins = health_ins,
                              Home_ins = other_ins,
                              Childc = day_care + child_care)



vars_to_summarise <- c("Income", "Food", "Transportation", "HealthService", "Utilities", 
                       "Education", "HouseKeeping", "Rent", "Insurance", "Health_ins", 
                       "Home_ins", "Childc")



tabela_resumo <- ConcEni %>%
  filter(married %in% c(1, 2)) %>%
  group_by(married) %>%
  summarise(
    across(all_of(vars_to_summarise),
           list(Mean = ~mean(.x, na.rm = TRUE),
                Median = ~median(.x, na.rm = TRUE)))
  )





