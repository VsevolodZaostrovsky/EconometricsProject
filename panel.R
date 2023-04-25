# install.packages("plm")
# install.packages("clubSandwich")

library(plm) #panel data linear models
library(clubSandwich) #robust standard errors
# library(ggplot2)

dd <- read.csv("~/Downloads/proj_ecometrica/global_data.csv", sep = ";")

dd <- na.omit(dd)


dd_panel <- pdata.frame(dd, index = c("COUNTRY", "YEAR"))

table(dd_panel$YEAR)


#### ESTIMATION


# RE model
m1 <- plm(Happiness.Score ~ I(log(vvp_pps_na_dyshy.xls)) + I(sqrt(bezrabotiza.xls)) + nalogi.xls + 
            rashodi_na_med.xls + chislo_ubistv.xls + 
            inflazia.xls + 
            trati_na_obraz.xls,
          data = dd_panel,
          effect = "individual", 
          model = "random")
summary(m1, vcov = vcovHC(m1, cluster = "group"))

# FE model
m2 <- plm(Happiness.Score ~ I(log(vvp_pps_na_dyshy.xls)) + I(sqrt(bezrabotiza.xls)) + nalogi.xls + 
            rashodi_na_med.xls + chislo_ubistv.xls + 
            inflazia.xls + 
            trati_na_obraz.xls,
          data = dd_panel,
          effect = "individual", 
          model = "within")
summary(m2, vcov = vcovHC)


phtest(m2, m1) # Hausman test
# P-value is low, hence models are different, hence we take FE


# FE model
mMSK <- plm(WoSPapersPer100Faculty ~ averageScoreBudget + foreignFacultyShare + 
            facultyWageToRegionalAverage + studentsBAShare + 
            profileHard,
          data = dd_panel[dd$region_map %in% c("Moscow", "CitySPB"), ],
          effect = "twoways", 
          model = "within")
summary(mMSK, vcov = vcovHC)

mNoMSK <- plm(WoSPapersPer100Faculty ~ averageScoreBudget + foreignFacultyShare + 
              facultyWageToRegionalAverage + studentsBAShare + 
              profileHard,
            data = dd_panel[!dd$region_map %in% c("Moscow", "CitySPB"), ],
            effect = "twoways", 
            model = "within")
summary(mNoMSK, vcov = vcovHC)



# RE model
mMSK <- plm(WoSPapersPer100Faculty ~ lag(averageScoreBudget, 1) + foreignFacultyShare + 
              facultyWageToRegionalAverage + studentsBAShare + 
              profileHard + top5_100,
            data = dd_panel[dd$region_map %in% c("Moscow", "CitySPB"), ],
            effect = "individual", 
            model = "random")
summary(mMSK, vcov = vcovHC)

mNoMSK <- plm(WoSPapersPer100Faculty ~ averageScoreBudget + foreignFacultyShare + 
                facultyWageToRegionalAverage + studentsBAShare + 
                profileHard + top5_100,
              data = dd_panel[!dd$region_map %in% c("Moscow", "CitySPB"), ],
              effect = "twoways", 
              model = "random")
summary(mNoMSK, vcov = vcovHC)



# Breusch-Pagan test

# Pooled model
m2 <- plm(WoSPapersPer100Faculty ~ averageScoreBudget + foreignFacultyShare + 
            facultyWageToRegionalAverage + studentsBAShare + 
            profileHard,
          data = dd_panel,
          model = "pooling")

# Breusch-Pagan test for random individual effect
plmtest(m2, type = "bp", effect = "individual")








