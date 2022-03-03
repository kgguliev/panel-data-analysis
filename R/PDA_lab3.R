### February 10, 2022
### Panel data: Analysis and Applications for the Social Sciences
### Lab 3. Interaction terms

# DOWER, P., FINKEL, E., GEHLBACH, S., & NAFZIGER, S. (2018). 
# Collective Action and Representation in Autocracies: Evidence from Russia’s Great Reforms. 

# DATA SOURCE
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/X7AMNO 

# STATEMENT
# The data is used for the training purposes only. For the further use, please cite the original paper. 
######################################################
install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("margins")
install.packages("psych")
install.packages("memisc")

library(haven) 
library(dplyr)
library(ggplot2)
library(margins)
library(psych)
library(memisc)

# Use data "PDA_lab3.dta"
df <- read_dta("./data/PDA_lab3.dta")

head(df)
describe(df)

### Data description
## ch_schools_pc - Change in rural schools, 1860 to 1880 (per capita)
## afreq - frequency of peasant unrest from 1851 to 1863
## nozemstvo - a dummy-variable, 1 stands for those districts (uezd) that did NOT receive zemstva 
## distance_moscow - distance from Moscow
## goodsoil - soil fertility index (constructed by the authors from data on soil type from the Food and Agriculture Organization)
## lnurban - urban population (log), 1863
## lnpopn - total population (log), 1863 
## province_capital - a dummy-variable, 1 stands for provincial capitals, 0 - otherwise

### Описание данных
## ch_schools_pc - Изменение в количестве сельских школ с 1860 до 1880 гг. на душу сельского населения уезда
## afreq - Доля лет между 1851 и 1863 гг., в которые были зафиксированы крестьянские выступления
## nozemstvo - Единицей закодированы уезды тех губерний, в которых в результате реформы 1864 года земства созданы не были
## distance_moscow - Расстояние от Москвы до центра уезда
## goodsoil - Показатель плодородности почвы, рассчитанный авторами на основании данных Продовольственной и сельскохозяйственной организации ООН
## lnurban - Логарифм городского населения уезда на 1863 г.
## lnpopn - Логарифм населения уезда на 1863 г.
## province_capital - Единицей закодированы уезды, в которых находился столичный город губернии

# For ease of interpretation, recode the "zemstvo" variable
df$zemstvo <- dplyr::recode(df$nozemstvo, "1" = "0", "0" = "1")

# Create the supplementary dataset
plot_data <- df %>% dplyr::select(masterid, afreq, ch_schools_pc, zemstvo) %>% na.omit() 
plot_data <- mutate(plot_data, zemstvo = factor(zemstvo))

# Differences between zemstvo regions and non-zemstvo regions 
ggplot(data = plot_data, aes(x = afreq, y = ch_schools_pc, color = zemstvo)) +
  geom_point(size=2) + labs(x = "Frequency of unrest", 
                            y = "Change in rural schools, \n 1860 to 1880 (per capita)",
                            title = "The effect of peasant collective action on redistribution") + 
  geom_smooth(method=lm)+ scale_colour_manual(values = c("coral3", "darkorchid3"))

# Regression models
# Paper replication
m1 <- lm(ch_schools_pc ~ afreq + zemstvo, data = df)
summary(m1)

m2 <- lm(ch_schools_pc ~ afreq + zemstvo + afreq:zemstvo, data = df)
summary(m2)

# compare the separate-sample and interaction approaches
interactions1 <- subset(df, zemstvo == 1)
m2_1 <- lm(ch_schools_pc ~ afreq, data = interactions1)
summary(m2_1)
interactions0 <- subset(df, zemstvo == 0)
m2_2 <- lm(ch_schools_pc ~ afreq, data = interactions0)
summary(m2_2)

mtable(m2, m2_1, m2_2)

# marginal effects
margins_m2 <- margins(m2)
summary(margins_m2)
plot(margins_m2)

# marginal effects at specified values
s <- margins(m2, at = list(zemstvo = c("0", "1")))
summary(s)

# control variables included
m3 <- lm(ch_schools_pc ~ afreq + zemstvo + afreq:zemstvo + distance_moscow + goodsoil + lnurban + lnpopn + province_capital, data = df)
summary(m3)
mtable(m2, m3)

# interaction between continuous variables
m4 <- lm(ch_schools_pc ~ afreq + zemstvo + distance_moscow + goodsoil + lnurban + lnpopn + province_capital +lnurban:goodsoil, data = df)
summary(m4)
margins_m4 <- margins(m4)
summary(margins_m4)
s2 <- margins(m4, at = list(lnurban = 0:13))
summary(s2)

ggplot(s2, aes(x = lnurban)) + 
  geom_line(aes(y = dydx_goodsoil)) +
  geom_line(aes(y = dydx_goodsoil+1.96*sqrt(Var_dydx_goodsoil)), linetype = 2) +
  geom_line(aes(y = dydx_goodsoil-1.96*sqrt(Var_dydx_goodsoil)), linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Conditional effect") +
  xlab("lnurban") + ylab("Marginal effect of goodsoil")

