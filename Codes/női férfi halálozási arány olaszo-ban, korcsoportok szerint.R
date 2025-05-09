sex_cols <- c(Férfi = "#F4BA3B", Nő =  "#730B6D")

italy_rates <-tibble(
  age_grp = rep(c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+'), 2),
  sex = rep(c("Férfi", "Nő"), each = 10),
  cfr = c(0.1, 0, 0.1, 0.5, 1.5,   4.2, 12.4, 29.5, 40.5, 40.4,
          0.1, 0, 0, 0.2,   0.4, 1, 5.7,  16.7, 20.7,   20.3) / 100,
  age_midpt = rep(c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95), 2)
)



italy_rates %>%
  ggplot(aes(x = age_midpt, y = cfr, colour = sex)) +
  geom_point() +
  geom_text(data = filter(italy_rates, cfr > 0.009),
            aes(label = percent(cfr), y = cfr + 0.012), size = 3) +
  geom_line() +
  scale_x_continuous(breaks = italy_rates$age_midpt, labels = italy_rates$age_grp) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_colour_manual(values = sex_cols) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Korcsoport", colour = "", y = "Megfigyelt halálozási ráta",
       title = "Megfigyelt halálozási ráta az olasz koronavírusos betegek körében, 2020. április 28.",
       subtitle = "94.174 férfi 16,6%-os halálozási aránnyal; 104.861 nő 9,1%-os halálozási aránnyal",
       caption = "Forrás: Istituto Superiore di Sanità, Roma")+
  scale_color_manual(values=c('#000000','#990000'))

"Vége"





pop_2020 <- popM %>%
  mutate(sex = "Férfi") %>%
  rbind(mutate(popF, sex = "Nő")) %>%
  select(country = name, age, pop = `2020`, sex) %>%
  left_join(age_lu, by = "age") %>%
  group_by(country, age_grp, sex) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  filter(country %in% selected_countries) %>%
  mutate(country = fct_drop(country)) %>%
  group_by(country) %>%
  mutate(prop = pop / sum(pop)) %>%
  ungroup()

# check no misspellings in countries
# stopifnot(sum(!selected_countries %in% unique(pop_2020$country)) == 0)

pop_2020 %>%
  ggplot(aes(x = as.numeric(age_grp), y = prop, colour = sex)) +
  geom_line() +
  geom_point()+
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 1:10, labels = levels(pop_2020$age_grp)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Korcsoport",
       y = "",
       colour = "",
       title = "Az olasz népesség becsült koreloszlása 2020-ban",
       subtitle = "Korcsoport és nem szerint",
       caption = "Forrás: UN World Population Prospects 2019")+
  scale_color_manual(values=c('#000000','#990000'))

"Ennek is"


#######################################################################################
par(mfrow=c(2, 3), mai=c(0,0,0,0))
plotit(wr, "adjecency")
plotit(wr2, lab='lag-2 adj.')
plotit(wd10, lab="10 km")
plotit(k1, '25 km')
plotit(k4, 'k=3')
plotit(k8, 'k=6')

##################################################

par(mfrow=c(2, 3), mai=c(0,0,0,0))
plotit(wr, "adjecency")
plotit(wr2, lab='lag-2 adj.')
plotit(wd4, lab="4 km")
plotit(k1, 'k=1')
plotit(k4, 'k=4')
plotit(k8, 'k=8')


## moranplotok

par(mfrow=c(2, 3), mai=c(0,0,0,0))
moran.plot(log(vegleges_nagydata_terkeppel@data$deceduti), szomszedlista_olaszo_wr, labels = F,zero.policy = T, xlab = "Halottak számának logaritmusa" , ylab = "Térbeli késleltetett")
moran.plot(log(vegleges_nagydata_terkeppel@data$deceduti), szomszedlista_olaszo_wr2, labels = F, xlab = "Halottak számának logaritmusa" , ylab = "Térbeli késleltetett")
moran.plot(log(vegleges_nagydata_terkeppel@data$deceduti), szomszedlista_olaszo_wd4, labels = F,xlab = "Halottak számának logaritmusa" , ylab = "Térbeli késleltetett")
moran.plot(log(vegleges_nagydata_terkeppel@data$deceduti), szomszedlista_olaszo_k1, labels = F, xlab = "Halottak számának logaritmusa" , ylab = "Térbeli késleltetett")
moran.plot(log(vegleges_nagydata_terkeppel@data$deceduti), szomszedlista_olaszo_k4,labels = F, xlab = "Halottak számának logaritmusa" , ylab = "Térbeli késleltetett")
moran.plot(log(vegleges_nagydata_terkeppel@data$deceduti), szomszedlista_olaszo_k8, labels = F, xlab = "Halottak számának logaritmusa" , ylab = "Térbeli késleltetett")



#######################################################################################
first_non_china_d <- coronavirus %>%
  filter(Country.Region != "China" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

first_italy_d <- coronavirus %>%
  filter(Country.Region == "Italy" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

first_usa_d <- coronavirus %>%
  filter(Country.Region == "US" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

first_uk_d <- coronavirus %>%
  filter(Country.Region == "United Kingdom" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

first_uk_d
###------------------------------###



d1 <- coronavirus %>%
  group_by(date, type) %>%
  summarise(cases = sum(cases)) %>%
  arrange(date) %>%
  spread(type, cases) %>%
  ungroup() %>%
  mutate(cfr_today = death / confirmed,
         cfr_cumulative = cumsum(death) / cumsum(confirmed))

d1b <- d1 %>%
  filter(date %in% c(first_italy_d, first_non_china_d, first_usa_d, first_uk_d))
ac <- "#990000"

d1c <- d1 %>%
  mutate(cc = cumsum(confirmed)) %>% 
  summarise(`10000  ` = min(date[cc > 10000]),
            `100000 ` = min(date[cc > 100000]),
            `1000000` = min(date[cc > 1000000])) %>%
  gather(variable, date) %>%
  left_join(d1, by = "date") %>%
  mutate(label = paste0(format(as.numeric(variable), big.mark = ",", scientific = FALSE), "\nfertőzött"))

d1_millio <- d1 %>%
  mutate(cc = cumsum(confirmed)) %>% 
  summarise(`10000` = min(date[cc > 10000]),`1000000` = min(date[cc > 1000000])) %>%
  gather(variable, date) %>%
  left_join(d1, by = "date") %>%
  mutate(label = paste0(format(as.numeric(variable), big.mark = ",", scientific = FALSE), "\nfertőzött"))

d1 %>%
  ggplot(aes(x = date, y = cfr_cumulative)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 0.1)) +
  expand_limits(y = 0) +
  geom_point(data = d1b, colour = ac, shape = 18, size = 3) +
  geom_point(data = d1_millio, colour = "#663333", shape = 18, size = 3) +
  annotate("text", x = first_italy_d, 
           y = filter(d1, date == first_italy_d)$cfr_cumulative - 0.001, 
           label = "Az első haláleset Olaszországban",
           hjust = 0, vjust = 1,size = 3, colour = ac) +
  annotate("text", x = first_uk_d, 
           y = filter(d1, date == first_uk_d)$cfr_cumulative - 0.001, 
           label = "Az első haláleset az Egyesült Királyságban",
           hjust = -0, vjust = 1, size = 3, colour = ac) +
  annotate("text", x = first_usa_d, 
           y = filter(d1, date == first_usa_d)$cfr_cumulative - 0.001, 
           label = "Az első haláleset az USA-ban",
           hjust = 1, vjust = -1.5, size = 3, colour = ac) +
  annotate("text", x = first_non_china_d, 
           y = filter(d1, date == first_non_china_d)$cfr_cumulative + 0.001, 
           label = "Az első haláleset Kínán kívül",
           hjust = 0, vjust = 2,size = 3, colour = ac) +
  geom_text(data = d1c, aes(label = label), 
            size = 3, colour = "#663333", vjust = -1,
            hjust = 0, lineheight = 0.9, nudge_y = -0.002) +
  labs(caption = the_caption,
       x = "",
       y = "Megfigyelt halálozási ráta",
       title = "A COVID-19 halálozási rátájának emelkedése",
       subtitle = "2020. májusáig")

################################################################################xx

qplot(vegleges_nagydata_terkeppel@data$deceduti)
qplot(log(vegleges_nagydata_terkeppel@data$deceduti))
hist(vegleges_nagydata_terkeppel@data$deceduti)
hist(log(vegleges_nagydata_terkeppel@data$deceduti))

#########################x

hist(vegleges_nagydata_terkeppel@data$deceduti,breaks = 8, 
     xlab = "Halálozás", ylab = "", main = "Halálozás eloszlása")
hist(log(vegleges_nagydata_terkeppel@data$deceduti), prob=T, breaks = 8, 
     xlab = "Halálozás logaritmusána", ylab = "", main = "Halálozás logaritmusának eloszlása")

#########################x

g = vegleges_nagydata_terkeppel@data$deceduti
m<-mean(g)
std<-sqrt(var(g))


hist(vegleges_nagydata_terkeppel@data$deceduti,breaks = 8, 
     xlab = "Halálozás", ylab = "", main = "Halálozás eloszlása")
curve(dnorm(x, mean=m, sd=std), 
      col="#990000", lwd=2, add=TRUE, yaxt="n")

g = log(vegleges_nagydata_terkeppel@data$deceduti)
m<-mean(g)
std<-sqrt(var(g))

hist(log(vegleges_nagydata_terkeppel@data$deceduti), prob=T, breaks = 8, 
     xlab = "Halálozás logaritmusána", ylab = "", main = "Halálozás logaritmusának eloszlása")
curve(dnorm(x, mean=m, sd=std), 
      col="#990000", lwd=2, add=TRUE, yaxt="n")

#########################x

myhist <- hist(vegleges_nagydata_terkeppel@data$deceduti,breaks = 8, 
               xlab = "Halálozás", ylab = "", main = "Halálozás eloszlása")
multiplier <- myhist$counts / myhist$density
mydensity <- density(vegleges_nagydata_terkeppel@data$deceduti)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)

####################################################################################
?stargazer
#############


stargazer(ols_kezdo_1,ols_kezdo_2,ols_kezdo_3,ols_elhizas,ols_elhizas_0,ols_elhizas_1,
          style = "qje",
          title="Regressziós eredmények", type="text",out="4regresszio.htm",
          df=FALSE, digits=5)

stargazer(ols_kezdo_1,ols_kezdo_2,ols_kezdo_3,ols_elhizas,ols_elhizas_0,ols_elhizas_1,
          style = "qje",
          title="Regressziós eredmények", type="html",out="4regresszio.htm",
          df=FALSE, digits=5)





##################################################################################



hist(szukebb$egy_lakosra_juto_napi_cigi)

g = szukebb$egy_lakosra_juto_napi_cigi
m<-mean(g)
std<-sqrt(var(g))

hist(g, xlab = "Egy lakosra jutó naponta elszívott cigaretta", ylab = "", main = "íEgy lakosra jutó naponta elszívott cigaretta eloszlása")
curve(dnorm(x, mean=m, sd=std), 
      col="#990000", lwd=2, add=TRUE, yaxt="n")






