library("tidyverse")
library("ggplot2")
#install.packages("ggpubr",dependencies = TRUE, INSTALL_opts = '--no-lock')
#options("install.lock"=FALSE)
library("lfe")
library("survival")
library("survminer")
library("rms")
library("ggpubr")
library("stargazer")
library(dplyr)
########################
#REPLICATING RESULTS
########################
#loading data
load(file = "HansLueders_ElectoralResponsiveness_APSR_replicationdata.RData")

#Creating graph not in paper, but I find useful for context:
PCpetitions %>% 
  count(topic_edited_en) %>% 
  top_n(10, n) %>% 
  inner_join(PCpetitions, by = "topic_edited_en") %>% 
  ggplot(aes(x = topic_edited_en)) +
  geom_bar()

#replicating tables
#replicating table 3 results
pet3 <- filter(PCpetitions, PCpetitions$beforeafterALL_2 >= -90 & PCpetitions$beforeafterALL_2 <= 90)
cm3 <- filter(CMpetitions, CMpetitions$beforeafter >= -90 & CMpetitions$beforeafter <= 90)

m1 <- felm(log(timediffTOPCODED + 1) ~ preALL_2*electionALL + n_net | GDRCountyID + completion.year + dayofyear| 0 | GDRCountyID, data=pet3)
m2 <- felm(log(timediffTOPCODED + 1) ~ preALL_2*electionALL + n_net  | zip + completion.year + dayofyear | 0 | zip, data=pet3)
m3 <- felm(log(timediffTOPCODED + 1) ~ preALL_2*electionALL + n_net + as.factor(GDRCountyID)*as.numeric(completion.year) | 
             GDRCountyID + completion.year + dayofyear | 0 | GDRCountyID, data=pet3)
m4 <- felm(log(timediffTOPCODED + 1) ~ preALL_2*electionALL + n_net | GDRCountyID + completion.year + dayofyear | 0 | GDRCountyID + monthyear, data=pet3)
m5 <- felm(log(timediffTOPCODED + 1) ~ pre1989 + n_net  | GDRCountyID | 0 | GDRCountyID, data=cm3)
m6 <- felm(log(timediffTOPCODED + 1) ~ pre1989 + n_net  | zip | 0 | zip, data=cm3)

stargazer(m1, m2, m3, m4, m5, m6,
          omit.stat = c("rsq", "ser", "f"),
          omit = c("GDRCountyID", "completion.year"),
          df = T,
          notes.align="c",
          no.space=T,
          model.names=F,
          dep.var.labels.include = T,
          column.labels = c(rep("PC",4), rep("CM",2)),
          add.lines = list(
            c("Day-FE?", "yes", "yes", "yes", "yes", "", ""),
            c("Year-FE?", "yes", "yes", "yes", "yes", "", ""),
            c("County-FE?", "yes", "", "yes", "yes", "yes", ""),
            c("zip code-FE?", "", "yes", "", "", "", "yes"),
            c("County x year?", "", "", "yes", "", "", ""),
            c("SE clustered by", "county", "zip code", "county", "county and month-year", "county", "zip code")))

print("Response days")
print("People's Chamber")
print("response*election + pending petitions, county+ day + year dummy variables")
summary(m1)
print("response*election + pending petitions, zip + day + year dummy variables")
summary(m2)
print("response*election + pending petitions + county*year, county + year + day dummy variables" )
#summary(m3)
m3$coefficients
print("response*election + pending petitions + county*year, county + month dummy variables")
summary(m4)
print("Council of Ministers")
print("before election + pending petitions, county dummy variable")
summary(m5)
print("before election + pending petitions, zip dummy variable")
summary(m6)


#replicating table 4 results
pc4 <- filter(PCpetitions, PCpetitions$beforeafterALL_2 >= -90 & PCpetitions$beforeafterALL_2 <= 90 & PCpetitions$completion.year <= 1982)
n1 <- felm(positiveResolution ~ preALL_2*electionALL + n_net | GDRCountyID + completion.year + dayofyear | 0 | GDRCountyID, data=pc4)
n2 <- felm(positiveResolution ~ preALL_2*electionALL + n_net | zip + completion.year + dayofyear | 0 | zip, data=pc4)
n3 <- felm(positiveResolution ~ preALL_2*electionALL + n_net + as.factor(GDRCountyID)*as.numeric(completion.year) | 
             GDRCountyID + completion.year + dayofyear | 0 | GDRCountyID, data=pc4)
n4 <- felm(positiveResolution ~ preALL_2*electionALL + n_net  | GDRCountyID + completion.year + dayofyear | 0 | GDRCountyID + monthyear, data=pc4)
n5 <- felm(positiveResolution ~ preALL_2*electionALL + n_net | GDRCountyID + completion.year + dayofyear | 0 | GDRCountyID, data=PCpetitions, 
           subset=PCpetitions$beforeafterALL_2 >= -90 & PCpetitions$beforeafterALL_2 <= 90 & PCpetitions$completion.year <= 1982 & 
             (PCpetitions$topic_edited_en == "Housing" | PCpetitions$topic_edited_en == "Construction"))
n6 <- felm(positiveResolution ~ preALL_2*electionALL + n_net | GDRCountyID + completion.year + dayofyear | 0 | GDRCountyID, data=PCpetitions, 
           subset=PCpetitions$beforeafterALL_2 >= -90 & PCpetitions$beforeafterALL_2 <= 90 & PCpetitions$completion.year <= 1982 & 
             (PCpetitions$topic_edited_en != "Housing" & PCpetitions$topic_edited_en != "Construction"))

stargazer(n1, n2, n3, n4, n5, n6, 
          omit.stat = c("rsq", "ser", "f"),
          omit = c("GDRCountyID", "completion.year"),          
          notes.align="c",
          no.space=T,
          model.names=F,
          dep.var.labels.include = T,
          dep.var.labels  = c("1(positive resolution)"),
          column.labels = c(rep("all petitions",4), "housing", "non-housing"),
          add.lines = list(
            c("Day-FE?", "yes", "yes", "yes", "yes", "yes", "yes"),
            c("Year-FE?", "yes", "yes", "yes", "yes", "yes", "yes"),
            c("County-FE?", "yes", "", "yes", "yes", "yes", "yes"),
            c("zip code-FE?", "", "yes", "", "", "", ""),
            c("County x year?", "", "", "yes", "", "", ""),
            c("SE clustered by", "county", "zip code", "county", "county and month-year", "county", "county")))


print("Success Rate")
print("People's Chamber")
print("response*election + pending petitions, county+ day + year dummy variables")
summary(n1)
print("response*election + pending petitions, zip + day + year dummy variables")
summary(n2)
print("response*election + pending petitions + county*year, county + year + day dummy variables" )
#summary(m3)
n3$coefficients
print("response*election + pending petitions + county*year, county + month dummy variables")
summary(n4)
print("Council of Ministers")
print("before election + pending petitions, county dummy variable")
summary(n5)
print("before election + pending petitions, zip dummy variable")
summary(n6)


#replicating table 5 results
cm5 = filter(CMpetitions, CMpetitions$beforeafter >= -90 & CMpetitions$beforeafter <= 90)
p1 <- felm(log(timediffTOPCODED + 1) ~ pre1989*character_criticism + n_net  | GDRCountyID | 0 | GDRCountyID, data=cm5)
p2 <- felm(log(timediffTOPCODED + 1) ~ pre1989*response_centralgov + n_net | GDRCountyID  | 0 | GDRCountyID, data=cm5)
p3 <- felm(response_centralgov ~ pre1989 + n_net | GDRCountyID | 0 | GDRCountyID, data=cm5)
p4 <- felm(response_centralgov ~ pre1989*character_criticism + n_net | GDRCountyID | 0 | GDRCountyID, data=cm5)

print("Critical of Government")
print("response time")
print("before election*is criticism + response number, county dummy veraiable")
summary(p1)
print("before election + response number, county dummy veraiable")
summary(p2)
print("response success")
print("before election*is criticism + response number, county dummy veraiable")
summary(p3)
print("before election*is criticism + response number, county dummy veraiable")
summary(p4)

stargazer(p1, p2, p3, p4,
          omit.stat = c("rsq", "ser", "f"),
          notes.align="c",
          no.space=T,
          model.names=F,
          dep.var.labels.include = T,
          dep.var.labels  = c("Response time (log days)", "1(Central government response)"),
          add.lines = list(
            c("County-FE?", "yes", "yes", "yes", "yes")))




#replicating the important graph
#People's chamber graph
# Fit Kaplan-Meier survival curve
survPC <- survfit(Surv(timediffTOPCODED) ~ I(preALL_1 == 0), type="kaplan-meier", 
                  data=PCpetitions[PCpetitions$beforeafterALL_1>=-90 & PCpetitions$beforeafterALL_1 <= 90,])

# Creating the plot (need to use the rms package since survminer doesn't work on current version of R(or at least I couldnt get it to work)
plot(survPC, xlab = "Days", ylab = "Probability of No Response", main = "Probability of No Response Before and After an Election for the People's Chamber", col = c("green", "red"), xlim = c(0,90))
legend("bottomleft", legend = c("Before Election", "After Election"),  col = c("green", "red"), lwd = 2)
ggsave("Graph1.png", width = 6, height = 4, units = 'in', dpi = 300)

#council of ministers graph
survPC <- survfit(Surv(timediffTOPCODED) ~ I(pre1989 == 0), type="kaplan-meier", 
                  data=CMpetitions[CMpetitions$beforeafter>=-90 & CMpetitions$beforeafter <= 90,])

# Creating the plot (need to use the rms package since survminer doesn't work on current version of R(or at least I couldnt get it to work)
plot(survPC, xlab = "Days", ylab = "Probability of No Response", main = "Probability of No Response Before and After an Election for the Council of Ministers", col = c("green", "red"), xlim = c(0,90))
legend("bottomleft", legend = c("Before Election", "After Election"),  col = c("green", "red"), lwd = 2)
ggsave("Graph2.png", width = 6, height = 4, units = 'in', dpi = 300)

##########################
#ORIGINAL CONTRIBUTION
##########################
#Two new column are added to the dataframe, they are:
#bezirk <- this contains the name of the bezirk (equivalent of bundesland in East Germany) from which each petition is sent
#urban <- is a dummy vector where 1 denotes an urban bezirk from which the petition is sent and 0 denotes a rural bezirk




#The next few vectors are used to create columns which classify the area to each bezirk and if it is urban or rural

#regional vectors
cottbus <- c("Cottbus-Stadt","Cottbus","Bad Liebenwerda","Calau","Cottbus-Land","Finsterwalde","Forst","Guben" ,"Wokrejs Gubin",
             "Herzberg","Hoyerswerda","Jessen","Luckau","Lübben","Senftenberg","Spremberg","Weißwasser")

dresden <- c("Dresden-Stadt","Dresden","Görlitz","Bautzen","Bischofswerda","Dippoldiswalde","Dresden-Land","Freital","Görlitz-Land","Görlitz-Stadt",
             "Großenhain","Kamenz","Löbau","Meißen","Niesky","Pirna","Riesa","Sebnitz","Zittau")

erfurt <- c("Erfurt-Stadt","Erfurt","Weimar","Weimar-Stadt","Apolda","Arnstadt","Eisenach","Erfurt-Land","Gotha","Heiligenstadt","Langensalza","Mühlhausen",
            "Nordhausen","Worbis","Sömmerda","Sondershausen","Weimar-Land")

frankfurt_oder <- c("Frankfurt (Oder)","Frankfurt","Frankfurt an der Oder", "Frankfurt-Stadt","Eisenhüttenstadt","Eisenhüttenstadt-Stadt","Schwedt/Oder","Schwedt","Angermünde","Bad Freienwalde",
                    "Beeskow","Bernau","Eberswalde","Eisenhüttenstadt-Land","Fürstenwalde","Seelow","Strausberg")

gera <- c("Gera","Gera-Stadt","Jena","Jena-Stadt","Jena-Land","Eisenberg","Gera-Land","Greiz","Jena","Lobenstein","Pößneck","Rudolstadt","Saalfeld","Schleiz",
          "Stadtroda","Zeulenroda")

halle <- c("Halle","Halle-Stadt","Dessau","Dessau-Stadt","Halle-Neustadt","Halle-Neustadt-Stadt","Artern","Aschersleben","Bernburg","Bitterfeld","Eisleben","Gräfenhainichen",
           "Hettstedt","Hohenmölsen","Köthen", "Merseburg","Naumburg","Nebra","Quedlinburg","Querfurt","Roßlau",
           "Saalkreis","Sangerhausen","Weißenfels","Wittenberg","Zeitz")

chemnitz <- c("Karl-Marx-Stadt","Karl-Marx-Stadt-Stadt","Plauen","Plauen-Stadt","Zwickau","Zwickau-Stadt","Johanngeorgenstadt","Schneeberg","Annaberg","Aue","Auerbach",
             "Brand-Erbisdorf","Flöha","Freiberg","Glauchau","Hainichen",
             "Hohenstein-Ernstthal","Karl-Marx-Stadt-Land","Klingenthal","Marienberg","Oelsnitz","Plauen-Land",
             "Reichenbach","Rochlitz","Schwarzenberg","Stollberg","Werdau","Zschopau","Zwickau-Land")

leipzig <- c("Leipzig-Stadt","Leipzig","Altenburg","Borna","Delitzsch","Döbeln","Eilenburg","Geithain","Grimma","Leipzig-Land",
             "Oschatz","Schmölln","Torgau","Wurzen")


magdeburg <- c("Magdeburg","Magdeburg-Stadt","Burg","Gardelegen","Genthin","Halberstadt","Haldensleben","Havelberg","Kalbe","Kalbe (Milde)",
               "Milde","Klötze","Loburg","Oschersleben","Osterburg","Salzwedel","Schönebeck","Seehausen","Staßfurt",
               "Stendal","Stassfurt","Tangerhütte","Wanzleben","Wernigerode","Wolmirstedt","Zerbst")

neubrandenburg <- c("Neubrandenburg","Neubrandenburg-Stadt","Altentreptow","Anklam","Demmin","Malchin","Neubrandenburg-Land","Neustrelitz",
                    "Pasewalk","Prenzlau","Röbel/Müritz","Röbel","Müritz","Strasburg","Templin","Teterow","Ueckermünde",
                    "Waren")

potsdam <- c("Potsdam","Potsdam-Stadt","Brandenburg an der Havel","Belzig","Brandenburg","Brandenburg-Stadt","Brandenburg-Land","Gransee","Jüterbog","Königs-Wusterhausen",
             "Kyritz","Luckenwalde","Nauen","Neuruppin","Oranienburg","Potsdam-Land","Pritzwalk","Rathenow","Wittstock","Zossen")

rostock <- c("Rostock","Greifswald","Greifswald-Stadt","Greifswald-Land","Stralsund","Stralsund-Land","Stralsund-Stadt","Wismar","Bad Doberan","Greifswald Land","Grevesmühlen","Grimmen",
             "Ribnitz-Damgarten","Rostock-Land","Rügen","Stralsund","Wismar-Land","Wismar-Stadt","Wolgast","Rostock-Stadt")

schwerin <- c("Schwerin","Schwerin-Stadt","Bützow","Gadebusch","Güstrow","Hagenow","Ludwigslust","Lübz","Parchim","Perleberg",
              "Schwerin-Land","Sternberg")

Suhl	<- c("Suhl","Suhl-Stadt","Bad Salzungen","Hildburghausen","Ilmenau","Meiningen","Neuhaus","Neuhaus am Rennweg","Schmalkalden","Sonneberg","Suhl-Land")

berlin <- c("Friedrichshain","Hellersdorf","Hohenschönhausen","Köpenick","Lichtenberg","Marzahn","Mitte","Pankow",
            "Prenzlauer Berg","Treptow","Weißensee","Berlin Ost")

urban <- c("Cottbus","Cottbus-Stadt","Dresden","Dresden-Stadt","Görlitz","Erfurt","Erfurt-Stadt","Weimar","Frankfurt (Oder)","Frankfurt","Frankfurt-Stadt","Eisenhüttenstadt","Eisenhüttenstadt-Land",
           "Schwedt/Oder","Schwedt","Gera","Gera-Stadt","Jena","Jena-Stadt","Halle","Halle-Stadt","Dessau","Dessau-Stadt","Halle-Neustadt","Karl-Marx-Stadt","Karl-Marx-Stadt-Stadt","Plauen","Zwickau",
           "Johanngeorgenstadt","Schneeberg","Leipzig","Leipzig-Stadt","Magdeburg","Magdeburg-Stadt","Neubrandenburg","Neubrandenburg-Stadt","Potsdam","Potsdam-Stadt","Brandenburg an der Havel",
           "Rostock","Greifswald","Stralsund","Wismar","Wismar-Stadt","Schwerin","Schwerin-Stadt","Suhl","Suhl-Stadt","Friedrichshain","Hellersdorf",
           "Hohenschönhausen","Frankfurt an der Oder","Dessau-Stadt","Weimar-Stadt","Köpenick","Görlitz-Stadt","Lichtenberg","Marzahn","Mitte","Pankow",
           "Prenzlauer Berg","Treptow","Weißensee","Rostock-Stadt","Berlin Ost","Halle-Neustadt-Stadt","Eisenhüttenstadt-Stadt","Dessau-Stadt","Brandenburg-Stadt",
           "Stralsund-Stadt","Greifswald-Stadt","Zwickau-Stadt","Plauen-Stadt")


rural <- c("Bad Liebenwerda","Potsdam-Land","Calau","Brandenburg-Land","Cottbus-Land","Finsterwalde","Forst","Guben" ,"Wokrejs Gubin","Herzberg",
           "Hoyerswerda","Jessen","Luckau","Lübben","Senftenberg","Spremberg","Weißwasser", "Bautzen","Bischofswerda",
           "Dippoldiswalde","Dresden-Land","Freital","Görlitz-Land","Großenhain","Kamenz","Löbau","Meißen",
           "Niesky","Pirna","Riesa","Sebnitz","Zittau","Apolda","Arnstadt","Eisenach","Erfurt-Land","Gotha","Heiligenstadt",
           "Langensalza","Mühlhausen","Nordhausen","Sömmerda","Sondershausen","Weimar-Land","Angermünde","Bad Freienwalde",
           "Beeskow","Bernau","Eberswalde","Eisenhüttenstadt","Fürstenwalde","Seelow","Strausberg","Eisenberg","Gera-Land",
           "Greiz","Jena","Lobenstein","Pößneck","Rudolstadt","Saalfeld","Schleiz","Stadtroda","Zeulenroda","Artern",
           "Aschersleben","Bernburg","Bitterfeld","Eisleben","Gräfenhainichen","Hettstedt","Hohenmölsen","Köthen",
           "Merseburg","Naumburg","Nebra","Quedlinburg","Querfurt","Roßlau","Saalkreis","Sangerhausen","Weißenfels",
           "Wittenberg","Zeitz","Annaberg","Aue","Auerbach","Brand-Erbisdorf","Flöha","Freiberg","Glauchau","Hainichen",
           "Hohenstein-Ernstthal","Karl-Marx-Stadt-Land","Klingenthal","Marienberg","Oelsnitz","Plauen-Land",
           "Reichenbach","Rochlitz","Schwarzenberg","Stollberg","Werdau","Zschopau","Zwickau-Land","Altenburg","Borna",
           "Delitzsch","Döbeln","Eilenburg","Geithain","Grimma","Leipzig-Land","Oschatz","Schmölln","Torgau","Wurzen",
           "Burg","Gardelegen","Genthin","Halberstadt","Haldensleben","Havelberg","Kalbe","Milde","Klötze","Loburg",
           "Oschersleben","Osterburg","Salzwedel","Schönebeck","Seehausen","Staßfurt","Stendal","Stassfurt","Tangerhütte",
           "Wanzleben","Wernigerode","Wolmirstedt","Zerbst","Altentreptow","Anklam","Demmin","Malchin","Neubrandenburg-Land",
           "Neustrelitz","Pasewalk","Prenzlau","Röbel/Müritz","Röbel","Müritz","Strasburg","Templin","Teterow","Ueckermünde",
           "Waren","Belzig","Brandenburg","Gransee","Jüterbog","Königs-Wusterhausen","Kyritz","Luckenwalde","Nauen",
           "Neuruppin","Oranienburg","Potsdam","Pritzwalk","Rathenow","Wittstock","Zossen","Bad Doberan","Greifswald Land",
           "Grevesmühlen","Grimmen","Ribnitz-Damgarten","Rostock-Land","Rügen","Stralsund","Wismar","Wolgast","Bützow",
           "Gadebusch","Güstrow","Hagenow","Ludwigslust","Lübz","Parchim","Perleberg","Schwerin-Land","Sternberg",
           "Bad Salzungen","Hildburghausen","Ilmenau","Meiningen","Neuhaus","Schmalkalden","Sonneberg","Suhl-Land","Worbis",
           "Neuhaus am Rennweg","Jena-Land","Wismar-Land","Eisenhüttenstadt-Land","Kalbe (Milde)","Greifswald-Land","Stralsund-Land"   )

#craeting a new dataframe with just the columns I need, since working with a 75 column df is good to avoid when possible
PCpetitions2 <- data.frame(PCpetitions$beforeafterALL_2,PCpetitions$electionALL,PCpetitions$preALL_2,PCpetitions$timediffTOPCODED,PCpetitions$n_net, PCpetitions$GDRCounty, PCpetitions$completion.year,PCpetitions$dayofyear,PCpetitions$positiveResolution)

#These two loops create the new columns and fills them with the bezirk from which each petition is sent and whether it is a rural or urban bezirk
PCpetitions2["bezirk"] <- NA #This will contain the name of the Bezerk
PCpetitions2["urban"] <- NA #This is a dummy variable where 1 is an urban bezirk and 0 is a rural bezirk
for( i in 1:nrow(PCpetitions2)){
  if (PCpetitions2[i,6] %in% berlin){PCpetitions2[i,10] <- "berlin"}
  else if (PCpetitions2[i,6] %in% chemnitz){PCpetitions2[i,10] <- "chemnitz"}
  else if (PCpetitions2[i,6] %in% cottbus){PCpetitions2[i,10] <- "cottbus"}
  else if (PCpetitions2[i,6] %in% dresden){PCpetitions2[i,10] <- "dresden"}
  else if (PCpetitions2[i,6] %in% erfurt){PCpetitions2[i,10] <- "erfurt"}
  else if (PCpetitions2[i,6] %in% frankfurt_oder){PCpetitions2[i,10] <- "frankfurt_oder"}
  else if (PCpetitions2[i,6] %in% gera){PCpetitions2[i,10] <- "gera"}
  else if (PCpetitions2[i,6] %in% halle){PCpetitions2[i,10] <- "halle"}
  else if (PCpetitions2[i,6] %in% leipzig){PCpetitions2[i,10] <- "leipzig"}
  else if (PCpetitions2[i,6] %in% magdeburg){PCpetitions2[i,10] <- "magdeburg"}
  else if (PCpetitions2[i,6] %in% neubrandenburg){PCpetitions2[i,10] <- "neubrandenburg"}
  else if (PCpetitions2[i,6] %in% potsdam){PCpetitions2[i,10] <- "potsdam"}
  else if (PCpetitions2[i,6] %in% rostock){PCpetitions2[i,10] <- "rostock"}
  else if (PCpetitions2[i,6] %in% schwerin){PCpetitions2[i,10] <- "schwerin"}
  else if (PCpetitions2[i,6] %in% Suhl){PCpetitions2[i,10] <- "suhl"}
}
for( i in 1:nrow(PCpetitions2)){
  if (PCpetitions2[i,6] %in% urban){PCpetitions2[i,11] <- 1}
  else {PCpetitions2[i,11] <- 0}
}

#This uses the bezirk as a factor in a regression for response time aroun elections
q1 <- felm(log(PCpetitions.timediffTOPCODED + 1) ~ PCpetitions.preALL_2*PCpetitions.electionALL + as.factor(bezirk) + PCpetitions.n_net  | 
              PCpetitions.completion.year + PCpetitions.dayofyear, data=PCpetitions2, 
           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90)
summary(q1)

#This uses the bezirk as a factor in a regression for positive responses around elections
q2 <- felm(PCpetitions.positiveResolution ~ PCpetitions.preALL_2*PCpetitions.electionALL + as.factor(bezirk) + PCpetitions.n_net | 
             PCpetitions.completion.year + PCpetitions.dayofyear, data=PCpetitions2, 
           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90 & PCpetitions2$PCpetitions.completion.year <= 1982)
summary(q2)

#This uses urban as a dummy variable to predict positive resolution
q3 <- felm(PCpetitions.positiveResolution ~ PCpetitions.preALL_2*PCpetitions.electionALL + PCpetitions.n_net +urban| bezirk + PCpetitions.completion.year + PCpetitions.dayofyear | 0 | bezirk, data=PCpetitions2, 
           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90)
summary(q3)

#This uses urban as a dummy variable to predict presponse time
q4 <- felm(log(PCpetitions.timediffTOPCODED + 1) ~ PCpetitions.preALL_2*PCpetitions.electionALL + PCpetitions.n_net +urban| bezirk + PCpetitions.completion.year + PCpetitions.dayofyear | 0 | bezirk, data=PCpetitions2, 
           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90 & PCpetitions2$PCpetitions.completion.year <= 1982)
summary(q4)


stargazer(q1, q2,
          omit.stat = c("rsq", "ser", "f"),
          notes.align="c",
          no.space=T,
          model.names=F,
          dep.var.labels.include = T,
          dep.var.labels  = c("Response time (log days)", "1(Central government response)"),
          add.lines = list(
            c("Day-FE?", "yes", "yes"),
            c("Year-FE?", "yes", "yes")))

stargazer(q3, q4,
          omit.stat = c("rsq", "ser", "f"),
          notes.align="c",
          no.space=T,
          model.names=F,
          dep.var.labels.include = T,
          dep.var.labels  = c("Response time (log days)", "1(Central government response)"),
          add.lines = list(
            c("Day-FE?", "yes", "yes"),
            c("Year-FE?", "yes", "yes"),
            c("Bezirk?", "yes", "yes")))





#spare code
#n1 <- felm(log(PCpetitions.timediffTOPCODED + 1) ~ PCpetitions.preALL_2*PCpetitions.electionALL + PCpetitions.n_net + as.factor(bezirk), data=PCpetitions2,
#           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90)
#summary(n1)#
#
#m3 <- felm(log(PCpetitions.timediffTOPCODED + 1) ~ PCpetitions.preALL_2*PCpetitions.electionALL + PCpetitions.n_net + as.factor(bezirk)*as.numeric(PCpetitions.completion.year) | 
#             PCpetitions.completion.year + PCpetitions.dayofyear , data=PCpetitions2, 
#           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90)
#summary(m3)#
#
#m1 <- felm(PCpetitions.positiveResolution ~ bezirk , data=PCpetitions2, 
#           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90 & PCpetitions2$PCpetitions.completion.year <= 1982)
#summary(m1)#
#
#m3 <- felm(PCpetitions.positiveResolution ~ PCpetitions.preALL_2*PCpetitions.electionALL + PCpetitions.n_net + as.factor(bezirk)*as.numeric(PCpetitions.completion.year) | 
#             bezirk + PCpetitions.completion.year + PCpetitions.dayofyear | 0 | bezirk, data=PCpetitions2, 
#           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90 & PCpetitions2$PCpetitions.completion.year <= 1982)
#summary(m3)
#
#m1 <- felm(PCpetitions.positiveResolution ~ PCpetitions.preALL_2*PCpetitions.electionALL + PCpetitions.n_net +urban| PCpetitions.GDRCounty + PCpetitions.completion.year + PCpetitions.dayofyear| 0 | PCpetitions.GDRCounty, data=PCpetitions2, 
#           subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90 & PCpetitions2$PCpetitions.completion.year <= 1982)
#summary(m1)
#
#ty <- lm(PCpetitions.positiveResolution ~ PCpetitions.preALL_2*PCpetitions.electionALL + PCpetitions.n_net +urban, data=PCpetitions2, 
#         subset=PCpetitions2$PCpetitions.beforeafterALL_2 >= -90 & PCpetitions2$PCpetitions.beforeafterALL_2 <= 90 & PCpetitions2$PCpetitions.completion.year <= 1982)
#
#summary(ty)
#
#########
#Using
#########
tw <- lm(log(PCpetitions.timediffTOPCODED + 1) ~ urban, data=PCpetitions2)

summary(tw)


tm <- lm(PCpetitions.positiveResolution ~ urban, data=PCpetitions2)

summary(tm)


n1 <- lm(log(PCpetitions.timediffTOPCODED + 1)~PCpetitions.preALL_2*PCpetitions.electionALL+urban, data=PCpetitions2)
summary(n1)
