library(ggplot2)
library(dplyr)
library(writexl)

#importing IMF for population and GDP
imf <- read.delim("/Volumes/GoogleDrive/My Drive/Projects/Code/Democracy v GDP/WEOOct2023all (3).xls")

#importing EIU for democracy index
eiu <- read.csv("/Volumes/GoogleDrive/My Drive/Projects/Code/Democracy v GDP/democracy-index-eiu.csv")

#filtering down imf to just give gdp and population
imf <- imf %>%
  filter(Subject.Descriptor %in% c('Gross domestic product per capita, current prices', 'Population')) %>%
  filter(Units %in% c('U.S. dollars', 'Persons')) %>%
  mutate_all(~ as.character(.) %>% gsub(",", "", .) %>% type.convert(as.is = TRUE))
imf <- imf[, c("X2022", "Country", "ISO", "Subject.Descriptor")] #just relevant columns

#splitting into individual datasets that are all 2022 data
population <- subset(imf, Subject.Descriptor == "Population") %>%
  rename(population = X2022)
gdp <- subset(imf, Subject.Descriptor!= "Population") %>% #gdp is everything NOT population
  rename(gdp = X2022)
democracy <- subset(eiu, Year == "2022") %>%
  filter(Code != "") %>% #removing all blank codes (eg continents)
  rename(democracy = democracy_eiu) %>%
  rename(ISO = Code)

#combining them into 1 dataset
combined <- merge(population, gdp, by = 'ISO', all = TRUE) #combining the two IMF sources
combined <- merge(combined, democracy, by = 'ISO', all = TRUE) %>% #now with the EIU source
  select(ISO, Country.x, gdp, democracy, population) %>% #removing unneccessary columns 
  rename(code = ISO) %>%
  rename(country = Country.x) %>%
  filter_all(all_vars(!is.na(.))) %>% #removed all rows with a blank or n/a cell in any column
  filter_all(all_vars(!(. %in% c('n/a')))) #same for any 'n/a's so now all rows contain values for all columns

#making numeric
combined$gdp <- as.numeric(combined$gdp)
combined$population <- as.numeric(combined$population)

#plots
ggplot(combined, aes(x = democracy, y = gdp, size = population)) +
  geom_point() +
  scale_size_continuous(range = c(3, 20)) +  #making all the dots bigger
  labs(title = "Scatter Plot of Democracy vs. GDP",
       x = "Democracy",
       y = "GDP per capita",
       size = "Population")

#save table and make scatterplot pretty
file_path <- file.path("/Volumes/GoogleDrive/My Drive/Projects/Code/Democracy v GDP/", "dataset.csv")
write.csv(combined, file = file_path, row.names = FALSE)
