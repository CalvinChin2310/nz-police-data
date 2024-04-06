```{rsetup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


```

```{r setup, include=FALSE}

library(tidyverse)
library(viridis)


```
# Methods and Analysis

## 1. loading and arranging/cleaning the datasets
```{r reading the csv files of the datasets used, include=FALSE}

data_1 <- read.csv("demographic.csv") #dataset for victimisation, age-group, year/month, ethnicity & sex
data_2 <- read.csv("police2.csv") #dataset for police district, same date but different csv files

head(data_1)
str(data_1)


head(data_2)
str(data_2)


```

## 2. Removing NA or non-applicable categorical or numerical data.
```{r cleaning and sorting the variables, include=FALSE}

#for the Age.Group variables in data_1
unique(data_1$Age.Group)
data_1$Age.Group <- as.factor(data_1$Age.Group)
class(data_1$Age.Group)

levels(data_1$Age.Group)
data_1$Age.Group <- factor((data_1$Age.Group),
                           levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                      "25-29", "30-34", "35-39", "40-44", 
                                      "45-49", "50-54", "55-59","60-64", 
                                      "65-69", "70-74", "75-79",
                                      "80yearsorover"))
levels(data_1$Age.Group)
```

```{r cleaning and sorting the year variables, include=FALSE}
#for the Year Variable in data_1
unique(data_1$Year)
class(data_1$Year)
data_1$Year <- as.factor(data_1$Year)
class(data_1$SEX)


data_1$Year <- factor((data_1$Year),
                      levels = c("2014", "2015", "2016", "2017", "2018",
                                 "2019", "2020", "2021", "2022", "2023"))
levels(data_1$Year)
```

```{r cleaning and sorting the sex variables, include=FALSE}
#for the Sex variable in data_1
unique(data_1$SEX)
data_1$SEX <- as.factor(data_1$SEX)
class(data_1$SEX)


data_1$SEX <- factor((data_1$SEX),
                     levels = c("Male", "Female"))
levels(data_1$SEX)


```

```{r cleaning and sorting the ethnicity variables, include=FALSE}
#for the Ethnicity variable in data_1
unique(data_1$Ethnicity)
data_1$Ethnicity <- as.factor(data_1$Ethnicity)
class(data_1$Ethnicity)


data_1$Ethnicity <- factor((data_1$Ethnicity),
                           levels = c("Asian", "European", "Indian", 
                                      "Maori", "Not Stated", "Other Ethnicities",
                                      "Pacific Island" ))
levels(data_1$Ethnicity)
```

```{r cleaning and sorting the police district variables, include=FALSE}
#for the Police.District variable from data_2
unique(data_2$Police.District)
data_2$Police.District <- as.factor(data_2$Police.District)
class(data_2$Police.District)

levels(data_2$Police.District)
data_2$Police.District <- factor((data_2$Police.District),
                                 levels = c("Auckland City", "Bay of Plenty", "Central",
                                            "Canterbury", "Counties/Manukau", "Eastern","Southern",
                                            "Northland", "Tasman",
                                            "Waitemata", "Waikato", "Wellington"))
levels(data_2$Police.District)
```
### 3. Load the required packages to plot the graph of choice.

## (a) Year vs victimisations
```{r plot between year vs victimisation, echo=FALSE}

# Line graph plot between the Year vs Sex variables in data_1

data_1 %>%
  drop_na(SEX, Year) %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2022))) %>%
  group_by(SEX, Year) %>%
  summarise(cnt = n(),
            .groups = "keep") %>%
  ggplot(aes(x=Year, y=cnt, group = SEX, color = SEX)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = cnt), vjust = 1.5, size =3) +
  scale_color_manual(values = c("Male" = "orange",
                                "Female" = "green")) +
  
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "gray95"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  labs(title = "cases of theft based on gender from 2017-2021",
       x = "Year",
       y = " Total theft related cases")
```

## (b) Age Group of offenders vs Year 
```{r cases by Age Group, echo=FALSE}
library(RColorBrewer) #we load packages 
nb.cols <- 14
My_colour <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)

data_1 %>%
  drop_na(Age.Group, Year) %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2022))) %>%
  filter(!(Age.Group %in% c("0-4"))) %>%
  ggplot(aes(Year, fill = Age.Group))+
  geom_bar(position = "dodge",
           alpha = 1, width = 0.9)+
  scale_fill_manual(values = My_colour)+
  labs(title = "cases of theft based on Age Group Between 2017-2021",
       x = "Year",
       y = "victims involved")
```

## (c) District in which theft happened vs Year
```{r cases by demographic, echo=FALSE}

data_2 %>%
  drop_na(Police.District, Year) %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2022, 2023))) %>%
  ggplot(aes(Year, fill = Police.District))+
  geom_bar(alpha = 1, width = 0.5)+
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(guide = guide_axis(n.dodge=6))+
  facet_wrap(~Police.District)+
  labs(title = "Theft in police districts Between 2017-2021",
       x = "Year of analysis from 2017-2021",
       y = "Total theft related cases")
```

## (d) Ethnicity of offenders vs Year
```{r cases by ethnic.Group, echo=FALSE}
data_1 %>%
  drop_na(Ethnicity, Year) %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2022))) %>%
  group_by(Ethnicity, Year) %>%
  summarise(cnt = n(),
            .groups = "keep") %>%
  ggplot(aes(x=Year, y=cnt, group = Ethnicity, color = Ethnicity))+ 
  geom_line()+
  geom_point() +
  geom_text(aes(label = cnt), vjust = -0.5, size = 4) +
  
  labs(title = "Theft Cases By Ethnicity from 2017-2021",
       x = "Year",
       y = "Cases")


```
# Victimisation throughout 2017 - 2021

```{r conclusion, echo=FALSE}

data_1 %>%
  drop_na(Victimisations, Year) %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2022))) %>%
  group_by(Victimisations, Year) %>%
  summarise(cnt = n(),
            .groups = "keep") %>%
  ggplot(aes(x = Year, y = cnt, fill = Victimisations)) +
  geom_col() +
  scale_fill_gradient(low = "darkgreen", high = "red", limits = c(0, 1000)) +
  theme_minimal() +
  labs(title = "Theft Cases Between 2017-2021",
       x = "Year",
       y = "Cases")



```

# Appendix

```{r  echo=FALSE}
data_2 %>%
  drop_na(Police.District, Year) %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2022, 2023))) %>%
  filter(!(Police.District %in% c("Canterbury", "Tasman", "Eastern", "Southern", "Central"))) %>%
  ggplot(aes(Year, fill = Police.District))+
  geom_bar(alpha = 1, width = 0.5)+
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  facet_wrap(~Police.District)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Theft cases from North Island police districts Between 2017-2021",
       x = "Year of analysis from 2017-2021",
       y = "Total theft related cases")
```

```{r  echo=FALSE}
data_2 %>%
  drop_na(Police.District, Year) %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2022, 2023))) %>%
  filter(!(Police.District %in% c("Auckland City", "Counties/Manukau", "Northland", "Waitemata", "Waikato", "Wellington", "Eastern", "Southern", "Central"))) %>%
  ggplot(aes(Year, fill = Police.District))+
  geom_bar(alpha = 1, width = 0.5)+
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  facet_wrap(~Police.District)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Theft cases from South Island police districts Between 2017-2021",
       x = "Year of analysis from 2017-2021",
       y = "Total theft related cases")


```
```{r echo=FALSE}
data_1 %>%
  drop_na(Victimisations, Year) %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2022))) %>%
  group_by(Victimisations, Year) %>%
  summarise(cnt = n(),
            .groups = "keep") %>%
  ggplot(aes(x=Year, y=cnt, group=Victimisations, colour=Victimisations))+
  geom_line()+
  geom_point()+
  geom_text(aes(label = cnt), vjust = -0.5, size = 4) +
  scale_color_gradient(low="blue", high="red") +
  theme_minimal()+
  labs(title = "Theft Cases Between 2017-2021",
       x = "Year",
       y = "Cases")

```