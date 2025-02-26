library(tidyverse)
library(readxl)

##read in the data##########################################
#data <- read_excel("/Users/gregmacfarlane/Library/CloudStorage/Box-Box/Macfarlane/research/tprs/data/spacing_data.xlsx")

data1 <- read_excel("/Users/Tyler/Box/tprs/data/spacing_data.xlsx")
#How do we make the data path relative?

data2 <- read_excel("/Users/Tyler/Box/tprs/data/test_spacing.xlsx")

###organize and mutate data#################################
data1 <- data1 |> 
  select(state, speed, spacing) |>
  print()

#Change chr to fct to better sort the states later
data1 <- mutate_if(data1, is.character, as.factor) |>
  print()

data2 <- data2 |> 
  select(specifications, speed, spacing) |>
  print()

#Change chr to fct to better sort the states later
data2 <- mutate_if(data2, is.character, as.factor) |>
  print()

##test plot#################################################

ggplot(data2, aes(x = speed, y = spacing, 
                  color = specifications)) + 
  geom_line() + 
  xlab("Speed [mph]") + 
  ylab("Strip spacing [ft]") + 
  theme_bw() +
  theme(text = element_text(size = 14, 
                            family = "Times New Roman")) +
  guides(color = guide_legend(title = "Specificiations",))


##State plot################################################

ggplot(data1, aes(x = speed, y = spacing)) + 
  geom_line() + 
  xlab("Speed [mph]") + 
  ylab("Strip spacing [ft]") +
  theme_bw() +  # white background, grey lines
  
  #axis labels in 14 pt Times New Roman font
  theme(text = element_text(size = 14, 
                            family = "Times New Roman")) +
  
  facet_wrap(~factor(state, 
                     c("Maryland", "Virginia", "Minnesota",
                       "New York","Texas", "Missouri", 
                       "California", "North Dakota",
                       "Wisconsin", "Florida", "Colorado")),
                     #puts the states in a particular order
             
             ncol = 3) + #set column count
  
  #pane titles in 14 pt Times New Roman font
  theme(strip.text = element_text(size = 14,
                               family = "Times New Roman"))
        