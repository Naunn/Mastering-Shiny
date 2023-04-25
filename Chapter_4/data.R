library(tidyverse)

## DANE ----
# Pobieranie danych
# dir.create("neiss")
# 
# download <- function(name) {
#   url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
#   download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
# }
# download("injuries.tsv.gz")
# download("population.tsv")
# download("products.tsv")

# Przypisanie danych
injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries
# trmt_date - date the person was seen in the hospital (not when the accident occurred).
# age, sex, and race - demographic information about the person who experienced the accident.
# body_part - the location of the injury on the body (like ankle or ear).
# location - the place where the accident occurred (like home or school).
# diag - the basic diagnosis of the injury (like fracture or laceration).
# prod_code - the primary product associated with the injury.
# weight - statistical weight giving the estimated number of people who would suffer
# this injury if this dataset was scaled to the entire population of the US.
# narrative - a brief story about how the accident occurred.

products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom("neiss/population.tsv")
population

## EKSPLORACJA ----
products[products$prod_code == 649,]
# # A tibble: 1 x 2
# prod_code title  
# <dbl> <chr>  
#   1       649 toilets

selected <- injuries %>% filter(prod_code == 649)
nrow(selected)
# [1] 2993

# Weight is by the weight variable so that the counts can be interpreted
# as estimated total injuries across the whole US.
selected %>% count(location, wt = weight, sort = TRUE)
# # A tibble: 6 x 2
# location                       n
# <chr>                        <dbl>
# 1 Home                       99603. 
# 2 Other Public Property      18663. 
# 3 Unknown                    16267. 
# 4 School                      659. 
# 5 Street Or Highway           16.2
# 6 Sports Or Recreation Place  14.8

selected %>% count(body_part, wt = weight, sort = TRUE)
# # A tibble: 24 x 2
# body_part       n
# <chr>         <dbl>
# 1 Head        31370.
# 2 Lower Trunk 26855.
# 3 Face        13016.
# 4 Upper Trunk 12508.
# 5 Knee         6968.
# 6 N.S./Unk     6741.
# 7 Lower Leg    5087.
# 8 Shoulder     3590.
# 9 All Of Body  3438.
# 10 Ankle       3315.

selected %>% count(diag, wt = weight, sort = TRUE)
# # A tibble: 20 x 2
# diag                      n
# <chr>                   <dbl>
# 1 Other Or Not Stated   32897. 
# 2 Contusion Or Abrasion 22493. 
# 3 Inter Organ Injury    21525. 
# 4 Fracture              21497. 
# 5 Laceration            18734. 
# 6 Strain, Sprain        7609. 
# 7 Dislocation           2713. 
# 8 Hematoma              2386. 
# 9 Avulsion              1778. 
# 10 Nerve Damage         1091. 
# 11 Poisoning             928. 
# 12 Concussion            822. 
# 13 Dental Injury         199. 
# 14 Hemorrhage            167. 
# 15 Crushing              114. 
# 16 Dermat Or Conj        84.2
# 17 Burns, Not Spec       67.2
# 18 Puncture              67.2
# 19 Burns, Thermal        34.0
# 20 Burns, Scald          17.0

summary <- selected %>% count(age, sex, wt = weight, sort = TRUE)
summary
summary %>%
  ggplot(aes(age, n, colour = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")
# Szczyt dla mlodych chlopcow moze wynikac z tego, ze chlopcy zwykle
# korzystają z toalety na stojaco, a wzrost dla kobiet moze wynikac z osteoporozy
# (tzn. mozliew, ze kobiety i mezczyzni maja urazy w tym samym tempie,
# ale wiecej kobiet trafia na ER, poniewaz sa w wyzszym ryzyku zlaman).

# Ale jak to sie ma w kontekscie procentowym? Przeciez jest nierowna ilosc ludzi w roznym wieku
summary <- selected %>% 
  count(age, sex, wt = weight, sort = TRUE) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4) # 1e4 = 10000
# mutate() pokaze nam ile osob w tym wieku i o tej plci doznaje wypadkujow na 10tys. osob
summary %>%
  ggplot(aes(age, rate, colour = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people")

selected %>% 
  sample_n(10, replace = FALSE) %>% # Losowa probka (bez powtorzen)
  pull(narrative)  # .$narrative # Wyciagniecie kolumny bez uzywania $

# przekonwertowanie zmiennej na czynnik (factor), uporzadkowanie wedlug
# czestosci poziomow, a nastepnie zrzucenie wszystkich poziomow po 5 najlepszych
injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
  # fct_infreq() - change order (of the levels) by number of observations with each level
  # fct_lump() - for lumping together levels that meet some criteria
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))
# # A tibble: 6 x 2
# diag                        n
# <fct>                   <int>
# 1 Other Or Not Stated   1806436
# 2 Fracture              1558961
# 3 Laceration            1432407
# 4 Strain, Sprain        1432556
# 5 Contusion Or Abrasion 1451987
# 6 Other                 1929147





