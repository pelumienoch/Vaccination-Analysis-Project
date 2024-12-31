pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  gtsummary,
  skimr
)

here()
yusuf <- import(here("Dataset.xlsx"))
### now, lets view the dataset
head(yusuf)  # this shows only the first 6 rows by default
head(yusuf, 10)   # this shows the firt 10 rows
tail(yusuf)
### lets skim the data, that is, to provide a summary of the dataset
skimr::skim(yusuf)

### lets check the names of the column
names(yusuf)
### Transform multiple columns
## Here the transformation as.factor() is applied to specific columns named within across()

yusuf <- yusuf %>% 
  mutate(across(.cols = -c(SN), .fns = as.factor))
skimr::skim(yusuf)

### I want to cut them into sections, now section A
yusufA <- yusuf %>%
  select(Age:Occupation)
### Section B
yusufB <- yusuf %>%
  select(`Have you ever heard of immunization schedule before`: `At what age are children expected to receive their first immunization`)
### Section C
yusufC <- yusuf %>%
  select(`When was your child’s last vaccination`:`Have you ever been referred to another health facility for missed vaccines`)
yusufD <- yusuf %>%
  select(`What are challenges you face in assessing vaccination services, I. Lack of awareness about vaccination schedules`:
           `How far is the Primary health center from your home`)

### I am creating the sociodemographic tables
yA <- yusufA %>%
  tbl_summary() ### table is created

yA <- as.data.frame(yA) ### table is converted to data frame so as to print the table in docx with flextable

pacman::p_load(flextable)
yAA  <- flextable(yA) %>%
  save_as_docx(yA, path = "socio demograhic table.docx")


### I am creating the section B tables
yB <- yusufB %>%
  tbl_summary() ### table is created

yB <- as.data.frame(yB) ### table is converted to data frame so as to print the table in docx with flextable

pacman::p_load(flextable)
yBB  <- flextable(yB) %>%
  save_as_docx(yB, path = "section B.docx")
  

### I am creating the section C tables
yC <- yusufC %>%
  tbl_summary() ### table is created

yC <- as.data.frame(yC) ### table is converted to data frame so as to print the table in docx with flextable

yCC  <- flextable(yC) %>%
  save_as_docx(yC, path = "section C.docx")


### I am creating the section D tables
yD <- yusufD %>%
  tbl_summary() ### table is created

yD <- as.data.frame(yD) ### table is converted to data frame so as to print the table in docx with flextable

yDD  <- flextable(yD) %>%
  save_as_docx(yD, path = "section D.docx")



### want to perform a combinationa analysis
yusuf2 <- import(here("Yusuf raw data.xlsx"), which = "B")

names(yusuf2)
# create column with the symptoms named, separated by semicolons
y1<- yusuf2 %>%
  mutate(BCG = ifelse(BCG == "1", "BCG", NA),
         BopV = ifelse(BopV == "1", "BoPV", NA),
         Penta = ifelse(Penta == "1", "Penta", NA),
         Td = ifelse(Td == "1", "Td", NA),
         Rota = ifelse(Rota == "1", "Rota", NA),
         MenA = ifelse(MenA == "1", "MenA", NA),
         Measles = ifelse(Measles == "1", "Measles", NA),
         HPV = ifelse(HPV == "1", "HPV", NA),
         IPV = ifelse(IPV == "1", "IPV", NA))


y1<- y1 %>% 
  unite(col = "vaccinationKnowledge",
        c(BCG, BopV, Penta, Td, Rota, MenA, Measles, HPV, IPV), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of vaccinationKnowledge column, but of class "list" (which is required to use ggupset() in next step)
    vaccinationKnowledge_list = as.list(strsplit(vaccinationKnowledge, "; "))
  )

pacman::p_load(
  tidyverse,     # data management and visualization
  UpSetR,        # special package for combination plots
  ggupset)       # special package for combination plots



ggplot(
  data = y1,
  mapping = aes(x = vaccinationKnowledge_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("BCG", "BoPV", "Penta", "Td", "Rota", "MenA", "Measles", "HPV","IPV"))+
  labs(
    title = "Knowledge of Vaccination",
    subtitle = "The frequent combinations of the knowledge of available vaccines by particpants",
    #caption = "Caption here.",
    x = "vaccines combinations",
    y = "Frequency in dataset")


### I want to change the 1 and 2, i want to change the 2 to 0

y2<- yusuf2 %>%
  mutate(BCG = ifelse(BCG == "1", "1", 0),
          BopV = ifelse(BopV == "1", "1", 0),
          Penta = ifelse(Penta == "1", "1", 0),
          Td = ifelse(Td == "1", "1", 0),
          Rota = ifelse(Rota == "1", "1", 0),
          MenA = ifelse(MenA == "1", "1", 0),
          Measles = ifelse(Measles == "1", "1", 0),
          HPV = ifelse(HPV == "1", "1", 0),
          IPV = ifelse(IPV == "1", "1", 0))


y2 %>% 
  mutate(across(.cols = everything(), .fns = as.numeric)) %>%
  UpSetR::upset(
    sets = c("BCG", "BopV", "Penta", "Td", "Rota", "MenA", "Measles", "HPV","IPV"),
    order.by = "freq",
    sets.bar.color = c("blue", "red", "yellow", "darkgreen", "green", "orange", "brown", "purple", "grey"), # optional colors
    empty.intersections = "on",
    # nsets = 3,
    nintersects = 12,   # number of bars to plot, in this case, plot the top 12
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "vaccine combination",
    sets.x.label = "knowledge of vaccines")
skim(y2)


### want to perform a combinationa analysis
yusuf3 <- import(here("Yusuf raw data.xlsx"), which = "C")

names(yusuf3)
# create column with the symptoms named, separated by semicolons
y3<- yusuf3 %>%
  mutate(BCG = ifelse(BCG == "1", "BCG", NA),
         BoPV = ifelse(BoPV == "1", "BoPV", NA),
         Penta = ifelse(Penta == "1", "Penta", NA),
         Td = ifelse(Td == "1", "Td", NA),
         Rota = ifelse(Rota == "1", "Rota", NA),
         MenA = ifelse(MenA == "1", "MenA", NA),
         Measles = ifelse(Measles == "1", "Measles", NA),
         HPV = ifelse(HPV == "1", "HPV", NA),
         IPV = ifelse(IPV == "1", "IPV", NA),
         YF = ifelse(YF == "1", "YF", NA))


y3<- y3 %>% 
  unite(col = "vaccinationmissed",
        c(BCG, BoPV, Penta, Td, Rota, MenA, Measles, HPV, IPV, YF), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of vaccinationKnowledge column, but of class "list" (which is required to use ggupset() in next step)
    vaccinationmissed_list = as.list(strsplit(vaccinationmissed, "; "))
  )

pacman::p_load(
  tidyverse,     # data management and visualization
  UpSetR,        # special package for combination plots
  ggupset)       # special package for combination plots



ggplot(
  data = y3,
  mapping = aes(x = vaccinationmissed_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("BCG", "BoPV", "Penta", "Td", "Rota", "MenA", "Measles", "HPV","IPV", "YF"))+
  labs(
    title = "Vaccine missed",
    subtitle = "The frequent combinations of the vaccines missed by particpants",
    #caption = "Caption here.",
    x = "vaccines combinations",
    y = "Frequency in dataset")


### I want to change the 1 and 2, i want to change the 2 to 0

y4<- yusuf3 %>%
  mutate(BCG = ifelse(BCG == "1", "1", 0),
         BoPV = ifelse(BoPV == "1", "1", 0),
         Penta = ifelse(Penta == "1", "1", 0),
         Td = ifelse(Td == "1", "1", 0),
         Rota = ifelse(Rota == "1", "1", 0),
         MenA = ifelse(MenA == "1", "1", 0),
         Measles = ifelse(Measles == "1", "1", 0),
         HPV = ifelse(HPV == "1", "1", 0),
         IPV = ifelse(IPV == "1", "1", 0),
         YF = ifelse(YF == "1", "1", 0))


y4 %>% 
  mutate(across(.cols = everything(), .fns = as.numeric)) %>%
  UpSetR::upset(
    sets = c("BCG", "BoPV", "Penta", "Td", "Rota", "MenA", "Measles", "HPV","IPV", "YF"),
    order.by = "freq",
    sets.bar.color = c("blue", "red", "yellow", "darkgreen", "green", "orange", "brown", "purple", "grey", "pink"), # optional colors
    empty.intersections = "on",
    # nsets = 3,
    nintersects = 10,   # number of bars to plot, in this case, plot the top 12
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "vaccine combination",
    sets.x.label = "missed vaccines")
skim(y4)



### want to perform a combinationa analysis
yusuf4 <- import(here("Yusuf raw data.xlsx"), which = "D")

names(yusuf4)
# create column with the symptoms named, separated by semicolons
y3<- yusuf4 %>%
  mutate(`Lack of awareness about vaccination schedules` = ifelse(`Lack of awareness about vaccination schedules` == "1", 
                                                                  "Lack of awareness about vaccination schedules", NA),
         `Stock outs/unavailability of some vaccines` = ifelse(`Stock outs/unavailability of some vaccines` == "1", 
                                                               "Stock outs/unavailability of some vaccines", NA),
         `Long waiting hours` = ifelse(`Long waiting hours` == "1", "Long waiting hours", NA),
         `No communication from health workers` = ifelse(`No communication from health workers` == "1", 
                                                         "No communication from health workers", NA),
         `Long distance to Health centre` = ifelse(`Long distance to Health centre` == "1", 
                                                   "Long distance to Health centre", NA),
         `Cultural believe` = ifelse(`Cultural believe` == "1", "Cultural believe", NA),
        `Religious beliefs` = ifelse(`Religious beliefs` == "1", "Religious beliefs", NA),
         `No reminder/recal from health centre` = ifelse(`No reminder/recal from health centre` == "1", "No reminder/recal from health centre", NA),
         `Msiconception about vaccines` = ifelse(`Msiconception about vaccines` == "1", "Msiconception about vaccines", NA),
         YF = ifelse(YF == "1", "YF", NA))


y3<- y3 %>% 
  unite(col = "vaccinationmissed",
        c(BCG, BoPV, Penta, Td, Rota, MenA, Measles, HPV, IPV, YF), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of vaccinationKnowledge column, but of class "list" (which is required to use ggupset() in next step)
    vaccinationmissed_list = as.list(strsplit(vaccinationmissed, "; "))
  )

pacman::p_load(
  tidyverse,     # data management and visualization
  UpSetR,        # special package for combination plots
  ggupset)       # special package for combination plots



ggplot(
  data = y3,
  mapping = aes(x = vaccinationmissed_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("BCG", "BoPV", "Penta", "Td", "Rota", "MenA", "Measles", "HPV","IPV", "YF"))+
  labs(
    title = "Vaccine missed",
    subtitle = "The frequent combinations of the vaccines missed by particpants",
    #caption = "Caption here.",
    x = "vaccines combinations",
    y = "Frequency in dataset")


### I want to change the 1 and 2, i want to change the 2 to 0

y4<- yusuf3 %>%
  mutate(BCG = ifelse(BCG == "1", "1", 0),
         BoPV = ifelse(BoPV == "1", "1", 0),
         Penta = ifelse(Penta == "1", "1", 0),
         Td = ifelse(Td == "1", "1", 0),
         Rota = ifelse(Rota == "1", "1", 0),
         MenA = ifelse(MenA == "1", "1", 0),
         Measles = ifelse(Measles == "1", "1", 0),
         HPV = ifelse(HPV == "1", "1", 0),
         IPV = ifelse(IPV == "1", "1", 0),
         YF = ifelse(YF == "1", "1", 0))


y4 %>% 
  mutate(across(.cols = everything(), .fns = as.numeric)) %>%
  UpSetR::upset(
    sets = c("BCG", "BoPV", "Penta", "Td", "Rota", "MenA", "Measles", "HPV","IPV", "YF"),
    order.by = "freq",
    sets.bar.color = c("blue", "red", "yellow", "darkgreen", "green", "orange", "brown", "purple", "grey", "pink"), # optional colors
    empty.intersections = "on",
    # nsets = 3,
    nintersects = 10,   # number of bars to plot, in this case, plot the top 12
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "vaccine combination",
    sets.x.label = "missed vaccines")
skim(y4)

