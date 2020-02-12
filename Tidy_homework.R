library(tidyverse)

path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
wide_data

new_tidy_data <- wide_data %>% gather(year, fertility, '1960':'2015')
head(new_tidy_data)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)


co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))


library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat_tidy <- spread(dat, gender, admitted)

library(Lahman)
top <- Batting  %>% filter(yearID == 2016) %>% arrange(desc(HR)) %>% slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()
# player ID, first name, last name, and number of HR for the top 10 players
top_names <- top %>% left_join(Master) %>% select(playerID, nameFirst, nameLast, HR)

#Top salaries 
top_salary <- Salaries %>% filter(yearID == 2016) %>% right_join(top_names) %>% select(nameFirst, nameLast, teamID, HR, salary)

#Awards 
Awards2016 <- AwardsPlayers %>% filter(yearID == 2016) 
length(intersect(Awards2016$playerID, top_names$playerID))
length(setdiff(Awards2016$playerID, top_names$playerID))

##Webscraping
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)


#guac recipy
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

### MLB payouts
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
html_text(nodes[[8]])
html_table(nodes[[4]])

tab1 = html_table(nodes[[10]])
tab2 = html_table(nodes[[19]])
tab_1 = tab1[-1, -1]
colnames(tab_1) = c("Team", "Payroll", "Average")
tab_2 = tab2[-1, ]
colnames(tab_2) = c("Team", "Payroll", "Average")

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h <- read_html(url)
tab <- h %>% html_nodes("table")

schools = c("U. Kentucky",                 "Univ New Hampshire",          "Univ. of Massachusetts",      "University Georgia", "U California",                "California State University")
schools

temp <- str_extract_all(polls$dates,"\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

### Text-Mining
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
file = gutenberg_download(1342) #Pride and prej text
#example = tibble(file)
words = file %>% unnest_tokens(word, text)

tidy_document <- words %>% filter(!word %in% stop_words$word & !str_detect(word, "^\\d"))
  #anti_join(stop_words, by = c("word" = "word"))

## A fin
afinn <- get_sentiments("afinn") %>% select(word, value)
afinn_sentiments = tidy_document %>% filter(tidy_document$word %in% afinn$word)
t = afinn_sentiments %>% left_join(afinn, by = "word")

### Puerto Rico Project
library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)
txt = pdf_text(fn)
x = str_split(txt[[9]], "\n")
