
library(tidyverse)


# wczytanie danych

data = readxl::read_xlsx('Meta 2.1.xlsx', sheet = 3)
data %>% head


# usunięcie pustych kolumn

data = data %>%
  janitor::remove_constant(na.rm = TRUE)      


# zmiana nazw kolumn 

data %>% colnames

data = data %>%
  janitor::clean_names()

names(data) = c('bibitem', 
                'first_author',
                'part',
                'year',
                'plan', 
                'group',
                'game_name',
                'game_type',
                'prosocial',
                'cooperative',
                'multiplayer',
                'compared_game_type',
                'effect_size',
                'effect_coef',
                'SMD_calc',
                'SMD',
                'v',
                'SE',
                'n',
                'test',
                'n_con',
                'm_con',
                'sd_con',
                'n_prosoc',
                'm_prosoc',
                'sd_prosoc',
                'measured_construct')


# podgląd danych

View(data)
dplyr::glimpse(data)
skimr::skim(data)
summarytools::dfSummary(data) %>%
  summarytools::view(., footnote = '', custom.css = 'tiny_text.css', table.classes = 'tiny-text')
kableExtra::kable_styling(knitr::kable(data), font_size = 8)


# transformacja danych # dane o artykułach (czyszczenie + dodanie ważnych kolumn)

data %>%
  distinct(bibitem)

data = data %>%
  mutate(id = 1:nrow(.),
         bibitem = if_else(!(str_sub(bibitem, 1, 2) %in% c('A.', 'B.')), bibitem, str_sub(bibitem, 4, -1L)),
         .before = 1) %>%
  fill(bibitem, .direction = 'down') %>%
  mutate(bibitem = if_else(is.na(str_locate(str_sub(bibitem, str_locate(bibitem, '[)]')[,1] + 3, -1L), '\\.|\\(')[,1]),
                           str_c(bibitem, '.'),
                           bibitem),
         bibitem = if_else(id == 26,
                           str_c(str_sub(bibitem, 1, 10), '(', year, '). ', str_sub(bibitem, 11, -1L)),
                           if_else(str_sub(bibitem, str_locate(bibitem, '[)]')[,1] + 1, str_locate(bibitem, '[)]')[,1] + 1) != '.',
                                   str_c(str_sub(bibitem, 1, str_locate(bibitem, '[)]')[,1]), '.', str_sub(bibitem, str_locate(bibitem, '[)]')[,1] + 1, -1L)),
                                   bibitem)),
         bibitem = if_else(str_sub(bibitem, str_locate(bibitem, '[)]')[,1] + 2, str_locate(bibitem, '[)]')[,1] + 2) != ' ',
                           str_c(str_sub(bibitem, 1, str_locate(bibitem, '[)]')[,1] + 1), ' ', str_sub(bibitem, str_locate(bibitem, '[)]')[,1] + 2, -1L)),
                           bibitem),
         bibitem = if_else(str_detect(str_sub(bibitem, 1, str_locate(bibitem, '[(]')[,1] - 1), ';') | !str_detect(str_sub(bibitem, 1, str_locate(bibitem, '[(]')[,1] - 1), '[.]'),
                           lapply(str_split(str_sub(bibitem, 1, str_locate(bibitem, '[(]')[,1] - 1), '; '),
                                  function(x) {name_joiner = function(y) {if (length(y) >= 3){
                                                                            name_joiner(c(str_c(y[1:2], collapse = '., '), y[3:length(y)]))
                                                                          } else if (length(y) == 2){
                                                                            name_joiner(str_c(y[1:2], collapse = '., & '))
                                                                          } else {
                                                                            y
                                                                          }};
                                               name_joiner(str_sub(x, 1, str_locate(x, ',')[,1] + 2)) %>%
                                                 str_c('. ')}) %>% unlist() %>%
                              str_c(str_sub(bibitem, str_locate(bibitem, '[(]')[,1], -1L)),
                           bibitem),
         title = str_sub(bibitem,
                         str_locate(bibitem, '[)]')[,1] + 3,
                         str_locate(bibitem, '[)]')[,1] + 2 + str_locate(str_sub(bibitem, str_locate(bibitem, '[)]')[,1] + 3, -1L), '\\.|\\(')[,1] - 1),
         title = if_else(title != 'D7',
                         title,
                         str_sub(bibitem,
                                 str_locate(bibitem, '-')[,1] + 1,
                                 str_locate(bibitem, '-')[,1] + str_locate(str_sub(bibitem, str_locate(bibitem, '-')[,1] + 1, -1L), '\\.|\\(')[,1] - 1)),
         .after = 'year') %>%
  relocate(c(bibitem, part), .after = title) %>%
  mutate(cite = str_sub(bibitem, 1, str_locate(bibitem, ',')[,1] - 1) %>%
                  str_c(year,
                        if_else(!(str_split(title, ' ') %>% lapply(`[`, 1) %in% c('A', 'An', 'The')),
                                str_split(title, ' ') %>% lapply(`[`, 1),
                                str_split(title, ' ') %>% lapply(`[`, 2)) %>%
                          str_remove_all('[^[:alnum:]]'),
                        '_',
                        part) %>%
                  str_to_lower() %>%
                  str_replace_all(' ', '_') %>%
                  str_remove_all("’"),
         .after = id) %>%
  mutate(first_author = str_remove_all(first_author, ' et al.| el at.'),
         first_author = if_else(!str_detect(first_author, ','),
                                first_author,
                                str_sub(first_author, 1, str_locate(first_author, ',')[,1] - 1))) %>%
  mutate_at(vars(c(first_author, title)), str_to_title) 


# transformacja danych # zmiana typów zmiennych ilościowych

data = data %>%
  mutate_at(vars(c(year,
                   prosocial,
                   cooperative,
                   multiplayer,
                   starts_with('n'))), as.integer) %>%
  mutate(SMD = as.numeric(SMD)) %>%
  relocate(effect_coef, .before = 'effect_size')

data %>% janitor::tabyl(sd_prosoc)
data %>% janitor::tabyl(sd_con)
data %>% janitor::tabyl(m_con)
data %>% janitor::tabyl(m_prosoc)

data = data %>%
  mutate(sd_prosoc = str_replace(sd_prosoc, '=,', '=0,')) %>% 
  mutate_at(vars(starts_with('sd_')), ~str_remove_all(.x, '95%ci=')) %>%
  mutate_at(vars(matches('^sd_|^m_')), ~as.numeric(str_replace_all(.x, ',', '.')))
  

# transformacja danych # etykietowanie zmiennych jakościowych

data %>% janitor::tabyl(plan) 
data %>% janitor::tabyl(game_type) 
data %>% janitor::tabyl(compared_game_type) 

data = data %>%
  mutate(plan = factor(if_else(plan == 'eksperyment', 'E', 'C')),
         game_type = factor(if_else(game_type == 'video', 'V', 'O')),
         compared_game_type = case_when(compared_game_type == 'competitve' ~ 'competitive',
                                        compared_game_type == 'cooperetive' ~ 'cooperative',
                                        TRUE ~ compared_game_type))

data %>% janitor::tabyl(group) 
data = data %>% 
  mutate(group = str_remove_all(group, '[(]|[)]|lat|[+]| imigranci') %>%
                 str_replace_all(c('  ' = ' ',
                                   'młodzeż' = 'młodzież',
                                   'przedszkolaki' = 'dzieci',
                                   '^18-42' = 'dorośli 18-42',
                                   '6-8, 8-10, 10-12, 12-14' = '6-14')) %>%
                 str_trim('right')) %>%
  separate(col = 'group',                                   
           into = c('group', 'age_bounds'),                  
           sep = ' ') %>%
  separate(col = 'age_bounds',                                   
           into = c('age_lower_bound', 'age_upper_bound'),                  
           sep = '-',
           convert = TRUE) %>%
  mutate(group = case_when(group == 'dzieci' ~ 'C',         # childrens
                           group == 'młodzież' ~ 'Y',       # youth
                           group == 'studenci' ~ 'S',       # students
                           group == 'dorośli' ~ 'A') %>%    # adults
                  factor(levels = c('C', 'Y', 'S', 'A')))  
  

# transformacja danych # rozwinięcie testów statystycznych

data %>% janitor::tabyl(test) 
data = data %>% 
  mutate(test = str_to_lower(test) %>%
                  str_remove_all('\\[|\\]') %>%
                  str_replace_all(
                           c('[.]' = ',',
                             ';' = ',',
                             'chi2' = 'chi',
                             't [(]' = 't(',
                             'f [(]' = 'f(',
                             '\n' = ' ',
                             ' +' = ' ',
                             ' ,' = ',',
                             '= ' = '=',
                             ' =' = '=',
                             '> ' = '>',
                             ' >' = '>',
                             '< ' = '<',
                             ' <' = '<',
                             '[(](\\d+),\\s(\\d+)[)]' = '(\\1,\\2)',
                             ', n=' = ',',
                             '95%ci=' = '95%ci_low=',
                             ',,' = ', 95%ci_up=0,',
                             ', ' = ' ',
                             '=,' = '=0,',
                             '<,' = '<0,',
                             '>,' = '>0,',
                             '-,' = '-0,'
                             )) %>%
                  str_trim()) 



# podgląd danych

View(data)
dplyr::glimpse(data)
skimr::skim(data)
summarytools::dfSummary(data) %>%
  summarytools::view(., footnote = '', custom.css = 'tiny_text.css', table.classes = 'tiny-text')
kableExtra::kable_styling(knitr::kable(data), font_size = 8)



















