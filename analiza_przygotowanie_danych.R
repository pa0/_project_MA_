# wczytanie danych
data = readxl::read_xlsx('dane/Meta 2.1.xlsx', sheet = 3)

# usunięcie pustych kolumn
data = data %>%
  janitor::remove_constant(na.rm = TRUE)      

# zmiana nazw kolumn 
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

#View(data)
#dplyr::glimpse(data)
#skimr::skim(data)
#summarytools::dfSummary(data) %>%
#  summarytools::view(., footnote = '', custom.css = '_extensions/tiny_text.css', table.classes = 'tiny-text')
#kableExtra::kable_styling(knitr::kable(data), font_size = 8)


# -------------- transformacja danych ----------------------------------------#
# dane o artykułach (czyszczenie + dodanie ważnych kolumn)

data = data %>%
  mutate(id = 1:nrow(.),
         bibitem = if_else(!(str_sub(bibitem, 1, 2) %in% c('A.', 'B.')), bibitem, str_sub(bibitem, 4, -1L)), .before = 1) %>%
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


# -------------- transformacja danych ----------------------------------------#
# zmiana typów zmiennych ilościowych

data = data %>%
  mutate_at(vars(c(year,
                   prosocial,
                   cooperative,
                   multiplayer,
                   starts_with('n'))), as.integer) %>%
  mutate(SMD = as.numeric(str_replace(SMD, ',', '.'))) %>%
  relocate(effect_coef, .before = 'effect_size')

# dane ilościowe

#data %>% janitor::tabyl(sd_prosoc)
#data %>% janitor::tabyl(sd_con)
#data %>% janitor::tabyl(m_con)
#data %>% janitor::tabyl(m_prosoc)

data = data %>%
  mutate(sd_prosoc = str_replace(sd_prosoc, '=,', '=0,')) %>% 
  mutate_at(vars(starts_with('sd_')),  ~str_remove_all(.x, '95%ci=')) %>%
  mutate_at(vars(matches('^sd_|^m_')), ~as.numeric(str_replace_all(.x, ',', '.')))


# -------------- transformacja danych ----------------------------------------#
# etykietowanie zmiennych jakościowych
# dane jakościowe

#data %>% janitor::tabyl(plan) 
#data %>% janitor::tabyl(game_type) 
#data %>% janitor::tabyl(compared_game_type) 

# wyniki

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

#data %>% janitor::tabyl(group) 

# -------------- transformacja danych ----------------------------------------#
# czyszczenie testów statystycznych

#data %>% janitor::tabyl(test) 

data = data %>% 
  mutate(test = na_if(test, 's') %>%
           str_to_lower() %>%
           str_remove_all('\\[|\\]') %>%
           str_replace_all(
             c('[.]' = ',',
               ';' = ',',
               'chi2' = 'chi',
               ' [(]' = '(',
               '\n' = ' ',
               ' +' = ' ',
               ' ,' = ',',
               '= ' = '=',
               ' =' = '=',
               '> ' = '>',
               ' >' = '>',
               '< ' = '<',
               ' <' = '<',
               '[(](\\d+),(\\d+)[)]' = '(\\1;\\2)',
               '[(](\\d+),\\s(\\d+)[)]' = '(\\1;\\2)',
               ', n=' = ';',
               '95%ci=' = 'ci95%_low=',
               ',,' = ', ci95%_up=0,',
               ', ' = ' ',
               '=,' = '=0,',
               '<,' = '<0,',
               '>,' = '>0,',
               '-,' = '-0,',
               ',' = '.',
               ';' = ',',
               'se=(\\d+),(\\d+)  p<(\\d+),(\\d+)' = 'p<(\\d+),(\\d+) se=(\\d+),(\\d+)')) %>%
           str_remove_all('\\([^)]*\\)') %>%
           str_trim() %>%
           str_squish() %>%
           str_remove('[[:punct:]]+$')) %>%
  mutate(test = ifelse(cite == 'eriksson2021behavioral_a', str_c(rev(str_split(.[cite == 'eriksson2021behavioral_a', 'test'], ' ')[[1]]), collapse = ' '), test))



# podgląd danych

#View(data)
#dplyr::glimpse(data)
#skimr::skim(data)
#summarytools::dfSummary(data) %>%
#  summarytools::view(., footnote = '', custom.css = 'tiny_text.css', table.classes = 'tiny-text')
#kableExtra::kable_styling(knitr::kable(data), font_size = 8)


# -------------- transformacja danych ----------------------------------------#
# rozwinięcie testów statystycznych (do dalszej obróbki)

data = data %>%
  mutate(
    test_list =
      lapply(lapply(test, function(x) {
        y = str_split(x, ' ')
        y = structure(y[[1]], names = c(sapply(y[[1]], function(z)
          case_when(
            str_sub(z, 1, 2) %in% c('p=', 'p<', 'p>') ~ 'p',
            str_sub(z, 1, 2) %in% c('z=', 't(', 't=', 'f(', 'f=', 'ch') |
              str_sub(z, 1, 9) == 'ci95%_low' ~ 'stat',
            str_sub(z, 1, 8) == 'ci95%_up' ~ 'stat_2',
            str_sub(z, 1, 2) %in% c('se', 'sd') ~ 'se',
            # PK: ale że se == sd?
            str_sub(z, 1, 2) == 'd=' ~ 'd',
            TRUE ~ 'o'
          ))))
        y}), function(w) w[order(factor(names(w), levels = c('p', 'stat', 'stat_2', 'se', 'd', 'o')))]),
    test_list_c =
      lapply(lapply(test, function(x) {
        y = str_split(x, ' ')
        y = structure(y[[1]], names = c(sapply(y[[1]], function(z)
          if_else(
            str_sub(z, 1, 2) %in% c('p=','p<','p>','z=','t(','t=','f(','f=','ch','ci','se','sd','d='),
            str_sub(z, 1, 1), 'o' ))))
        y
      }), function(w)
        w[order(factor(names(w), levels = c('p', 'z', 't', 'f', 'c', 's', 'd', 'o')))]),
    test_list_c = lapply(test_list_c, function(x) {
      y = str_split(x, '[<=>]')
      sapply(y, function(z)
        structure(
          ifelse(length(z) == 2, as.numeric(z[2]), z[1]),
          names = ifelse(length(z) == 2, z[1], 'o')
        ))
    }),
    test_list_o = lapply(test_list, function(x) {
      y = str_split(x, '[<=>]')
      structure(sapply(y, function(z)
        ifelse(length(z) == 2, as.numeric(z[2]), z[1])), names = names(x))
    })
  ) %>%
  mutate(test_list_copy = test_list) %>%
  unnest_wider(test_list, names_sep = '.', names_repair = 'universal') %>%
  rename(test_list = test_list_copy, test_list.d = test_list.d...38) %>% select(-test_list.d...39) %>%
  relocate(str_c('test_list.', c('p', 'stat', 'stat_2', 'se', 'd', 'o')), .after = 'test') %>%
  mutate(
    across(starts_with('test_list.'), function(x) {
      y = str_split(x, '[<=>]')
      sapply(y, function(z)
        structure(
          ifelse(length(z) == 2, as.numeric(z[2]), z[1]),
          names = ifelse(length(z) == 2, z[1], 'o')
        ))
    }),
    across(starts_with('test_list.') &
             !ends_with('.o'), function(x)
               structure(as.numeric(x), names = names(x)))
  )

# można by jeszcze uwzględnić w p: =,>,<


# ============================================================================#
# ----------------- wielkości efektów ----------------------------------------#

data = data %>%
  relocate(c(n, n_prosoc, n_con), .after = test_list)

# 0 - n = n1 + n2, n1 = n2
# 1 - n = n1 + n2, n1 != n2 
# 2 - n = n1, n2 = NA
# 3 - n != n1 + n2 
# 4 - n1 = n2 = NA

# do przypadku 3
data = data %>%
  mutate(across(starts_with('n_'), ~ ifelse(str_starts(cite, 'tountopoulou2021training'), n/2, .)))     # z artykułu wynika, że raczej n1 = n2 = n/2

# do przypadku 4
data = data %>%
  mutate(across(starts_with('n_'), ~ ifelse(cite == 'shaza2021impact_a', n / 2, .))) %>%
  rows_update(
    data.frame(
      cite = 'charewicz2019effects_b',
      n = 103,
      n_prosoc = round(103 * 51 / 106),
      n_con = round(103 * 55 / 106)
    ),
    by = 'cite'
  ) # z df w tekście wynika, że odrzucono 3 osoby, ale nie z których grup, więc zakładam proporcjonalnie

# #pozostałe przypadki 4 są całe testy i statystyki do uzupełnienia#

# dla przypadku 2
data = data %>%
  mutate(
    test_list.stat = structure(
      ifelse(
        cite == 'morschheuser2017how_a',
        unname(effect_size / ((test_list.stat_2 - effect_size) / qt(0.975, n - 3)
        )),
        test_list.stat
      ),
      # chyba dobry test jest (β = 0.476, p < 0.001 (i chyba bet jest 3), ale tu brakuje SE
      names = ifelse(cite == 'morschheuser2017how_a', 't', names(test_list.stat))
    ),
    test_list.stat_2 = NULL,
    n_con = ifelse(!is.na(n_con) & is.na(n_prosoc), NA, n_con)
  )

# ogólne poprawy
data = data %>%
  mutate(
    n = case_when(
      #cite == 'wu2019promoting'              ~ 131,     # PK dlaczego? grały tylko osoby z 2 grup 28+34
      cite == 'lobel2016relation_a'          ~ 184,
      cite == 'parsons2019prosocial_a'       ~ 71,
      cite == 'parsons2018validation_a'      ~ 76,
      cite == 'parsons2018validation_b'      ~ 109,
      cite == 'jin2017when_c'                ~ 96,
      cite == 'badatala2016effects_a'        ~ 30,
      cite == 'kaufman2015psychologically_c' ~ 32,
      TRUE                                   ~ n
    )
  ) %>%
  rows_update(data.frame(
    cite = c(
      'jin2017when_c',
      'badatala2016effects_a',
      'kaufman2015psychologically_a',
      'kaufman2015psychologically_c',
      'boduszek2019prosocial_a',
      'boduszek2019prosocial_b'
    ),
    n_con     = c(48, 15, 18, 16, 86, 86),
    n_prosoc  = c(48, 15, 19, 16, 86, 86)
  ), by = 'cite') %>%
  rows_update(data.frame(
    cite      = c('shaza2021impact_a', 'kaufman2015psychologically_c'),
    m_con     = (53.66 + 55.45) / 2,
    sd_con    = (18.14 + 19.16) / 2,
    m_prosoc  = (63.46 + 67.62) / 2,
    sd_prosoc = c((20.83 + 18.08) / 2, 1.09)
  ),
  by = 'cite') %>%
  mutate(
    test_list.se   = if_else(cite == 'wu2019promoting_a', c('se' = 0.07), test_list.se),
    test_list.stat = structure(
      case_when(
        cite == 'wu2019promoting_a' |
        cite == 'shoshani2021video_a'      ~ effect_size / test_list.se,
        cite == 'lobel2016relation_a'      ~ sign(effect_size) * qt(1 - test_list.p / 2, n - 9),
        cite == 'eriksson2021behavioral_a' ~ -2.94,
                                           ~ test_list.stat
      ),
      names = ifelse(
        cite %in% c(
          'wu2019promoting_a',
          'shoshani2021video_a',
          'lobel2016relation_a',
          'eriksson2021behavioral_a'
        ),
        't',
        names(test_list.stat)
      )
    )
  )

# ============================================================================#
# ------------------ wszystkie ES --------------------------------------------#

# dla przypadków 0,1,2 (w tym 3,4) 
data = data %>%
  rowwise() %>%
  mutate(
    test_stat = case_when(effect_coef == 'r' ~ 'r', !is.na(test_list.stat) ~ names(test_list.stat)),
    SMD_R     = case_when(
      test_stat == 'r'                                    ~ effectsize::r_to_d(effect_size, n1 = n),
      if_all(matches('(_con|_prosoc)$'), ~ !is.na(.x))    ~ metaConvert::es_from_means_sd(m_prosoc, sd_prosoc, m_con, sd_con, n_prosoc, n_con, smd_to_cor = "viechtbauer")$d,
      if_all(matches('^(m|sd)(.*)(_prosoc|_con)$'),       ~ !is.na(.x)) ~ metaConvert::es_from_means_sd(m_prosoc, sd_prosoc, m_con, sd_con, n, n)$d,
      n_con == n_prosoc & test_stat == 'z'                ~ effectsize::z_to_d(test_list.stat, n = n)$d,
      n_con == n_prosoc & test_stat == 't'                ~ esc::esc_t(test_list.stat, totaln = n)$es,
      n_con == n_prosoc & test_stat == 'f'                ~ esc::esc_f(test_list.stat, totaln = n)$es,
      n_con == n_prosoc & test_stat == 'chi'              ~ esc::esc_chisq(test_list.stat, totaln = n)$es,
      n_con != n_prosoc & test_stat == 't'                ~ esc::esc_t(test_list.stat, grp1n = n_prosoc, grp2n = n_con)$es,
      n_con != n_prosoc & test_stat == 'f'                ~ esc::esc_f(test_list.stat, grp1n = n_prosoc, grp2n = n_con)$es,
      n_con != n_prosoc & test_stat == 'chi'              ~ esc::esc_rpb(effectsize::chisq_to_phi(abs(test_list.stat), n = n, adjust = F)$phi, grp1n = n_prosoc,grp2n = n_con)$es,
      is.na(n_con) & is.na(n_prosoc) & test_stat == 'chi' ~ effectsize::oddsratio_to_d(max(c((38 + 14) / (23 + 10)), (23 + 10) / (38 + 14))), # z OR = max(b/c, c/b) z tabeli krzyżowej
      TRUE ~ NA_real_
    ),
    SMD_R = ifelse( cite == 'beene2015effect_a' | cite == 'gordon2018powering_a', NA, SMD_R)           # PK: dlaczego tu kasujesz efekty?
  ) %>%
  relocate(SMD, SMD_calc, .after = 'SMD_R')


cor.test(abs(data$SMD_R), abs(data$SMD))       # korelacja 0.93
#cor.test(abs(data$SMD_R), abs(data$SMD_calc))

# usuwam niepotrzebne już (?) SMD z excela
data$SMD <- NULL
data$SMD_calc <- NULL


data <- data %>% 
  rowwise() %>%
  mutate(SMD_R_var = case_when(test_stat == 'r'                                            ~ 4/((1 - effect_size**2) * (n - 1)),                                         
                               if_all(matches('(_con|_prosoc)$'), ~ !is.na(.x))            ~ (n_con + n_prosoc)/(n_con * n_prosoc) + SMD_R**2/2/n,
                               if_all(matches('^(m|sd)(.*)(_prosoc|_con)$'), ~ !is.na(.x)) ~ 2/n + SMD_R**2/(4 * n),                               
                               n_con != n_prosoc & test_stat == 't'                        ~ (n_con + n_prosoc)/(n_con * n_prosoc) + SMD_R**2/2/n,
                               n_con != n_prosoc & test_stat == 'f'                        ~ (n_con + n_prosoc)/(n_con * n_prosoc) + SMD_R**2/2/n,            
                               n_con != n_prosoc & test_stat == 'chi'                      ~ (n_con + n_prosoc)/(n_con * n_prosoc) + SMD_R**2/2/n, # ? - kor. potraktowana punkt.-dwuser., więc typowy wzór chyba adekwatniejszy niż z chi
                               n_con == n_prosoc & test_stat == 'z'                        ~ (n_con + n_prosoc)/(n_con * n_prosoc) + SMD_R**2/2/n,
                               n_con == n_prosoc & test_stat == 't'                        ~ (n_con + n_prosoc)/(n_con * n_prosoc) + SMD_R**2/2/n,     
                               n_con == n_prosoc & test_stat == 'f'                        ~ (n_con + n_prosoc)/(n_con * n_prosoc) + SMD_R**2/2/n,                                   
                               n_con == n_prosoc & test_stat == 'chi'                      ~ SMD_R**2/abs(test_list.stat),
                               is.na(n_con) & is.na(n_prosoc) & test_stat == 'chi'         ~ (1/(38 + 14) + 1/(23 + 10)) * 3/pi**2,
                               TRUE ~ NA))

data = data %>% 
  mutate(SMD_var  = case_when(
  # test_stat == 'r'                                    ~ 4/((1 - effect_size**2) * (n - 1)), # powtórzone?
    n_con == n_prosoc & test_stat == 'chi'              ~ SMD_R**2/abs(test_list.stat),
    is.na(n_con) & is.na(n_prosoc) & test_stat == 'chi' ~ (1/(38 + 14) + 1/(23 + 10)) * 3 / pi**2,
    !is.na(n_con) & !is.na(n_prosoc)                    ~ (n_con + n_prosoc)/(n_con * n_prosoc) + SMD_R**2/(2 * n),
    !is.na(SMD_R)                                       ~ 2/n + SMD_R**2/(4 * n),
    TRUE ~ NA),
    SMD_se     = sqrt(SMD_var),
    W_var      = 1 / SMD_var,
    W_n        = case_when(
      !is.na(n_con) & !is.na(n_prosoc) ~ n,
      is.na(n_con) & is.na(n_prosoc)   ~ n * 2),           # ? - mogę tu tak zignorować, że to zależne? [nieistotne - i tak tego nie używam]
    SMD_W_var  = SMD_R * W_var,
    SMD_W_n    = SMD_R * W_n,                  # PK: o co chodzi z tym W?
    SMD2_W_var = SMD_R**2 * W_var,
    SMD2_W_n   = SMD_R**2 * W_n,
    W2_var     = W_var**2,
    W2_n       = W_n**2
    ) 

cor.test(abs(data$SMD_var), abs(data$v))               # korelacja 0.58


# PK: Proszę usuń robocze i już zbędne kolumny przed zapisaniem
save(data, file = "./dane/data.Rda")


############ cross_check ##########
# ---------- r
data |> 
  select(effect_size, effect_coef, n, SMD, SMD_R) |> 
  dplyr::filter(effect_coef == "r") %>%
  mutate(
    SMD2 = effectsize::r_to_d(r = effect_size, n1 = n)
  )
# ---------- m,sd,n_1/2
data |> 
  select(n_prosoc, n_con, m_prosoc, m_con, sd_prosoc, sd_con, SMD_R) |> 
  na.omit() %>%
  escalc(n1i = n_prosoc, n2i = n_con,
         m1i = m_prosoc, m2i = m_con,
         sd1i = sd_prosoc, sd2i = sd_con, 
         measure = "SMD", data = .) |>
  mutate(SMD2 = round(yi,digits = 8)) |> select (-yi, -vi)

# --------- z testu 
data |> 
  dplyr::filter(n_con == n_prosoc & test_stat == 't') |> 
  select(effect_size, test, n, test_list.stat, SMD) |> 
  mutate(
    SMD2 = esc::esc_t(test_list.stat, totaln = n)$es
  )

  
