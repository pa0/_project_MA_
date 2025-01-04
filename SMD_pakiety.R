
# MAc/MAd - brakuje z, chi_d na około

MAc::t_to_d(t, n.1, n.2)[,'d']
MAc::f_to_d(f, n.1, n.2)[,'d']

MAc::mean_to_d(m.1, m.2, sd.1, sd.2, n.1, n.2)[,'d']  
MAc::mean_to_d2(m.1, m.2, s.pooled, n.1, n.2)[,'d']   # pooled SD

MAc::r_from_chi(chi.sq, n)
MAc::r_to_d(r, N)


MAd::t_to_d(t, n.1, n.2)
MAd::f_to_d(f, n.1, n.2)

MAd::mean_to_d(m.1, m.2, sd.1, sd.2, n.1, n.2)        
MAd::mean_to_d2(m.1, m.2, s.pooled, n.1, n.2)         # pooled SD

MAd::r_from_chi(chi.sq, n)
MAd::r_to_d(r, N) 

## compute.es - brakuje z

compute.es::tes(t, n.1, n.2, level = 95, cer = 0.2, dig = 2, verbose = TRUE, id=NULL, data=NULL)$d
compute.es::fes(f, n.1, n.2, level = 95, cer = 0.2, dig = 2, verbose = TRUE, id=NULL, data=NULL)$d       

compute.es::mes(m.1, m.2, sd.1, sd.2, n.1, n.2, level = 95, cer = 0.2, dig = 2, verbose = TRUE, id=NULL, data=NULL) 
compute.es::mes2(m.1, m.2, s.pooled, n.1, n.2, level = 95, cer = 0.2, dig = 2, verbose = TRUE, id=NULL, data=NULL)  # pooled SD

compute.es::chies(chi.sq, n, level = 95, cer = 0.2, dig = 2, verbose = TRUE, id=NULL, data=NULL)$d
compute.es::res(r, var.r = NULL, n, level = 95, cer = 0.2, dig = 2, verbose = TRUE, id=NULL, data=NULL)

# esc - brakuje z

esc::esc_t(t, p, totaln, grp1n, grp2n, es.type = c("d", "g", "or", "logit", "r", "f", "eta", "cox.or", "cox.log"), study = NULL, ...)$es
esc::esc_f(f, totaln, grp1n, grp2n, es.type = c("d", "g", "or", "logit", "r", "f", "eta", "cox.or", "cox.log"), study = NULL, ...)$es

esc::esc_mean_sd(grp1m, grp1sd, grp1n, grp2m, grp2sd, grp2n, totalsd, r, es.type = c("d", "g", "or", "logit", "r", "cox.or", "cox.log"), study = NULL)$es  
esc::esc_mean_se(grp1m, grp1sd, grp1n, grp2m, grp2se, grp2n, r, es.type = c("d", "g", "or", "logit", "r", "cox.or", "cox.log"), study = NULL)$es

esc::esc_chisq(chisq, p, totaln, es.type = c("d", "g", "or", "logit", "r", "f", "eta", "cox.or", "cox.log"), study = NULL)$es
esc::esc_phi(phi, p, totaln, es.type = c("d", "g", "or", "logit", "r", "f", "eta", "cox.or", "cox.log"), study = NULL)$es
esc::esc_rpb(r, p, totaln, grp1n, grp2n, es.type = c("d", "g", "or", "logit", "f", "eta", "cox.or", "cox.log"), study = NULL)$es     # point-biserial correlations 

# metaConvert - z tylko dla z Fishera

metaConvert::es_from_fisher_z(fisher_z, n_sample, unit_type = "raw_scale", n_exp, n_nexp, cor_to_smd = "viechtbauer", sd_iv, unit_increase_iv, reverse_fisher_z)$d     # tylko dla z Fishera

metaConvert::es_from_student_t(student_t, n_exp, n_nexp, smd_to_cor = "viechtbauer", reverse_student_t)$d
metaConvert::es_from_anova_f(anova_f, n_exp, n_nexp, smd_to_cor = "viechtbauer", reverse_anova_f)$d

metaConvert::es_from_means_sd(mean_exp, mean_sd_exp, mean_nexp, mean_sd_nexp, n_exp, n_nexp, smd_to_cor = "viechtbauer", reverse_means)$d
metaConvert::es_from_means_sd_pooled(mean_exp, mean_nexp, mean_sd_pooled, n_exp, n_nexp, smd_to_cor = "viechtbauer", reverse_means)$d
metaConvert::es_from_means_se(mean_exp, mean_se_exp, mean_nexp, mean_se_nexp, n_exp, n_nexp, smd_to_cor = "viechtbauer", reverse_means)$d
metaConvert::es_from_means_ci(mean_exp, mean_ci_lo_exp, mean_ci_up_exp, mean_nexp, mean_ci_lo_nexp, mean_ci_up_nexp, n_exp, n_nexp, smd_to_cor = "viechtbauer", max_asymmetry = 10, reverse_means)$d


metaConvert::es_from_chisq(chisq, n_sample, n_cases, n_exp, yates_chisq = FALSE, reverse_chisq)$d
metaConvert::es_from_phi(phi, n_cases, n_exp, n_sample, reverse_phi)$d
metaConvert::es_from_pearson_r(pearson_r, sd_iv, n_sample, n_exp, n_nexp, cor_to_smd = "viechtbauer", unit_increase_iv, unit_type = "raw_scale", reverse_pearson_r)$d
metaConvert::es_from_pt_bis_r(pt_bis_r, n_exp, n_nexp, smd_to_cor = "viechtbauer", reverse_pt_bis_r)$d   # point-biserial correlations

# effectsize - brakuje m_d, chi_d na około, tylko dla równych grup

effectsize::z_to_d(z, n, ...)$d                                                                              # n1 = n2, tak samo jak dla t w praktyce

effectsize::t_to_d(t, df_error, paired = FALSE, ci = 0.95, alternative = "two.sided", ...)$d                 # n1 = n2; df_error = 30
effectsize::F_to_d(f, df, df_error, ci = 0.95, alternative = "two.sided", ...)$d                             # n1 = n2; df = 1, df_error = 30

effectsize::chisq_to_phi(chisq, n, nrow=2, ncol=2, adjust = TRUE, ci = 0.95, alternative = "greater", ...)$phi
effectsize::chisq_to_cramers_v(chisq, n, nrow, ncol, adjust = TRUE, ci = 0.95, alternative = "greater", ...)$Cramers_v   # trzeba podać nrow, ncol 
effectsize::r_to_d(r, n1, n2, ...)                                                                                       # point-biserial correlations, n1 != n2


#################################################################################################################################################################################################################


## z

2 * 3.2 / sqrt(30) 
effectsize::z_to_d(3.2, 30)$d                                                   # efekt taki sam jak dla t, ale inne przedziały ufności

# 2 * tanh(3.2) / (9.083685 * sqrt(1 - tanh(3.2)**2))
# 1 * tanh(3.2) / (4.541842 * sqrt(1 - tanh(3.2)**2))
metaConvert::es_from_fisher_z(3.2, n_sample = 30)$d                                        
metaConvert::es_from_pearson_r(esc::convert_z2r(3.2), n_sample = 30)$d
metaConvert::es_from_pearson_r(tanh(3.2), n_sample = 30)$d

## t

2 * 3.2 / sqrt(30) # czemu nigdzie nie ma tego -2 ?
compute.es::tes(3.2, 15, 15, verbose = F, dig = 6)$d
esc::esc_t(3.2, totaln = 30)$es
MAc::t_to_d(3.2, 15, 15)[,'d']
MAd::t_to_d(3.2, 15, 15)[,'d']
metaConvert::es_from_student_t(3.2, 15, 15, smd_to_cor = 'viechtbauer')$d
metaConvert::es_from_student_t(3.2, 15, 15, smd_to_cor = 'lipsey_cooper')$d
effectsize::t_to_d(3.2, 30)$d

## f

2 * sqrt(3.2) / sqrt(30)
compute.es::fes(3.2, 15, 15, verbose = F, dig = 7)$d
esc::esc_f(3.2, totaln = 30)$es
MAc::f_to_d(3.2, 15, 15)[,'d']
MAd::f_to_d(3.2, 15, 15)[,'d']
metaConvert::es_from_anova_f(3.2, 15, 15, smd_to_cor = 'viechtbauer')$d
metaConvert::es_from_anova_f(3.2, 15, 15, smd_to_cor = 'lipsey_cooper')$d
effectsize::F_to_d(3.2, 1, 30)$d

## chi

# esc, es(na około) - 0.6910947
sqrt(3.2/30)
2 * abs(sqrt(3.2/30)) / sqrt(1 - (3.2/30))
2 * MAd::r_from_chi(3.2, 30) / sqrt(1 - MAd::r_from_chi(3.2, 30)**2)
2 * effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi / sqrt(1 - effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi**2)
esc::esc_chisq(3.2, totaln = 30)$es                                 
esc::esc_phi(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, totaln = 30)$es 
esc::esc_rpb(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, totaln = 30)$es # gdy są podane nierówne grupy wyniki są inne
compute.es::res(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, n = 30, verbose = F, dig = 7)$d

# effectsize(na około) - 0.667661
n1 = n2 = 15
h = (n1 + n2 - 2) / n1 + (n1 + n2 - 2) / n2                                     # zmodyfikowane h, dostosowane do n1 != n2
h
4
r = effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi
sqrt(4) * r / sqrt(1 - r**2)
sqrt(h) * r / sqrt(1 - r**2)
effectsize::r_to_d(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, 15, 15)   
metaConvert::es_from_pt_bis_r(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, 15, 15)$d

# es, MAd, MAc - 0.6794789
2 * abs(sqrt(3.2/30)) / sqrt(1 - 3.2/30) * sqrt((30 - 1)/30)                    # zmodyfikowane r->d (* sqrt((n - 1)/n)
compute.es::chies(3.2, 30, verbose = F, dig = 7)$d                               
MAc::r_to_d(MAc::r_from_chi(3.2, 30), 30)                                       
MAd::r_to_d(MAd::r_from_chi(3.2, 30), 30)
MAd::r_to_d(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, 30)

# metaConvert - 0.7643041, 0.5548455(?)
metaConvert::es_from_chisq(3.2, 30, 15, 15, yates_chisq = F)$d                               # (*) - d nie z phi, a z log(OR)
metaConvert::es_from_phi(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, 15, 15, 30)$d    # (*) 

metaConvert::es_from_pearson_r(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, n_sample = 30, n_exp = 15, n_nexp = 15)$d    # https://en.wikipedia.org/wiki/Delta_method; https://rdrr.io/cran/metafor/man/conv.delta.html

# różne, z poprawką Yate - 0.5389365, 0.5578523, 0.5578523, 0.7643041
effectsize::r_to_d(effectsize::chisq_to_phi(3.2, 30, adjust = T)$phi, 15, 15)   
esc::esc_phi(effectsize::chisq_to_phi(3.2, 30, adjust = T)$phi, totaln = 30)$es              # różnica wynika z innego r->d (inne h)
esc::esc_rpb(effectsize::chisq_to_phi(3.2, 30, adjust = T)$phi, totaln = 30)$es 
metaConvert::es_from_chisq(3.2, 30, 15, 15, yates_chisq = T)$d                               # różnica wynika z innego podejścia; korekta nie wpłynęła (**)    

## m, sd unpooled - 0.7844645

(3 - 2)/sqrt((1**2 + 1.5**2)/2)
compute.es::mes(3, 2, 1, 1.5, 15, 15, verbose = F, dig = 7)$d
MAc::mean_to_d(3, 2, 1, 1.5, 15, 15)[,'d']
MAd::mean_to_d(3, 2, 1, 1.5, 15, 15)[,'d']
metaConvert::es_from_means_sd(3, 1, 2, 1.5, 15, 15)$d
esc::esc_mean_sd(3, 1, 15, 2, 1.5, 15)$es

## m, sd pooled - 0.5714286

(3 - 2)/1.75
compute.es::mes2(3, 2, 1.75, 15, 15, verbose = F, dig = 7)$d
MAc::mean_to_d2(3, 2, 1.75, 15, 15)[,'d']
MAd::mean_to_d2(3, 2, 1.75, 15, 15)[,'d']
metaConvert::es_from_means_sd_pooled(3, 2, 1.75, 15, 15)$d
esc::esc_mean_sd(grp1m = 3, grp1n = 15, grp2m = 2, grp2n = 15, grp1sd = 1.75, grp2sd = 1.75)$es  # pooled sd można sprowadzić do sd_pooled = sd_1 = sd_2 

(3 - 2)/sqrt((1.75**2 * (30 - 1) - ((1**2 * 15 * 15) / 30)) / (30 - 1))                          # Wilson, 2016; https://github.com/strengejacke/esc/blob/master/R/esc_mean_sd.R
                                                                                                 # Wilson, 2017, str.12; gdy powyższy mianownik ujemny, to użyty jest ten wzór: (totalsd^2 * (totaln - 1) - ((grp1m^2 + grp2m^2 - 2 * grp1m * grp2m) / totaln)) / totaln
esc::esc_mean_sd(grp1m = 3, grp1n = 15, grp2m = 2, grp2n = 15, totalsd = 1.75)$es                # total sd to sd całej próby (c(grp1, grp2)); gdy nie znamy sd_1, sd_2

## m, se - 0.209657

(3 - 2)/sqrt(((1 * sqrt(15 - 1))**2 + (1.5 * sqrt(15 - 1))**2)/2)
esc::esc_mean_se(3, 1, 15, 2, 1.5, 15)$es
compute.es::mes(3, 2, 1 * sqrt(15 - 1), 1.5 * sqrt(15 - 1), 15, 15, verbose = F, dig = 7)$d
MAc::mean_to_d(3, 2, 1 * sqrt(15 - 1), 1.5 * sqrt(15 - 1), 15, 15)[,'d']
MAd::mean_to_d(3, 2, 1 * sqrt(15 - 1), 1.5 * sqrt(15 - 1), 15, 15)[,'d']

(3 - 2)/sqrt(((1 * sqrt(15))**2 + (1.5 * sqrt(15))**2)/2)                       # zmodyfikowane n (n - 1->n)
metaConvert::es_from_means_se(3, 1, 2, 1.5, 15, 15)$d                   


###  gdy n1 != n2:

## z

metaConvert::es_from_fisher_z(3.2, n_exp = 13, n_nexp = 17)$d                                        
metaConvert::es_from_pearson_r(esc::convert_z2r(3.2), n_exp = 13, n_nexp = 17)$d
metaConvert::es_from_pearson_r(tanh(3.2), n_exp = 13, n_nexp = 17)$d

## t

3.2 * sqrt(1/13 + 1/17)
compute.es::tes(3.2, 13, 17, verbose = F, dig = 6)$d
esc::esc_t(3.2, grp1n = 13, grp2n = 17)$es
MAc::t_to_d(3.2, 13, 17)[,'d']
MAd::t_to_d(3.2, 13, 17)[,'d']
metaConvert::es_from_student_t(3.2, 13, 17, smd_to_cor = 'viechtbauer')$d
metaConvert::es_from_student_t(3.2, 13, 17, smd_to_cor = 'lipsey_cooper')$d

2 * 3.2 / sqrt(30)
effectsize::t_to_d(3.2, 30)$d

## f

sqrt(3.2 * (1/13 + 1/17))
compute.es::fes(3.2, 13, 17, verbose = F, dig = 7)$d
esc::esc_f(3.2, grp1n = 13, grp2n = 17)$es
MAc::f_to_d(3.2, 13, 17)[,'d']
MAd::f_to_d(3.2, 13, 17)[,'d']
metaConvert::es_from_anova_f(3.2, 13, 17, smd_to_cor = 'viechtbauer')$d
metaConvert::es_from_anova_f(3.2, 13, 17, smd_to_cor = 'lipsey_cooper')$d

2 * sqrt(3.2) / sqrt(30)
effectsize::F_to_d(3.2, 1, 30)$d

## chi

# esc
r = effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi
n1 = 13; n2 = 17
(n1 + n2)/n1 + (n1 + n2)/n2
h = 1/(n1*n2/(n1 + n2)**2)
h
4
sqrt(h) * r / sqrt(1 - r**2)                                                    #  zmodyfikowane h, dostosowane do n1 != n2
esc::esc_rpb(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, grp1n = 13, grp2n = 17)$es 

# effectsize
h = (n1 + n2 - 2) / n1 + (n1 + n2 - 2) / n2                                     # z -2 przy n
h
4
sqrt(h) * r / sqrt(1 - r**2)                                                    
effectsize::r_to_d(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, 13, 17)  
metaConvert::es_from_pt_bis_r(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, 13, 17)$d

# metaConvert
metaConvert::es_from_chisq(3.2, 30, 13, 17, yates_chisq = F)$d                               # (*) - d nie z phi, a z log(OR) - jedyna opcja tutaj przeznaczona w teorii do phi, a nie point biserial correlations, ew. r Pearsona
metaConvert::es_from_phi(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, 13, 17, 30)$d    # (*) 

metaConvert::es_from_pearson_r(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, n_sample = 30, n_exp = 13, n_nexp = 17)$d    

## m, sd unpooled 

(3 - 2)/sqrt((1**2 * (13 - 1) + 1.5**2 * (17 - 1))/(13 + 17 - 2))               # sd pooled jest dopiero obliczane
compute.es::mes(3, 2, 1, 1.5, 13, 17, verbose = F, dig = 7)$d
MAc::mean_to_d(3, 2, 1, 1.5, 13, 17)[,'d']
MAd::mean_to_d(3, 2, 1, 1.5, 13, 17)[,'d']
metaConvert::es_from_means_sd(3, 1, 2, 1.5, 13, 17)$d
esc::esc_mean_sd(3, 1, 13, 2, 1.5, 17)$es

## m, sd pooled

(3 - 2)/1.75                                                                    # d się nie zmieni dla sd pooled, gdy n1 != n2, ale wariancje d i inne już tak
compute.es::mes2(3, 2, 1.75, 13, 17, verbose = F, dig = 7)$d
MAc::mean_to_d2(3, 2, 1.75, 13, 17)[,'d']
MAd::mean_to_d2(3, 2, 1.75, 13, 17)[,'d']
metaConvert::es_from_means_sd_pooled(3, 2, 1.75, 13, 17)$d
esc::esc_mean_sd(grp1m = 3, grp1n = 13, grp2m = 2, grp2n = 17, grp1sd = 1.75, grp2sd = 1.75)$es  # pooled sd można sprowadzić do sd_pooled = sd_1 = sd_2 

(3 - 2)/sqrt((1.75**2 * (30 - 1) - ((1**2 * 13 * 17) / 30)) / (30 - 1))                          # Wilson, 2016; https://github.com/strengejacke/esc/blob/master/R/esc_mean_sd.R
# Wilson, 2017, str.12; gdy powyższy mianownik ujemny, to użyty jest ten wzór: (totalsd^2 * (totaln - 1) - ((grp1m^2 + grp2m^2 - 2 * grp1m * grp2m) / totaln)) / totaln
esc::esc_mean_sd(grp1m = 3, grp1n = 13, grp2m = 2, grp2n = 17, totalsd = 1.75)$es                # total sd to sd całej próby (c(grp1, grp2)); gdy nie znamy sd_1, sd_2

## m, se 

(3 - 2)/sqrt(((1 * sqrt(13 - 1))**2  * (13 - 1) + (1.5 * sqrt(17 - 1))**2 * (17 - 1))/(13 + 17 - 2))
compute.es::mes(3, 2, 1 * sqrt(13 - 1), 1.5 * sqrt(17 - 1), 13, 17, verbose = F, dig = 7)$d
MAc::mean_to_d(3, 2, 1 * sqrt(13 - 1), 1.5 * sqrt(17 - 1), 13, 17)[,'d']
MAd::mean_to_d(3, 2, 1 * sqrt(13 - 1), 1.5 * sqrt(17 - 1), 13, 17)[,'d']
esc::esc_mean_se(3, 1, 13, 2, 1.5, 17)$es

(3 - 2)/sqrt(((1 * sqrt(13))**2  * (13 - 1) + (1.5 * sqrt(17))**2 * (17 - 1))/(13 + 17 - 2))  # zmodyfikowane n (n - 1->n)
metaConvert::es_from_means_se(3, 1, 2, 1.5, 13, 17)$d



### Podsumowując:
#
#   Najlepsze ogółem jest esc, dla "z" najlepsze jest effectsize, a jeśli chcemy konwersję "z Fishera", to tylko z metaConvert.
#   Gdy są nierówne próby, to "z" lepiej zastąpić przez "t" z esc (bo effectsize nie pozwala na nierówne grupy).
#                             "chi" można liczyć z esc dla ujednolicenia, ale to effectsize jest dokładniejsze - ma n z odjętymi stopniami swobody (które w esc wszędzie są pominięte),
#                             a "z Fishera" dalej można tylko z metaConvert.



######################################################################################################################################################################################



# działanie metaConvert::es_from_chisq/metaConvert::es_from_phi (*)

metaConvert::es_from_chisq(3.2, 30, 15, 15)$d   
metaConvert::es_from_phi(effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi, 15, 15, 30)$d


phi = effectsize::chisq_to_phi(3.2, 30, adjust = F)$phi
n_exp = n_cases = 15
n_sample = 30

cont_table = metafor::conv.2x2(ri = phi, ni = n_sample, n1i = n_exp, n2i = n_cases)
cont_table2 = metafor::conv.2x2(x2i = 3.2, ni = n_sample, n1i = n_exp, n2i = n_cases)
cont_table; cont_table2
sum((cont_table - 7.5)**2)/7.5                                                                    # lekko różne od 3.2 (bo dopasowane do możliwych wartości w tabeli krzyżowej (całkowitych))
2 * abs(sqrt(sum((cont_table - 7.5)**2)/7.5/30)) / sqrt(1 - (sum((cont_table - 7.5)**2)/7.5/30))  # lekko różne od es_from_phi; różnica jest też dalej - w es_from_2x2


metaConvert::es_from_2x2(n_cases_exp = cont_table$ai,
                         n_controls_exp = cont_table$bi,
                         n_cases_nexp = cont_table$ci,
                         n_controls_nexp = cont_table$di)

a = cont_table$ai
b = cont_table$bi
c = cont_table$ci
d = cont_table$di
n1 = a + b
n2 = c + d

or = n_cases_exp * n_controls_nexp /(n_controls_exp * n_cases_nexp)                               # d nie jest liczone z phi, a z log(OR); wzór jest m.in. w Wilson, 2017, str. 19
log(or) * sqrt(3)/pi



# działanie metafor::conv.2x2

# a - bez korekty

chi = 3.2
phi = effectsize::chisq_to_phi(chi, 30, adjust = F)$phi

metafor::conv.2x2(x2i = chi, ni = n_sample, n1i = n_exp, n2i = n_cases)
metafor::conv.2x2(ri = phi, ni = n_sample, n1i = n_exp, n2i = n_cases)


n1 = n2 = 15
n = 30
p1 = p2 = 0.5
p = p1 * p2 + phi * sqrt(p1 * (1 - p1) * p2 * (1 - p2))                         # wzór jest m.in. w Wilson, 2017, str. 18
p1 * p2 + sqrt(3.2/n) * sqrt(p1 * (1 - p1) * p2 * (1 - p2))     
p

a = round(n * p)
b = n1 - a
c = n2 - a                                                                      # jeśli n1 = n2, to rozwiązanie będzie symetryczne
d = n - a - b - c
c(a, b, c, d)

sum((c(a, b, c, d) - 7.5)**2)/7.5 

# a - z korektą (**)

metaConvert::es_from_chisq(3.2, 30, 15, 15, yates_chisq = T)$d   

Yate = n/(2 * sqrt(n1 * (n - n1) * n2 * (n - n2)))
p_Yate = p1 * p2 + (phi + Yate) * sqrt(p1 * (1 - p1) * p2 * (1 - p2))           # korekta jest przy phi
p_Yate

a = round(n * p_Yate)                                                           # tu jest zaokrąglenie, a korekta była zbyt mała, żeby zaokrąglenie było inne
b = n1 - a
c = n2 - a
d = n - a - b - c
c(a, b, c, d)

# b

a = 8
b = 21
c = 1
d = 13

metafor::conv.2x2(x2i = (a + b + c + d) * (abs(a * d - b * c))^2 / ((a + c) * (b + d) * (a + b) * (c + d)),
                  ni = a + b + c + d, n1i = a + b, n2i = a + c)

# c

chi = 3.2

metafor::conv.2x2(x2i = chi, ni = 13, n1i = 7, n2i = 5, correct = F)
metafor::conv.2x2(x2i = chi, ni = 13, n1i = 7, n2i = 5, correct = T)

metafor::conv.2x2(ri = sqrt(chi/13), ni = 13, n1i = 7, n2i = 5, correct = F)
metafor::conv.2x2(ri = sqrt(chi/13), ni = 13, n1i = 7, n2i = 5, correct = T)    # jeśli poda się phi, a nie chi, to nie uwzględnia korekty







