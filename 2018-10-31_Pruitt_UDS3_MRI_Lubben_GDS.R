# 2018-10-31_Pruitt_UDS3_MRI_Lubben_GDS.R

# |---------------------------------------- ----
# | USEFUL VARS ----

`%>%` <- magrittr::`%>%`
source('~/Desktop/config.R')
source('local_config.R')


# |---------------------------------------- ----
# | USEFUL LIBRARIES ----

library(ggplot2)


# |---------------------------------------- ----
# | GET DATA ----


# | _ MRI Data ----

boxr::box_auth(client_id = BOX_CLIENT_ID, client_secret = BOX_CLIENT_SECRET)
# boxr::box_dl(file_id = 320718450581, overwrite = TRUE)
df_mri <- boxr::box_read_excel(file_id = file_id)
names(df_mri)
df_mri <- df_mri %>% 
  dplyr::filter(!is.na(UDS_ID)) %>% 
  dplyr::mutate(UDS_ID = as.integer(UDS_ID)) %>% 
  dplyr::mutate(Scan_Date = as.Date(Scan_Date))

df_mri <- df_mri %>% 
  dplyr::mutate(ptid = dplyr::case_when(
    nchar(UDS_ID) == 3 ~ paste0('UM00000', UDS_ID),
    nchar(UDS_ID) == 4 ~ paste0('UM0000', UDS_ID),
    TRUE ~ NA_character_
  )) %>% 
  dplyr::select(ptid, dplyr::everything()) %>% 
  dplyr::arrange(ptid, dplyr::desc(Scan_Date))

# | _ UDS 3 Data ----

fields_u3_id <- c('ptid'
                  , 'form_date')
fields_u3_dx_iv <- c('normcog'  # Normal cognition
                     , 'demented' # Demented
                     , 'mciamem'  # Amnestic MCI, single domain (aMCI SD) 
                     , 'mciaplus' # Amnestic MCI, multiple domains (aMCI MD)
                     , 'mcinon1'  # Non-amnestic MCI, single domain (naMCI SD)
                     , 'mcinon2'  # Non-amnestic MCI, multiple domains (naMCI MD)
                     , 'impnomci' # Cognitively impaired, not MCI
                     , 'alzdis'   # AD
                     , 'alzdisif' # AD: primary, contrib, non-contrib
                     , 'lbdis'    # LBD
                     , 'lbdif'
                     , 'park'     # Parkinson's
                     , 'msa'      # Multiple system atrophy
                     , 'msaif'
                     , 'psp'      # PSP
                     , 'pspif'
                     , 'cort'     # CBD
                     , 'cortif'
                     , 'ftldmo'   # FTLD w/ motor
                     , 'ftldmoif'
                     , 'ftldnos'  # FTLD NOS
                     , 'ftldnoif'
                     , 'cvd'      # Vascular brain injury
                     , 'cvdif'
                     , 'esstrem'  # Essential tremor
                     , 'esstreif'
                     , 'downs'    # Down syndrome
                     , 'downsif'
                     , 'hunt'     # Huntington's disease
                     , 'huntif'
                     , 'prion'    # Prion disease
                     , 'prionif'
                     , 'brninj'   # Traumatic brain injury
                     , 'brninjif'
                     , 'hyceph'   # Normal-pressure hydrocephalus
                     , 'hycephif'
                     , 'epilep'   # Epilepsy
                     , 'epilepif'
                     , 'neop'     # CNS neoplasm
                     , 'neopif'
                     , 'othcog'   # Cognitive impairment due to other condition
                     , 'othcogif')
fields_u3_dx_fv <- paste0('fu_', fields_u3_dx_iv) # add 'fu_' fields
fields_u3_lub <- c('isns_date'
                   , 'see_hear'
                   , 'private'
                   , 'help'
                   , 'friend_see_hear'
                   , 'friend_private'
                   , 'friend_help')
fields_u3_gds_iv <- c('nogds'
                      , 'satis'
                      , 'dropact'
                      , 'empty'
                      , 'bored'
                      , 'spirits'
                      , 'afraid'
                      , 'happy'
                      , 'helpless'
                      , 'stayhome'
                      , 'memprob'
                      , 'wondrful'
                      , 'wrthless'
                      , 'energy'
                      , 'hopeless'
                      , 'better'
                      , 'gds')
fields_u3_gds_fv <- paste0('fu_', fields_u3_gds_iv)

fields_u3_raw <- c(fields_u3_id, 
                   fields_u3_dx_iv, fields_u3_dx_fv,
                   fields_u3_gds_iv, fields_u3_gds_fv,
                   fields_u3_lub) 
fields_u3 <- fields_u3_raw %>% paste(collapse = ',')

json_u3 <- RCurl::postForm(
  uri=REDCAP_API_URI,
  token=REDCAP_API_TOKEN_UDS3,
  content='record',
  format='json',
  type='flat',
  fields=fields_u3,
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json'
)
df_u3 <- jsonlite::fromJSON(json_u3) %>% dplyr::na_if('')
df_u3 <- readr::type_convert(df_u3) # guess column types
df_u3 <- df_u3 %>% 
  dplyr::select(-(dplyr::ends_with('_complete')))

# | _ _ Clean out rows that only have NAs ----
all(is.na(df_u3[1, 3:ncol(df_u3)]))
na_rows <- integer(0)
for (i in seq_len(nrow(df_u3))) {
  if (all(is.na(df_u3[i, 3:ncol(df_u3)]))) { na_rows <- c(na_rows, i) }
}
# all(unlist(purrr::map(df_u3_na[, 3:ncol(df_u3_na)], is.na) %>% 
#              purrr::map(., all)))
data_rows <- seq_len(nrow(df_u3))[-na_rows]
df_u3 <- df_u3[data_rows, ]

# | _ MiNDSet Data ----

fields_ms_raw <- c('subject_id'
                   , 'exam_date'
                   , 'birth_date'
                   , 'sex_value'
                   , 'race_value'
                   , 'ed_level')
fields_ms <- fields_ms_raw %>% paste(collapse = ',')
json_ms <- RCurl::postForm(
  uri=REDCAP_API_URI,
  token=REDCAP_API_TOKEN_MINDSET,
  content='record',
  format='json',
  type='flat',
  fields=fields_ms,
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json'
)
df_ms <- jsonlite::fromJSON(json_ms) %>% dplyr::na_if('')
df_ms <- readr::type_convert(df_ms) # guess column types
purrr::map(df_ms, class)

# |---------------------------------------- ----
# | FILTER / COMPUTE ----

# | _ Get IDs of those with MRIs ----
mri_ids <- df_mri %>% dplyr::distinct(ptid) %>% dplyr::pull() %>% sort()

# | _ Clean out non-MRI partic.s from MiNDSet ----
df_ms_mri <- df_ms %>% 
  dplyr::filter(subject_id %in% mri_ids)

# | _ Rename df_ms_mri fields for later join ----
# | _ _ 'subject_id' to 'ptid'; 'exam_date' to 'form_date'
df_ms_mri <- df_ms_mri %>% 
  dplyr::rename(ptid = subject_id,
                form_date = exam_date)

# | _ Clean df_ms_mri `ed_level` field ----
sort(unique(df_ms_mri$ed_level))
df_ms_mri <- df_ms_mri %>% 
  dplyr::mutate(ed_level = dplyr::case_when(
    ed_level == 'Some college' ~ '13',
    ed_level == 'PhD' ~ '22',
    !is.na(ed_level) ~ ed_level,
    TRUE ~ NA_character_
  )) %>% 
  dplyr::mutate(ed_level = as.integer(ed_level))

# | _ Calculate age in MiNDSet ----
purrr::map(df_ms_mri, class)
df_ms_mri <- df_ms_mri %>% 
  dplyr::mutate(age = dplyr::case_when(
    !is.na(birth_date) & !is.na(form_date) ~ 
      round(difftime(form_date, birth_date)/365.25, 2)
  ))

# | _ Create human-readable fields in MiNDSet ----
df_ms_mri <- df_ms_mri %>% 
  dplyr::mutate(race_value_str = dplyr::case_when(
    race_value == 1 ~ 'White',
    race_value == 2 ~ 'Black',
    race_value == 5 ~ 'Other',
    !is.na(race_value) ~ as.character(race_value),
    TRUE ~ NA_character_
  ))
df_ms_mri <- df_ms_mri %>% 
  dplyr::mutate(sex_value_str = dplyr::case_when(
    sex_value == 1 ~ 'Male',
    sex_value == 2 ~ 'Female',
    !is.na(sex_value) ~ as.character(sex_value),
    TRUE ~ NA_character_
  ))

# | _ Clean out 'fu_'s from UDS 3 field names ----
df_u3_mri <- df_u3 %>% 
  dplyr::filter(ptid %in% mri_ids) %>% # clean out non-MRI partic.s
  dplyr::arrange(ptid, form_date)

df_u3_mri_iv <- df_u3_mri %>% 
  dplyr::filter(redcap_event_name == 'visit_1_arm_1') %>% 
  dplyr::select(dplyr::one_of(c(fields_u3_id,
                                fields_u3_dx_iv,
                                fields_u3_gds_iv,
                                fields_u3_lub)))

df_u3_mri_fv <- df_u3_mri %>% 
  dplyr::filter(redcap_event_name != 'visit_1_arm_1') %>% 
  dplyr::select(dplyr::one_of(c(fields_u3_id,
                                fields_u3_dx_fv,
                                fields_u3_gds_fv,
                                fields_u3_lub)))
names(df_u3_mri_fv)[3:61] <- 
  stringr::str_replace(names(df_u3_mri_fv[3:61]), 'fu_', '')

df_u3_mri_cln <- dplyr::bind_rows(df_u3_mri_iv, df_u3_mri_fv) %>% 
  dplyr::arrange(ptid, form_date)

# | _ UDS Dx simplification: NL, MCI, ImpNoMCI, AD, LBD, FTD, Other
df_u3_mri_cln <- df_u3_mri_cln %>% 
  dplyr::mutate(uds_dx = dplyr::case_when(
    normcog == 1  ~ 'NL',
    mciamem == 1  ~ 'MCI',
    mciaplus == 1 ~ 'MCI',
    mcinon1 == 1  ~ 'MCI',
    mcinon2 == 1  ~ 'MCI',
    impnomci == 1 ~ 'CogImp',
    alzdis == 1 & alzdisif == 1  ~ 'AD',
    lbdis == 1 & lbdif == 1      ~ 'LBD',
    park == 1                    ~ 'Other',
    msa == 1 & msaif == 1        ~ 'Other',
    psp == 1 & pspif == 1        ~ 'Other',
    cort == 1 & cortif == 1      ~ 'Other',
    ftldmo == 1 & ftldmoif == 1  ~ 'FTD',
    ftldnos == 1 & ftldnoif == 1 ~ 'FTD',
    cvd == 1 & cvdif == 1        ~ 'Other',
    esstrem == 1 & esstreif == 1 ~ 'Other',
    downs == 1 & downsif == 1    ~ 'Other',
    hunt == 1 & huntif == 1      ~ 'Other',
    prion == 1 & prionif == 1    ~ 'Other',
    brninj == 1 & brninjif == 1  ~ 'Other',
    hyceph == 1 & hycephif == 1  ~ 'Other',
    epilep == 1 & epilepif == 1  ~ 'Other',
    neop == 1 & neopif == 1      ~ 'Other',
    othcog == 1 & othcogif == 1  ~ 'Other',
    TRUE ~ NA_character_
  ))

# | _ Join MiNDSet-MRI data ----
df_u3_mri_cln <- dplyr::left_join(df_u3_mri_cln, df_ms_mri, 
                                  by = c('ptid', 'form_date'))

# | _ Total Lubbens scores ----
df_u3_mri_cln <- df_u3_mri_cln %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(lubben_total = dplyr::case_when(
    !is.na(see_hear) & !is.na(private) & !is.na(help) &
      !is.na(friend_see_hear) & !is.na(friend_private) & !is.na(friend_help) ~
      sum(see_hear, private, help,
          friend_see_hear, friend_private, friend_help),
    TRUE ~ NA_integer_
  ))

# | _ Total Lubbens scores <=12 / >12 dummy
df_u3_mri_cln <- df_u3_mri_cln %>% 
  dplyr::mutate(lubben_total_gt_12 = dplyr::case_when(
    lubben_total <= 12 ~ 'Lubben <= 12',
    lubben_total > 12 ~ 'Lubben > 12',
    TRUE ~ NA_character_
  ))

# | _ Total GDS scores (`gds`) <=5 / > 5 dummy
df_u3_mri_cln <- df_u3_mri_cln %>% 
  dplyr::mutate(gds_total_gt_5 = dplyr::case_when(
    gds <= 5 ~ 'GDS <= 5',
    gds > 5 ~ 'GDS > 5',
    TRUE ~ NA_character_
  ))


# |---------------------------------------- ----
# | CLEAN ---- 

# | _ Keep most recent visits only ----
df_u3_mri_cln <- df_u3_mri_cln %>% 
  dplyr::arrange(ptid, form_date) %>% 
  dplyr::distinct(ptid, .keep_all = TRUE)


# |---------------------------------------- ----
# | COMPUTE ---- 

# | _ Request counts by category ----

# | Counts by Dx
(n_dx <- df_u3_mri_cln %>% 
  dplyr::group_by(uds_dx) %>% 
  dplyr::summarize(n = n()))
readr::write_csv(n_dx, 'n_dx.csv', na = '')

# | Counts by Lubben, Lubben-Dx
(n_lub <- df_u3_mri_cln %>% 
  dplyr::group_by(lubben_total_gt_12) %>% 
  dplyr::summarize(n = n()))
(n_lub_dx <- df_u3_mri_cln %>% 
  dplyr::group_by(lubben_total_gt_12, uds_dx) %>% 
  dplyr::summarize(n = n()) %>% 
  tidyr::spread(key = uds_dx, value = n))
readr::write_csv(n_lub, 'n_lub.csv', na = '')
readr::write_csv(n_lub_dx, 'n_lub_dx.csv', na = '')

# | Counts by GDS, GDS-Dx
(n_gds <- df_u3_mri_cln %>% 
  dplyr::group_by(gds_total_gt_5) %>% 
  dplyr::summarize(n = n()))
(n_gds_dx <- df_u3_mri_cln %>% 
  dplyr::group_by(gds_total_gt_5, uds_dx) %>% 
  dplyr::summarize(n = n()) %>% 
  tidyr::spread(key = uds_dx, value = n))
readr::write_csv(n_gds, 'n_gds.csv', na = '')
readr::write_csv(n_gds_dx, 'n_gds_dx.csv', na = '')

# | Counts by sex, sex-Dx
(n_sex <- df_u3_mri_cln %>% 
  dplyr::group_by(sex_value_str) %>% 
  dplyr::summarize(n = n()))
(n_sex_dx <- df_u3_mri_cln %>% 
  dplyr::group_by(sex_value_str, uds_dx) %>% 
  dplyr::summarize(n = n()) %>% 
  tidyr::spread(key = uds_dx, value = n))
readr::write_csv(n_sex, 'n_sex.csv', na = '')
readr::write_csv(n_sex_dx, 'n_sex_dx.csv', na = '')

# | Counts by race, race-Dx
(n_rac <- df_u3_mri_cln %>% 
  dplyr::group_by(race_value_str) %>% 
  dplyr::summarize(n = n()))
(n_rac_dx <- df_u3_mri_cln %>% 
  dplyr::group_by(race_value_str, uds_dx) %>% 
  dplyr::summarize(n = n()) %>% 
  tidyr::spread(key = uds_dx, value = n))
readr::write_csv(n_rac, 'n_rac.csv', na = '')
readr::write_csv(n_rac_dx, 'n_rac_dx.csv', na = '')

# | Counts by ed_level, ed_level-Dx
(n_edu <- df_u3_mri_cln %>% 
  dplyr::group_by(ed_level) %>% 
  dplyr::summarize(n = n()))
(n_edu_dx <- df_u3_mri_cln %>% 
  dplyr::group_by(ed_level, uds_dx) %>% 
  dplyr::summarize(n = n()) %>% 
  tidyr::spread(key = uds_dx, value = n))
readr::write_csv(n_edu, 'n_edu.csv', na = '')
readr::write_csv(n_edu_dx, 'n_edu_dx.csv', na = '')

# | Counts by age, age-Dx
(n_age <- df_u3_mri_cln %>% 
  dplyr::group_by(round(as.numeric(age))) %>% 
  dplyr::summarize(n = n()))
(n_age_dx <- df_u3_mri_cln %>% 
  dplyr::group_by(round(as.numeric(age)), uds_dx) %>% 
  dplyr::summarize(n = n()) %>% 
  tidyr::spread(key = uds_dx, value = n))
readr::write_csv(n_age, 'n_age.csv', na = '')
readr::write_csv(n_age_dx, 'n_age_dx.csv', na = '')

# |---------------------------------------- ----
# | PLOTS ----

# | _ Bar plots in facet grid ----
# | _ _ Base plot
t <- ggplot(df_u3_mri_cln, aes(x = uds_dx)) + 
  geom_bar(fill = 'gray', color = 'black') +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.4))
# | Partic count by Dx
t
ggsave('partic_count.png', width = 6, height = 4.5, dpi = 150)
# | Partic count by Lubben Score
t + facet_grid(. ~ lubben_total_gt_12) + 
  ggtitle('No. Participants by Lubben Threshold (most recent dx)')
ggsave('partic_count_by_lubben.png', width = 6, height = 4.5, dpi = 150)
# | Partic count by GDS Score
t + facet_grid(. ~ gds_total_gt_5) + 
  ggtitle('No. Participants by GDS Threshold (most recent dx)')
ggsave('partic_count_by_gds.png', width = 6, height = 4.5, dpi = 150)
# | Partic count by sex
t + facet_grid(. ~ sex_value_str) + 
  ggtitle('No. Participants by Sex')
ggsave('partic_count_by_sex.png', width = 6, height = 4.5, dpi = 150)
# | Partic count by race
t + facet_grid(. ~ race_value_str) + 
  ggtitle('No. Participants by Race')
ggsave('partic_count_by_race.png', width = 6, height = 4.5, dpi = 150)

# Education level by Dx
ed_level_min <- min(df_u3_mri_cln$ed_level, na.rm = TRUE)
ed_level_max <- max(df_u3_mri_cln$ed_level, na.rm = TRUE)
u <- ggplot(df_u3_mri_cln, aes(x = ed_level)) +
  geom_histogram(bins = ed_level_max-ed_level_min+1, 
                 fill = 'gray', color = 'black') +
  scale_x_continuous(breaks = seq(ed_level_min, ed_level_max, by = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.4)) +
  ggtitle('Distribution of Education Level by UDS Dx')
u + facet_grid(. ~ uds_dx)
ggsave('ed_level_count_by_dx.png', width = 12, height = 9, dpi = 200)
# v <- ggplot(df_u3_mri_cln, aes(x = ed_level)) +
#   geom_density() +
#   ggtitle('Distribution of Education Level by UDS Dx')
# v + facet_grid(. ~ uds_dx)
# w <- ggplot(df_u3_mri_cln, aes(x = uds_dx, y = ed_level)) + 
#   geom_boxplot() +
#   scale_y_continuous(breaks = seq(ed_level_min-1, ed_level_max+1, by = 1),
#                      limits = c(ed_level_min-1, ed_level_max+1)) +
#   coord_flip() +
#   ggtitle('Distribution of Education Level by UDS Dx')
# w

# Age by Dx
age_min <- floor(min(as.numeric(df_u3_mri_cln$age), na.rm = TRUE))
age_max <- ceiling(max(as.numeric(df_u3_mri_cln$age), na.rm = TRUE))
v <- ggplot(df_u3_mri_cln, aes(x = age)) +
  geom_histogram(binwidth = 5, 
                 fill = 'gray', color = 'black') +
  scale_x_continuous(breaks = seq(50, 90, by = 5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.4)) +
  ggtitle('Distribution of Age by UDS Dx')
v + facet_grid(. ~ uds_dx)
ggsave('age_count_by_dx.png', width = 6, height = 4.5, dpi = 200)


