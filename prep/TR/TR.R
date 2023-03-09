pct_ref <- 90

scen_year <- layers$data$scenario_year

#Ordenar la tabla en formato largo
tr_sustainability_pat2021<-melt(tr_sustainability_pat2021, id.vars = c("rgn_id"))


tr_sustainability_pat2021<- rename(tr_sustainability_pat2021,s_score = "value")
tr_sustainability_pat2021<- rename(tr_sustainability_pat2021,year = "variable")
tr_sustainability_pat2021$year<- as.numeric(tr_sustainability_pat2021$year)

write.csv(tr_sustainability_pat2021, "comunas/layers/tr_sustainability_pat2021.csv")

## read in layers
tourism = SelectLayersData(layers, layers='tr_jobs_pct_tourism') %>%
  dplyr::select( rgn_id = "id_num", year, ep ="val_num")

sustain = SelectLayersData(layers, layers='tr_sustainability') %>%
  dplyr::select( rgn_id = "id_num", year, s_score ="val_num")

factor = SelectLayersData(layers, layers='tr_factor') %>%
  dplyr::select( rgn_id = "id_num", year, factor ="val_num")

tr_data  <-
  dplyr::full_join(tourism, sustain, by = c('rgn_id', 'year'))

tr_model <- tr_data %>%
  dplyr::mutate(E   = ep,
                S   = s_score,
                Xtr = E * S)  %>%
  select(rgn_id, year, Xtr)

tr_modelnew<-merge(tr_model, factor)

## Añadir el factor de corrección de turismo
tr_modelnew<- tr_modelnew  %>%
  dplyr::mutate(xtr = Xtr * factor)

## Punto de ref
p_ref<- tr_modelnew  %>%
  group_by(year) %>%
  dplyr::summarise(rgn_id, p_max = max(xtr), p_min = min(xtr))

## Scores
tr_scores<- merge(p_ref, tr_modelnew)
tr_scores<- tr_scores %>%
  mutate(status = c((xtr-p_min)/(p_max-p_min)) ) %>%
  mutate(status2 = c(xtr/p_max))%>%
  select(rgn_id, year, status, status2)


write.csv(tr_scores, "scores.csv", row.names = F)
# get status
tr_status <- tr_scores %>%
  dplyr::filter(year == scen_year) %>%
  dplyr::mutate(score = status * 100) %>%
  dplyr::select(region_id = "rgn_id", score) %>%
  dplyr::mutate(dimension = 'status')


# calculate trend
trend_years <- (scen_year - 4):(scen_year)
tr_trend <-
  CalculateTrend(status_data =tr_scores, trend_years = trend_years)


# bind status and trend by rows
tr_score <- dplyr::bind_rows(tr_status, tr_trend) %>%
  dplyr::mutate(goal = 'TR')


return(scores)
