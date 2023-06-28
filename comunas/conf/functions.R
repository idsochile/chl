## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, FIS is the Fishing subgoal of Food Provision (FP).


FIS <- function(layers) {
    scen_year <- layers$data$scenario_year
    ### ESTADO DE BASE  2017- 2021 ###

    # Xfis= estado de las pesquerias pescadas en la naturaleza
    # SS= puntuaciones de estado de las pesquerias
    # B/Bmsy=indicador para informar SS
    # C= contribucion de la poblacion a la captura total


    ### from script FUNCTIONS global de OHI core

  #catch data
  c <-SelectLayersData(layers, layers='fis_meancatch') %>%
    dplyr::select( rgn_id = "id_num", Spp = "category", year, catch ="val_num")

  #  b_bmsy data

    b <- SelectLayersData(layers, layers='fis_b_bmsy') %>%
      dplyr::select(rgn_id = "id_num", year,  Spp = "category", b_bmsy ="val_num")

  # The following stocks are fished in multiple regions and often have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.  The following corrects this problem:
   # tmp <- dplyr::filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Sardinella_aurita-34')) %>%
   #   dplyr::arrange(stock_id, year) %>%
   #   data.frame()

    alpha <- 0.5
    beta <- 0.25
    lowerBuffer <- 0.95
    upperBuffer <- 1.05

    b$score = ifelse(
      b$b_bmsy < lowerBuffer,
      b$b_bmsy,
      ifelse (b$b_bmsy >= lowerBuffer &
                b$b_bmsy <= upperBuffer, 1, NA)
    )
    b$score = ifelse(!is.na(b$score),
                     b$score,
                     ifelse(
                       1 - alpha * (b$b_bmsy - upperBuffer) > beta,
                       1 - alpha * (b$b_bmsy - upperBuffer),
                       beta
                     ))


  ### from script FUNCTIONS global de OHI core
  ####
  # STEP 1. Merge the b/bmsy data with catch data
  ####

  data_fis <- c %>%
    dplyr::left_join(b, by = c('rgn_id', 'Spp', 'year')) %>%
    dplyr::select(rgn_id, Spp, year, catch, b_bmsy, score)


  ###
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Median score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  ###

  ## this takes the mean score within each region and year
  ##
  data_fis_gf <- data_fis %>%
    dplyr::group_by(rgn_id, year) %>%
    dplyr::mutate(mean_score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup()


  ## this takes the mean score across all regions within a year
  # (when no stocks have scores within a region)

  data_fis_gf2 <- data_fis_gf %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(mean_score_global = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mean_score = ifelse(!is.na(score), score, mean_score_global))

  data_fis_gf3 <- data_fis_gf2 %>%
    mutate(mean_score = ifelse(!is.na(score), score, mean_score_global))

  ### step 2.1
  ### Calcular las especies capturadas por region y año

  sp<- c %>%
    group_by(rgn_id, year) %>%
    dplyr::summarise(n = n())


  ### step 3. seleccionamos aquellas columnas que nos interesan
  #adaptacion CHL
  status_data <- data_fis_gf3 %>%
    dplyr::select(rgn_id, Spp, year, catch, mean_score)

  ###
  # STEP 4. Calculate status for each region
  ###

  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year
  ## adapt to pat
  status_data <- status_data %>%
    dplyr::group_by(year, rgn_id) %>%
    dplyr::mutate(SumCatch = sum(catch)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(wprop = catch / SumCatch) %>%
    left_join(sp, by = c("rgn_id", "year"))

  status_data$mean_score<- as.numeric(status_data$mean_score)

  ### condición por b_bmsy
  ### si el numero de especies capturados por region en bajo
  ### el b_bmsy de las especies es castigado por la sensibilidad de la pesquera de la comuna
  buffer_n <- 3
  buffer_w <- 0.4
  gama <- 0.3

  status_data$mean_score_f1<-  ifelse(status_data$n == buffer_n,
                                   status_data$mean_score - c(gama*status_data$mean_score),
                                   status_data$mean_score)

  buffer_n <- 2
  buffer_w <- 0.4
  gama <- 0.4

  status_data$mean_score_f2<-  ifelse(status_data$n == buffer_n ,
                                      status_data$mean_score_f1  - c(gama*status_data$mean_score_f1),
                                      status_data$mean_score_f1)

  buffer_n <- 1
  buffer_w <- 0.4
  gama <- 0.5

  status_data$mean_score_final<-  ifelse(status_data$n == buffer_n,
                                      status_data$mean_score_f2 - c(gama*status_data$mean_score_f2),
                                      status_data$mean_score_f2)




  status_data_final <- status_data %>%
    dplyr::group_by(rgn_id, year) %>%
    dplyr::summarize(status = prod(na.omit(mean_score_final ^ wprop)))   %>%
    dplyr::ungroup()




  ###
  # STEP 5. Get yearly status and trend
  ###

  status <-  status_data_final %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(score     = round(status * 100, 100),
           dimension = 'status') %>%
    dplyr::select(region_id = "rgn_id", score, dimension)


  # calculate trend

  trend_years <- (scen_year - 4):(scen_year)

  trend <-
    CalculateTrend(status_data = status_data_final, trend_years = trend_years)

  trend <- trend[!is.na(trend$score),]

  # assemble dimensions
  scores <- data.frame()
  scores <- rbind(status, trend) %>%
    dplyr::mutate(goal = 'FIS')

  return(scores)
}


MAR <- function(layers) {

  scen_year <- layers$data$scenario_year

  mar_sust <- SelectLayersData(layers, layers='mar_sustainability_scores') %>%
    dplyr::select( rgn_id = "id_num",  species = "category", sust_coeff ="val_num")


  mar_harvest <- SelectLayersData(layers, layers='mar_harvest_tonnes') %>%
    dplyr::select(rgn_id = "id_num",species = "category", year, tonnes = "val_num")


  c1<- merge(mar_harvest, mar_sust)

  # 4-year rolling mean of data
  c2 <- c1 %>%
    dplyr::group_by(rgn_id, species) %>%
    dplyr::arrange(rgn_id, species, year) %>%
    dplyr::mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm = TRUE, partial =
                                               TRUE, align = "right")) %>%
    dplyr::ungroup()

  ##Punto de referencia
  pto_ref<- c2 %>% group_by(rgn_id, species) %>%
    dplyr::summarise(pto_max = max(sm_tonnes))%>%
    dplyr::group_by(rgn_id) %>%
    dplyr::summarise(pto_ref = sum(pto_max)) %>%
    dplyr::mutate(Punto_ref = pto_ref*0.01) %>%
    dplyr::select(rgn_id, Punto_ref)

  ##Para calcular el estado
  c3<- c2 %>%
    dplyr::filter(year %in% c(2017:2021)) %>%
    dplyr::mutate(mult = sm_tonnes* sust_coeff) %>%
    dplyr::group_by(rgn_id, year) %>%
    dplyr::mutate(YC = sum(mult)) %>%
    dplyr::select(rgn_id, year, YC)

  c3<-c3[!duplicated(c3), ]


  c4<- merge(c3, pto_ref)

  status<-c4 %>%
    dplyr::mutate(status = YC/Punto_ref) %>%
    dplyr::select(rgn_id, year, status)

  # status
  status_a <- status %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id = rgn_id, score = status, dimension)


  trend_years <- (scen_year - 4):(scen_year)

  trend <- CalculateTrend(status_data = status, trend_years = trend_years)


  # return scores
  score = rbind(status_a, trend) %>%
    dplyr::mutate(goal = 'MAR')

  scores<- rbind(score,scores)

  return(scores)
}


FP <- function(layers, scores) {

  scen_year <- layers$data$scenario_year

   w <- SelectLayersData(layers, layers = "fp_wildcaught_weight") %>%
    dplyr::select(region_id = id_num, w_fis = val_num, year)
  w <- w[!is.na(w$w_fis),]



  # scores
  s <- scores %>%
    dplyr::filter(goal %in% c('FIS', 'MAR')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    dplyr::left_join(w, by = "region_id")  %>%
    dplyr::mutate(w_mar = 1 - w_fis) %>%
    dplyr::mutate(weight = ifelse(goal == "FIS", w_fis, w_mar))


  ## Some warning messages due to potential mismatches in data:
  ## In the future consider filtering by scenario year so it's easy to see what warnings are attributed to which data
  # NA score but there is a weight


  s <- s  %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "FP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


AO <- function(layers) {
  scen_year <- layers$data$scenario_year

  gini<-
    AlignDataYears(layer_nm = "ao_coef_gini", layers_obj = layers) %>%
    dplyr::select(rgn_id,  year = ao_coef_gini_year, Gini_inv)

  Sust_artes<-
    AlignDataYears(layer_nm = "ao_coef_sust", layers_obj = layers) %>%
    dplyr::select(rgn_id,  year = ao_coef_sust_year, Sustcoef_norm)

  RPA<-
    AlignDataYears(layer_nm = "ao_rpa", layers_obj = layers) %>%
    dplyr::select(rgn_id,  year = ao_rpa_year, normperct)

  #Calculo de status

  OA_data_final <- merge(RPA, gini, by.x=c("rgn_id", "year"), by.y=c("rgn_id","year"), all=T)

  OA_data_final_sust <- merge(OA_data_final, Sust_artes, by.x=c("rgn_id","year"), by.y=c("rgn_id","year"), all=T)

  OA_data_final2 <- OA_data_final_sust  %>%
    mutate(rollmeanBirths=(zoo::rollapply(normperct,5, mean, na.rm=T, partial=T)),
           rollmeanGini=(zoo::rollapply(Gini_inv,5, mean, na.rm=T, partial=T)),
           Xao=((rollmeanGini+rollmeanBirths+Sustcoef_norm)/3))


  #Para calcular la tendencia

  AO_2017_2021 <- OA_data_final2 %>%
    filter(year %in% (2017:2021)) %>%
    dplyr::rename(region_id = "rgn_id", status = "Xao") %>%
    dplyr::select(region_id,year,status)

  r.status <-  AO_2017_2021 %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(score     = round(status * 100, 1),
                  dimension = 'status') %>%
    dplyr::select(region_id, score, dimension)

  # trend


  trend_years <- (scen_year - 4):(scen_year)

  r.trend <-
    CalculateTrend(status_data = AO_2017_2021, trend_years = trend_years)


  # return scores
  scores <- rbind(r.status, r.trend) %>%
    dplyr::mutate(goal = 'AO')

  return(scores)
}

NP <- function(layers) {

  scen_year <- layers$data$scenario_year


  ### Reassembles NP harvest information from separate data layers:
  ### [rgn_name  rgn_id  product  year  tonnes  tonnes_rel  prod_weight]
  #########################################.

  ## load data from layers dataframe
  h_tonnes <-
    SelectLayersData(layers, layers =  "np_harvest_tonnes") %>%
    dplyr::select(year, rgn_id = id_num, Producto = category, tonnes = val_num)

  h_tonnes_rel <- SelectLayersData(layers, layers =  "np_harvest_tonnes_relative") %>%
    dplyr::select(year, rgn_id = id_num, Producto = category, tonnes_rel = val_num)

  h_tonnes_w <- SelectLayersData(layers, layers =  "np_harvest_tonnes_weigth") %>%
    dplyr::select(year, rgn_id = id_num, Producto = category, proportion = val_num)


  np_fofm <- SelectLayersData(layers, layers =  "np_fofm_scores") %>%
    dplyr::select(year, rgn_id = id_num, Producto = category, coef = val_num)

  np_seaweed <- SelectLayersData(layers, layers =  "np_seaweed_sust") %>%
    dplyr::select(year, rgn_id = id_num, Producto = category, coef = val_num)


  # merge harvest in tonnes and usd
  np_harvest<- left_join(h_tonnes, h_tonnes_w,
                         by= c("rgn_id", "year", "Producto"))

  np_harvest<- left_join(np_harvest,  h_tonnes_rel,
                         by =c("rgn_id", "year", "Producto"))

  #Sustentabilidad
  np_sust<- rbind(np_fofm, np_seaweed)

  ##Merge
  np_harvest<- merge(np_harvest, np_sust)

  ## Calcular el estado de cada producto

  np_status_all = np_harvest %>%
    mutate(Pc = tonnes_rel * coef)

  ##
  np_status_all = np_status_all %>%
    filter(!is.na(coef) & !is.na(proportion)) %>%
    group_by(rgn_id, year) %>%
    dplyr::summarize(status = weighted.mean(Pc* proportion)*100) %>%
    filter(!is.na(status)) %>%
    ungroup()



  ### Calculates NP status for all production years for each region, based
  ### upon weighted mean of all products produced.
  ### Reports scenario year as the NP status.
  ### Calculates NP trend for each region, based upon slope of a linear
  ### model over the past five years
  ### Returns data frame with status and trend by region:
  ### [goal   dimension   region_id   score]
  #########################################.


  ### get current status
  np_status_current <- np_status_all %>%
    filter(year == scen_year & !is.na(status)) %>%
    mutate(dimension = 'status',
           score     = round(status, 4)) %>%
    select(region_id = rgn_id, dimension, score)


  ### trend

  trend_years <- (scen_year - 4):(scen_year)

  np_trend <-
    CalculateTrend(status_data = np_status_all, trend_years = trend_years)

  ### return scores
  np_scores <- np_status_current %>%
    full_join(np_trend, by = c('region_id', 'dimension', 'score')) %>%
    mutate(goal = 'NP') %>%
    select(goal, dimension, region_id, score) %>%
    arrange(goal, dimension, region_id)


  return(np_scores)
}


CS <- function(layers) {
  scen_year <- layers$data$scenario_year

  cs = SelectLayersData(layers, layers='cs_habitat_extent') %>%
    dplyr::select( rgn_id = "id_num",  habitat = "category", year, km2 ="val_num")

  p_ref = SelectLayersData(layers, layers='cs_habitat_pref') %>%
    dplyr::select( rgn_id = "id_num",  habitat = "category", p_ref ="val_num")

  area = SelectLayersData(layers, layers='rgn_area') %>%
    dplyr::select( rgn_id = "id_num",  area_km2 ="val_num")



##Eliminar los NA
  cs <- cs[!is.na(cs$km2),]


##weigth
  coef<- data.frame(habitat = c("Kelp forest" , "Seagrass", "Tidal flats"),
                    w = c(133.1, 138, 129.8))

  wt<- mean(coef$w)
##Calculo del score
  cs_b<- area %>%
    mutate(B= area_km2 * wt)

  cs_scores<- cs %>%
    dplyr::filter(km2 > 0) %>%
    dplyr::left_join(p_ref, by = c("rgn_id", "habitat")) %>%
    dplyr::mutate(h = km2 / p_ref) %>%
    group_by(rgn_id, year, habitat) %>%
    left_join(coef, by = "habitat") %>%
    dplyr::summarise(A = c(h*w*km2)) %>%
    dplyr::group_by(rgn_id, year)%>%
    dplyr::summarise(A = sum(A))%>%
    left_join(cs_b, by = "rgn_id") %>%
    dplyr::mutate(status = (A/B)*100)


  ## Estado actual
  cs_status<- cs_scores %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::select(region_id ="rgn_id" , score = status) %>%
    dplyr::mutate(dimension = 'status')

  # trend
  trend_years <- (scen_year - 4):(scen_year)
  cs_trend <- CalculateTrend(status_data = cs_scores,
                             trend_years = trend_years) %>%
      dplyr::mutate(dimension = 'trend')


  cs_score <- dplyr::bind_rows(cs_status, cs_trend) %>%
    dplyr::mutate(goal = 'CS')%>%
    dplyr::select(goal, dimension, region_id, score)


  # return scores
  return(cs_score)
}



CP <- function(layers) {

  ## read in layers
  scen_year <- layers$data$scenario_year

  # layers for coastal protection
  extent_lyrs <-
    c('cp_habitat')

  health_lyrs <-
    c('cp_habitat_health')
  trend_lyrs <-
    c('cp_habitat_trend')


  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs)  %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))

  d<- merge(extent, health)

  trend <- AlignManyDataYears(trend_lyrs) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))

  d<- merge(d, trend)

  ##Rankeo del habitat
  habitat.rank <- data.frame(habitat = c('Macrocystis',  'Marismas' ,  'Playas' ,  'Tepu'),
                    rank =c(1,  1, 1, 1)  )

  d<- merge(d, habitat.rank)

  ##scores
  scores_CP <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) /
                                    (sum(sum(extent) * rank, na.rm = TRUE
                                    ))) * 100) %>%
    dplyr::mutate(dimension = 'status') %>%
    ungroup()

  d_trend = d %>%
    filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
  if (nrow(d_trend) > 0 ){
    scores_CP <- dplyr::bind_rows(
      scores_CP,
      d_trend %>%
        group_by(region_id) %>%
        dplyr::summarize(
          score = sum(rank * trend * extent, na.rm = TRUE) / (sum(extent * rank, na.rm =
                                                                    TRUE)),
          dimension = 'trend'))
  }

  scores_CP<- scores_CP %>%
    dplyr::mutate(goal = 'CP') %>%
    dplyr::select(goal, dimension, region_id, score)



  # return scores
  return(scores_CP)

}

TR <- function(layers) {
  scen_year <- layers$data$scenario_year


  ## read in layers
  tourism <-
    AlignDataYears(layer_nm = "tr_jobs_pct_tourism", layers_obj = layers) %>%
    dplyr::select(-layer_name)
  sustain <-
    AlignDataYears(layer_nm = "tr_sustainability", layers_obj = layers) %>%
    dplyr::select(-layer_name)
  factor <-
    AlignDataYears(layer_nm = "tr_factor", layers_obj = layers) %>%
    dplyr::select(rgn_id, factor)

  tr_data  <-
    dplyr::full_join(tourism, sustain, by = c('rgn_id', 'scenario_year'))

  tr_model <- tr_data %>%
    dplyr::mutate(E   = ep,
                  S   = s_score,
                  Xtr = E * S)  %>%
    select(rgn_id, year= "scenario_year", Xtr)

  tr_modelnew<-merge(tr_model, factor)

  ## Añadir el factor de corrección de turismo
  tr_modelnew<- tr_modelnew  %>%
    dplyr::mutate(xtr = Xtr * factor)

  ## Punto de ref
  p_ref<- tr_modelnew  %>%
    group_by(year) %>%
    dplyr::summarise(rgn_id,
                     p_max= quantile(xtr, prob=seq(0, 1, length = 11), probs = 0.9),
                     p_min= quantile(xtr, prob=seq(0, 1, length = 11), probs = 0))

  ## Scores
  tr_scores<- merge(p_ref, tr_modelnew)
  tr_scores<- tr_scores %>%
    mutate(status =pmin((xtr-p_min)/(p_max-p_min),1)) %>%
    select(rgn_id, year, status)


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

  # return final scores
  scores <- tr_score %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)
}



LIV<- function(layers) {

  le_wages = SelectLayersData(layers, layers='le_wage_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, wage_usd = val_num)

  le_jobs  = SelectLayersData(layers, layers='le_jobs_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, jobs = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)


  ## multipliers from Table S10 (Halpern et al 2012 SOM)
  multipliers_jobs = data.frame('sector' = c('Turismo','Pesca','Acuicultura', 'Alojamiento','Transporte'),
                                'multiplier' = c(1, 1.582, 2.7, 1,1))
  ## multipler not listed for tour (=1)

  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) /100 ,
           employed            = jobs_all * proportion_employed)

  liv =
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector')          %>%
    mutate(jobs_mult = jobs * multiplier)               %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('rgn_id', 'year'))     %>%
    mutate(jobs_adj = jobs_mult * proportion_employed)  %>% # adjust jobs by proportion employed
    left_join(le_wages, by=c('rgn_id','year','sector')) %>%
    arrange(year, sector, rgn_id)

  # LIV calculations ----

  # LIV status
  liv_status1 = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd))
  liv_status = liv_status1 %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    arrange(rgn_id, year, sector) %>%
    # summarize across sectors
    group_by(rgn_id, year) %>%
    dplyr :: summarise(
      # across sectors, jobs are summed
      jobs_sum  = sum(jobs_adj, na.rm=T),
      # across sectors, wages are averaged
      wages_avg = mean(wage_usd, na.rm=T)) %>%
    group_by(rgn_id) %>%
    arrange(rgn_id, year) %>%
    mutate(
      # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
      jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
      # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
      wages_avg_first = first(wages_avg)) %>% # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
    # calculate final scores
    ungroup() %>%
    mutate(
      x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
      x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
      score   = ((x_jobs + x_wages) / 2)*100  ,na.rm=T ) %>%
    # filter for most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    dplyr::select(
      region_id = rgn_id,
      score) %>%
    mutate(
      goal      = 'LIV',
      dimension = 'status')

  ## LIV trend ----

  # get trend across years as slope of individual sectors for jobs and wages
  liv_trend = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # get sector weight as total jobs across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(jobs_adj, na.rm=T)) %>%
    # reshape into jobs and wages columns into single metric to get slope of both
    reshape2::melt(id=c('rgn_id','year','sector','weight'), variable='metric', value.name='value') %>%
    mutate(
      sector = as.character(sector),
      metric = as.character(metric)) %>%
    # get linear model coefficient per metric
    group_by(metric, rgn_id, sector, weight) %>%
    do(mdl = lm(value ~ year, data=.)) %>%
    dplyr::summarize(
      metric = metric,
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    arrange(rgn_id, metric, sector) %>%
    # get weighted mean across sectors per region-metric
    group_by(metric, rgn_id) %>%
    dplyr::summarize(
      metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # get mean trend across metrics (jobs, wages) per region
    group_by(rgn_id) %>%
    dplyr::summarize(
      score = mean(metric_trend, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'LIV',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)


  ## create scores and rbind to other goal scores
  scores_liv = rbind(liv_status, liv_trend) %>%
    dplyr::select(region_id,
                  score,
                  dimension,
                  goal)
  # return all scores
  return(scores_liv)
}

ECO<- function(layers) {


  ## read in data layers
  le_gdp   = SelectLayersData(layers, layers='le_gdp')  %>%
    dplyr::select(rgn_id = id_num, year, gdp_usd = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)


  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  # ECO calculations ----
  eco = le_gdp %>%
    mutate(
      rev_adj = gdp_usd,
      sector = 'gdp') %>%
    # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
    dplyr::select(rgn_id, year, sector, rev_adj)

  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(rgn_id, year) %>%
    dplyr::summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(rgn_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    dplyr::summarize(
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(rgn_id) %>%
    dplyr::summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  ## create scores and rbind to other goal scores
  scores_eco = rbind(eco_status, eco_trend) %>%
    dplyr::select(region_id,
                  score,
                  dimension,
                  goal)



  # return all scores
  return(scores_eco)

}

LE = function(scores, layers){

  # calculate LE scores
  s <- scores %>%
    dplyr::filter(goal %in% c('LIV', 'ECO'),
                  dimension %in% c('status', 'trend', 'future', 'score')) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "LE") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return scores
  return(rbind(scores, s))
}

ICO <- function(layers) {
  scen_year <- layers$data$scenario_year

  lyr1 = SelectLayersData(layers, layers='ico_spp_status') %>%
    dplyr::select( region_id = "id_num", Specie, status ="val_chr")

  lyr2 = SelectLayersData(layers, layers='ico_spp_trend') %>%
    dplyr::select( region_id = "id_num", Specie, trend ="val_chr")

  rk<- merge(lyr1, lyr2)


  # lookup for weights status
  w.risk_category = c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)

  # lookup for population trend
  w.popn_trend = c('Decreciendo' = -0.5,
                   'Estable'     =  0,
                   'Creciendo' =  0.5)

  # status
  r.status = data.frame(ddply(rk, .(region_id), function(x){
    mean(1 - w.risk_category[x$status], na.rm=T) * 100 }))


  # trend
  r.trend = data.frame(ddply(rk, .(region_id), function(x){
    mean(w.popn_trend[x$trend], na.rm=T) }))

  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))

  scores <- cbind(rbind(s.status, s.trend), data.frame('goal'='ICO'))  %>%
      dplyr::select(region_id, goal,dimension, score = V1) %>%data.frame()

  return(scores)

}

LSP <- function(layers) {
  scen_year <- layers$data$scenario_year

  ref_pct_cmpa <- 40
  ref_pct_cp <- 40

  # select data
  offshore = SelectLayersData(layers, layers='lsp_prot_area_offshore3mn') %>%
    dplyr::select( region_id = "id_num",  year, cmpa ="val_num")

  inland = SelectLayersData(layers, layers='lsp_prot_area_inland1mn') %>%
    dplyr::select( region_id = "id_num",  year, cp="val_num")

#######
  lsp_data <- full_join(offshore, inland, by = c("region_id", "year"))

############
  status_data <- lsp_data %>%
    mutate(status    = (pmin(cmpa/ ref_pct_cp, 1) + pmin(cp / ref_pct_cmpa, 1)) / 2) %>%
    filter(!is.na(status))


  # Score actual

  r.status<- status_data %>%
    filter(year == scen_year) %>%
    select(region_id, score = "status") %>%
    dplyr::mutate(dimension = "status")
  r.status$score<- r.status$score*100


  # calculate trend

  trend_years <- (scen_year - 4):(scen_year)

  r.trend <-
    CalculateTrend(status_data = status_data, trend_years = trend_years) %>%
    dplyr::mutate(dimension = "trend")

  # return scores
  scores1 <- dplyr::bind_rows(r.status, r.trend) %>%
    mutate(goal = "LSP")

  return(scores[, c('region_id', 'goal', 'dimension', 'score')])
}

SP <- function(scores) {
  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    dplyr::filter(goal %in% c('ICO', 'LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "SP") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


CW <- function(layers) {

  scen_year <- layers$data$scenario_year

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }


  # layers
  prs = c('po_pathogen' = 'a',
          'po_nutrients_3nm' = 'u',
          'po_chemical' = 'l',
          'po_trash'     = 'd',
          'po_pathogens_fan' = 'f')

  trends<-c('cw_nutrient_trend'  = 'fert_trend',
            'cw_coastalpopn_trend' = 'popn_trend',
            'cw_pathogen_trend'    = 'path_trend')


# para calcular la media de los patogenos por saneamiento y por fan
  p = SelectLayersData(layers, layers=names(prs))
  pres_data1<-  p %>% filter(layer %in% c('po_pathogen', 'po_pathogens_fan')) %>%
    group_by(id_num) %>%
    dplyr::summarise(value= mean(val_num)) %>%
    select(region_id = id_num, value )

#Agregar todos los datos
  pres_data<- p %>% filter(layer %in% c('po_nutrients_3nm' , 'po_chemical',
                                        'po_trash' )) %>%
    select(region_id = id_num, value = val_num) %>%
    rbind(pres_data1)

  t = SelectLayersData(layers, layers=names(trends))
  trend_data<- t %>% select(region_id = id_num, value = val_num)
  # get data together:
#  prs_data <- AlignManyDataYears(prs_lyrs) %>%
#    dplyr::filter(scenario_year == scen_year) %>%
#    dplyr::select(region_id = rgn_id, value = pressure_score)

  d_pressures <- pres_data %>%
    dplyr::mutate(pressure = 1 - value) %>%  # invert pressure
    dplyr::mutate(pressure = ifelse(pressure == 0 , pressure + 0.01, pressure)) %>% # add small modifier to zeros to
    dplyr::group_by(region_id) %>%                                                  # prevent zeros with geometric mean
    dplyr::summarize(score = geometric.mean2(pressure, na.rm = TRUE)) %>% # take geometric mean
    dplyr::mutate(score = score * 100) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::ungroup()


  # get trend data together:
#  trend_data <- AlignManyDataYears(trend_lyrs) %>%
#    dplyr::filter(scenario_year == scen_year) %>%
#    dplyr::select(region_id = rgn_id, value = trend)

  d_trends <- trend_data %>%
    dplyr::mutate(trend = -1 * value)  %>%  # invert trends
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = mean(trend, na.rm = TRUE)) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::ungroup()


  # return scores
  scores <- rbind(d_pressures, d_trends) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()


  return(scores)
}


HAB <- function(layers) {
  scen_year <- layers$data$scenario_year

  ## read in layers
  hab = SelectLayersData(layers, layers='hab_extension') %>%
    dplyr::select( rgn_id = "id_num",  year , habitat = "category", value ="val_num")


  ##superficie total de las comunas
  sup<-hab %>% filter(year ==scenario_years)%>%
    group_by(rgn_id)%>%
    dplyr::summarise(total_km2= sum(value)) %>%
    dplyr::select(rgn_id, total_km2)


  ##Functions
  hab<- merge(hab, sup)


  ##Punto de ref
  p_ref<- hab %>%
    dplyr::mutate(p_ref = value/ total_km2)  %>%
    dplyr::group_by(habitat) %>%
    dplyr::summarise(ref = max(p_ref,  na.rm = TRUE)) %>%
    dplyr::select(habitat, ref)

  ## Numero de habitats
  com_hab <- hab[!is.na(hab$value),]
  com<- filter(com_hab, rgn_id == 1)
  com_h1<-data.frame( rgn_id= 1,
                      n_h = nrow(table(com$habitat)))
  for (i in c(2:103)) {
    com<- filter(com_hab, rgn_id == i)
    com_h<-data.frame(rgn_id= i,
                      n_h = nrow(table(com$habitat)))
    com_h1<- rbind(com_h1, com_h)
  }
  com_h1 <- com_h1[!is.na(com_h1$n_h),]


  ##Scores
  hab<- merge(com_hab, p_ref)
  hab<-merge(hab, sup)

  scores_hab<- hab %>%
    dplyr::mutate(Cc= value/total_km2)  %>%
    dplyr::mutate(C= Cc/ref) %>%
    dplyr::group_by(rgn_id, year) %>%
    dplyr::summarise(c_sum = sum(C,  na.rm = TRUE)) %>%
    dplyr::full_join(com_h1, by= c("rgn_id"))%>%
    dplyr::mutate(status= (c_sum/n_h) *100)


  ##Status
  scores_hab <- scores_hab %>%
    filter(year ==scenario_years) %>%
    mutate(dimension = 'status',
           score     = round(status, 4)) %>%
    mutate(goal = 'HAB')%>%
    select(region_id = "rgn_id", goal, dimension, score)


  ##Tendencia
  #Debido a que el cambio de cobertura de los habitats no estan disponibles al momento de la realización de este indice
  #Utilizaremos la tedencia 0, suponiendo que ese es el presente estudio es el habitat inicial

  trend_data<- data.frame(region_id = c(1:36),
                          goal = c(rep("HAB", 36)),
                          dimension = c(rep("trend", 36)),
                          score = c(rep(0, 36)))



  scores_HAB<- rbind(scores_hab, trend_data)


  # return scores
  return(scores_HAB)
}


SPP <- function(layers) {

  scen_year <- layers$data$scenario_year

status <- SelectLayersData(layers, layers='spp_status') %>%
  dplyr::select( region_id = "category",dimension, score = "val_num", goal)


trend <- SelectLayersData(layers, layers='spp_trend') %>%
  dplyr::select( region_id = "category",dimension, score = "val_num", goal)

scores <- rbind(status, trend) %>%
    dplyr::mutate(goal = 'SPP') %>%
    dplyr::select(region_id, goal, dimension, score)


  return(scores)
}

BD <- function(scores) {
  d <- scores %>%
    dplyr::filter(goal %in% c('HAB', 'SPP')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[, c('region_id', 'goal', 'dimension', 'score')]))
}

PreGlobalScores <- function(layers, conf, scores) {
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    dplyr::select(id_num = rgn_id, val_chr = rgn_name)

  # limit to just desired regions and global (region_id==0)
  scores <- subset(scores, region_id %in% c(rgns[, 'id_num'], 0))


  return(scores)
}

FinalizeScores <- function(layers, conf, scores) {
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    dplyr::select(id_num = rgn_id, val_chr = rgn_name)


  # add NAs to missing combos (region_id, goal, dimension)
  d <- expand.grid(list(
    score_NA  = NA,
    region_id = c(rgns[, 'id_num'], 0),
    dimension = c(
      'pressures',
      'resilience',
      'status',
      'trend',
      'future',
      'score'
    ),
    goal      = c(conf$goals$goal, 'Index')
  ),
  stringsAsFactors = FALSE)
  head(d)
  d <- subset(d,!(
    dimension %in% c('pressures', 'resilience', 'trend') &
      region_id == 0
  ) &
    !(
      dimension %in% c('pressures', 'resilience', 'trend', 'status') &
        goal == 'Index'
    ))
  scores <-
    merge(scores, d, all = TRUE)[, c('goal', 'dimension', 'region_id', 'score')]

  # order
  scores <- dplyr::arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score <- round(scores$score, 2)

  return(scores)
}
