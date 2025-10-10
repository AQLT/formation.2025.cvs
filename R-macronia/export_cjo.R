library(RJDemetra)
library(rJava)
#Export Excel
library(XLConnect)

#' Fonction permettant d'exporter un objet ts vers un fichier Excel
#' 
#' @param x objet à exporter.
#' @param file chemin vers le fichier Excel.
#' @param sheet nom de la feuille dans laquelle exporter les données.
#' @param format format de la date dans Excel (par défaut "dd/mm/yyyy").
ts2xls <- function(x, file, sheet="Feuille 1", format = "dd/mm/yyyy"){
  wb <- loadWorkbook(file, create = TRUE)
  createSheet(wb, sheet)
  if(is.mts(x)){
    col <- c("date", colnames(x))
  }else{
    col <- c("date", "x")
  }
  # Le titre
  writeWorksheet(wb,matrix(col,nrow = 1),
                 sheet = sheet,startCol = 1,startRow =1,
                 header = FALSE)

  # Petit trick pour que la colonne date soit au format date d'Excel
  csDate <- getOrCreateCellStyle(wb, name = "date")
  setDataFormat(csDate, format = format)
  date <- as.Date(format(zoo::as.Date((time(x))), "%d/%m/%Y"),
                  "%d/%m/%Y")
  writeWorksheet(wb,date,sheet = sheet,
                 startCol = 1,startRow = 2,
                 header = FALSE)
  setCellStyle(wb, sheet = sheet, row = seq_along(date)+1,
               col = 1,
               cellstyle = csDate)
  # Fin colonne date

  # Autres colonnes
  writeWorksheet(wb,x,sheet = sheet,startCol = 2,startRow = 2,
                 header = FALSE)
  setColumnWidth(wb, sheet, column = seq_along(col), width = -1)
  saveWorkbook(wb, file)
}

#' Fonction permettant de compléter les variables d'un workspace
#' 
#' @param liste_var ensemble des variables à ajouter dans le workspace.
#' @param workspace workspace dans lequel ajouter les variables.
complete_variables <- function(liste_var, workspace){
  if(!is.mts(liste_var))
    stop("liste_var doit être de type mts")
  context_dictionary <- .jcall(workspace,"Lec/tstoolkit/algorithm/ProcessingContext;", "getContext")
  ts_variable_managers <- context_dictionary$getTsVariableManagers()
  ts_variables <- .jnew("ec/tstoolkit/timeseries/regression/TsVariables")
  jd_r_variables <- ts_variable_managers$get("r")
  if (is.null(jd_r_variables)) {
    ts_variable_managers$set("r",
                             .jnew("ec/tstoolkit/timeseries/regression/TsVariables"))
    jd_r_variables <- ts_variable_managers$get("r")
  }
  jd_var_names <- jd_r_variables$getNames()

  model_var_names <- colnames(liste_var)

  for (i in seq_along(model_var_names)) {
    name <- model_var_names[i]
    dictionary_var <- jd_r_variables$get(name)
    tsvar <- .jnew("ec/tstoolkit/timeseries/regression/TsVariable",
                   name, RJDemetra:::ts_r2jd(liste_var[, i]))
    if (is.null(dictionary_var)) {
      jd_r_variables$set(name, tsvar)
    } else {
      warning(sprintf("La variable %s existe déjà", name))
    }
  }
}

# Pour la création des regresseurs, voir le TP R associé.
# Par exemple :
library(rjd3toolkit)

jours_macronia <- list(
  special_day("NEWYEAR"),
  special_day("EASTERMONDAY"), # Lundi de Pâques
  fixed_day(4, 4), # Jour de l'indépendance de la Macronia
  special_day("MAYDAY"), # 1er mai
  special_day("ASCENSION"), # Jour de l'Ascension
  special_day("WHITMONDAY"), # Lundi de Pentecôte
  special_day("ASSUMPTION"), # Assomption de Marie
  special_day("ALLSAINTSDAY"), # Toussaint
  special_day("CHRISTMAS") # Noël
)
# # Manque Début ramadan et jours décrétés
# # On récupère ces jours construisant un fichier Excel
# jours_mobiles <- readxl::read_excel("data/DateFetesMusulmanes_Macronia.xlsx") |>
#   as.data.frame()
# jours_mobiles <- jours_mobiles[,-1, drop = FALSE] # on enlève l'année

jours_macronia <- c(
  jours_macronia,
  list(
    # DEBUT_RAMADAN,
    single_day("2000-11-28"),
    single_day("2001-11-17"),
    single_day("2002-11-07"),
    single_day("2003-10-27"),
    single_day("2004-10-15"),
    single_day("2005-10-05"),
    single_day("2006-09-24"),
    single_day("2007-09-14"),
    single_day("2008-09-02"),
    single_day("2009-08-22"),
    single_day("2010-08-12"),
    single_day("2011-08-01"),
    single_day("2012-07-20"),
    single_day("2013-07-10"),
    single_day("2014-06-29"),
    single_day("2015-07-18"),
    single_day("2016-06-07"),
    single_day("2017-05-27"),
    single_day("2018-05-17"),
    single_day("2019-05-06"),
    single_day("2020-04-24"),
    single_day("2021-04-14"),
    single_day("2022-04-03"),
    single_day("2023-03-23"),
    single_day("2024-03-12")
  ))

CAL <- national_calendar(jours_macronia)
frequency <- 4
start <- c(1990,1)
end = c(2030, 1)
length = (end[1] - start[1]) * frequency + end[2] - start[2]


gen_calendrier <- function(cal, frequency, start = c(1990, 1), end = c(2030, 1)) {
  length = (end[1] - start[1]) * frequency + end[2] - start[2]
  ly <- rjd3toolkit::lp_variable(frequency = frequency, start = start,
                                 length = length)
  # N'hésitez pas à ajouter les votre !
  TD7 <- calendar_td(cal, frequency = frequency, start = start, length = length,
                     groups = c(1, 2, 3, 4, 5, 6, 0))
  TD4 <- calendar_td(cal, frequency = frequency, start = start, length = length,
                     groups = c(1, 1, 1, 1, 2, 3, 0))
  TD3 <- calendar_td(cal, frequency = frequency, start = start, length = length,
                     groups = c(1, 1, 1, 1, 1, 2, 0))
  TD3c <- calendar_td(cal, frequency = frequency, start = start, length = length,
                      groups = c(1, 1, 1, 1, 2, 2, 0))
  TD2 <- calendar_td(cal, frequency = frequency, start = start, length = length,
                     groups = c(1, 1, 1, 1, 1, 0, 0))
  TD2c <- calendar_td(cal, frequency = frequency, start = start, length = length,
                      groups = c(1, 1, 1, 1, 1, 1, 0))

  reg_jo <- ts(cbind(TD2, TD2c, TD3, TD3c, TD4, TD7),
               start = start, frequency = frequency)
  reg_jo <- ts.intersect(reg_jo,
                         ly)
  colnames(reg_jo) <- c(
    "TD2_semaine",
    "TD2c_lundi_samedi",
    sprintf("TD3_%s", c("semaine", "samedi")),
    sprintf("TD3c_%s", c("lundi_jeudi", "vendredi_samedi")),
    sprintf("TD4_%s", c("lundi_jeudi", "vendredi", "samedi")),
    sprintf("TD7_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")),
    "leap_year")
  reg_jo
}
regresseurs_JO_trim <- gen_calendrier(CAL, frequency = 4)
regresseurs_JO_mens <- gen_calendrier(CAL, frequency = 12)

colnames(regresseurs_JO_trim) <- paste0(colnames(regresseurs_JO_trim), "_trim")
colnames(regresseurs_JO_mens) <- paste0(colnames(regresseurs_JO_mens), "_mens")

############################################################
### Création d'un workspace en V2 et ajout des variables ###
############################################################

jws <- new_workspace()
# regresseurs_JO est l'objet mts qui contient tous vos régresseurs
# Il doit donc déjà être créé (voir code ci-dessus) !
complete_variables(regresseurs_JO_mens, jws)
complete_variables(regresseurs_JO_trim, jws)
save_workspace(jws,"wk_CJO.xml")

# On peut également exporter les régresseurs vers un fichier Excel
ts2xls(regresseurs_JO_mens, file = "data/reg_cjo_macronia.xlsx", sheet = "Reg mensuels")
ts2xls(regresseurs_JO_trim, file = "data/reg_cjo_macronia.xlsx", sheet = "Reg trim")

#############################################################################
### Création d'un workspace en V2 et ajout des variables et du calendrier ###
#############################################################################
# On va créer deux groupes de variables, on peut donc simplifier les noms
colnames(regresseurs_JO_mens) <-
  colnames(regresseurs_JO_trim) <-
  gsub("_mens", "", colnames(regresseurs_JO_mens))
ctxt <- rjd3toolkit::modelling_context(
  # on appelle "CAL" le calendrier
  calendars = list(CAL = CAL),
  # on crée un groupe de variables "cjo_mens" contenant les régresseurs mensuels
  # et un groupe de variables "cjo_trim" contenant les régresseurs trimestriels
  variables = list(cjo_mens = regresseurs_JO_mens,
                   cjo_trim = regresseurs_JO_trim)
)

jws <- rjd3workspace::jws_new(ctxt)
# On peut également ajouter les calendriers et les variables avec les fonctions :
# rjd3workspace:::add_variables()
# rjd3workspace:::add_calendar()
# Pour modifier un workspace existant :
# rjd3workspace::set_context()
rjd3workspace::save_workspace(jws, "wk_CJO_v3.xml")
