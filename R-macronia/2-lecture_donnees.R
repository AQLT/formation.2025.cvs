packages_a_installer <- c("rjd3workspace", "ggdemetra3", "rjwsacruncher", "readr", "openxlsx")
packages_a_installer <- packages_a_installer[! packages_a_installer %in% installed.packages()[,"Package"]]
if (length(packages_a_installer) > 0) {
  install.packages(packages_a_installer, repos = c("https://aqlt.r-universe.dev", "https://cloud.r-project.org"))
}
##################################
### Récupération des résultats ###
##################################
library(rjd3workspace)

# Chemin vers le workspace, A MODIFIER POUR POINTER VERS VOTRE WORKSPACE
new_file_workspace <- "R-macronia/macronia_10_2025.xml"

## Plusieurs façons de récupérer les données : 
# 1. en reprenant les fichiers CSV générés par le JWSACruncher
# 2. en utilisant rjd3workspace pour lire le workspace

# Méthode 1 : récupérer les données
library(readr)
series_sa <- read_delim(
  # On spécifie ici le chemin vers le fichier
  # Par défaut, il sera dans le dossier associé à votre workspace, dans un sous-dossier output
  # et ensuite vous aurez un dossier par SAProcessing
  file.path(
    gsub(".xml", "", new_file_workspace), 
    "Output", 
    "SAProcessing-1", # !!! MODIFIER SI NOM DU SAProcessing DIFFÉRENT
    "series_sa.csv"),
  delim = ";", escape_double = FALSE, col_names = TRUE,
  # Si sous JDemetra+ les données importées depuis un fichier Excel avec des accents,
  # il peut être nécessaire de spécifier l'encodage
  locale = locale(encoding = "WINDOWS-1252", decimal_mark = ","),
  trim_ws = TRUE
)
series_sa <- as.data.frame(series_sa)

# Exemple d'export en Excel :
# On crée un fichier "series.xlsx"
openxlsx::write.xlsx(list(sa = series_sa), sheet = "series_sap1.xlsx")
# Autre façon de lire les données avec base R:
series_sa2 <- read.csv2(
  # On spécifie ici le chemin vers le fichier
  # Par défaut, il sera dans le dossier associé à votre workspace, dans un sous-dossier output
  # et ensuite vous aurez un dossier par SAProcessing
  file.path(
    gsub(".xml", "", new_file_workspace), 
    "Output", 
    "SAProcessing-1", # MODIFIER SI NOM DU SAProcessing DIFFÉRENT
    "series_sa.csv"),
  fileEncoding = "WINDOWS-1252"
)


# Méthode 2 : utiliser rjd3workspace pour la v3
jws <- jws_open(new_file_workspace)
all_jmod <- rjd3workspace::.jread_workspace(jws)
all_jmod <- lapply(all_jmod, function(sap) {
  # On enlève les noms des MP dans les SaItem
  names(sap) <- gsub(".*\n", "", names(sap))
  sap
})

# Exemple pour récupérer la série désaisonnalisée
# Permet d'avoir une liste avec les séries désaisonnalisées de chaque SAProcessing
all_sa <- lapply(all_jmod, function(mp) {
  do.call(ts.union, lapply(mp, ggdemetra3::seasonaladj))
})
all_y <- lapply(all_jmod, function(mp) {
  do.call(ts.union, lapply(mp, ggdemetra3::raw))
})
openxlsx::write.xlsx(all_sa, sheet = "sa.xlsx")
