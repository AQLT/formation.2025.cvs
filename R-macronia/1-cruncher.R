packages_a_installer <- c("rjd3workspace", "rjd3report", "ggdemetra3", "rjwsacruncher", "readr", "openxlsx")
packages_a_installer <- packages_a_installer[! packages_a_installer %in% installed.packages()[,"Package"]]
if (length(packages_a_installer) > 0) {
  install.packages(packages_a_installer, repos = c("https://aqlt.r-universe.dev", "https://cloud.r-project.org"))
}
library(rjwsacruncher)
create_param_file(
  # dossier où le fichier de paramètres sera créé
  # Attention : modifier si besoin, si ce dossier n'existe pas vous aurez une erreur
  dir_file_param = "R-macronia", 
  policy = "lastoutliers", # politique de rafraichissement
  csv_layout = "vtable" # format d'export des fichiers CSV
)
# Il faut spécifier où est le JWSACruncher
options(cruncher_bin_directory = "/Applications/jwsacruncher/jwsacruncher-3.5.1/bin")
getOption("cruncher_bin_directory")

# Plutôt que de lancer le cruncher directement sur le workspace on va faire une copie
library(rjd3workspace)
jws <- jws_open(
  # Chemin vers le workspace, A MODIFIER POUR POINTER VERS VOTRE WORKSPACE
  "data/macronia.xml"
)
new_file_workspace <- sprintf(
  "R-macronia/macronia_%s.xml", # A MODIFIER
  format(Sys.time(), # je récupère la date du jour
         "%m_%Y") # je la mets sous le format MM_YYYY
)
# Si plus simple mettre directement le nom du fichier :
new_file_workspace <- "R-macronia/macronia_10_2025.xml" # MODIFIER

# Ce code permet de créer la copie du workspace
rjd3workspace::save_workspace(jws, new_file_workspace, replace = TRUE)

# ATTENTION : SI VOUS UTILISEZ DIRECTEMENT LE WORKSPACE macronia.xml AUCUNE DONNÉE NE SERA EXPORTÉE :
# LE CRUNCHER VA EN EFFET CHERCHER LES DONNÉES LÀ OÙ J'AI CRÉÉ LE PROGRAMME
cruncher(workspace = new_file_workspace,
         param_file_path = "R-macronia/parameters.param", # A MODIFIER AVEC VOTRE CHEMIN
         log_file = "R-macronia/log.txt" # exporte un fichier log
)
