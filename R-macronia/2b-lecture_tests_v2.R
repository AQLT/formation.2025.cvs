library(rjd3workspace)
# Chemin vers le workspace, A MODIFIER POUR POINTER VERS VOTRE WORKSPACE
new_file_workspace <- "data/macronia.xml"
jws <- RJDemetra::load_workspace(new_file_workspace)
RJDemetra::compute(jws)
all_jmod <- RJDemetra::get_jmodel(jws) # On charge tous les modèles
all_jmod <- lapply(all_jmod, function(sap) {
  # On enlève les noms des MP dans les SaItem
  names(sap) <- gsub(".*\n", "", names(sap))
  sap
})

# récupérer liste des statistiques exportables :
RJDemetra::get_dictionary(all_jmod[[1]][[1]])
# Pour récupérer une statistique en particulier :
RJDemetra::get_indicators(all_jmod[[1]][[1]], "diagnostics.seas-sa-f")
# Pour récupérer plusieurs tests sous forme de liste
liste_ind_res_seas <- c(`F-test` = "diagnostics.seas-sa-f", 
                        `QS-test` = "diagnostics.seas-sa-qs", 
                        Combined = "diagnostics.seas-sa-combined",
                        `Residual TD` = "diagnostics.td-sa-last")
RJDemetra::get_indicators(all_jmod[[1]][[1]], liste_ind_res_seas)
# Maintenant on veut récupérer les p-values et pour le test combiné que la valeur du résultat 
# On veut tout ça sous forme de tableau :
list2DF(lapply(RJDemetra::get_indicators(
  all_jmod[[1]][[1]], 
  liste_ind_res_seas), function(x) {
    if (length(x) > 1) 
      x <- x[2]
    x
  }))
# Maintenant il suffit de boucler sur tous mes SA-Processing et toutes mes séries :
all_res <- do.call(rbind, lapply(all_jmod, function(mp) {
  do.call(rbind, lapply(mp, function(sap) {
    res <- RJDemetra::get_indicators(sap, liste_ind_res_seas)
    res <- lapply(res, function(x) {
      if (length(x) > 1) 
        x <- x[2]
      x
    })
    names(res) <- names(liste_ind_res_seas)
    list2DF(res)
  }))
}))
all_res
