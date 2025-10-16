library(rjd3workspace)
# Chemin vers le workspace, A MODIFIER POUR POINTER VERS VOTRE WORKSPACE
new_file_workspace <- "R-macronia/macronia_10_2025.xml"
jws <- jws_open(new_file_workspace)
all_jmod <- rjd3workspace::jread_workspace(jws)
all_jmod <- lapply(all_jmod, function(sap) {
  # On enlève les noms des MP dans les SaItem
  names(sap) <- gsub(".*\n", "", names(sap))
  sap
})

# récupérer liste des statistiques exportables :
rjd3toolkit::dictionary(all_jmod[[1]][[1]])
# Pour récupérer une statistique en particulier :
rjd3toolkit::result(all_jmod[[1]][[1]], "diagnostics.seas-sa-f")
# Pour récupérer plusieurs tests sous forme de liste

liste_ind_res_seas <- c(`F-test` = "diagnostics.seas-sa-f", 
                        `QS-test` = "diagnostics.seas-sa-qs", 
                        Combined = "diagnostics.seas-sa-combined",
                        `Residual TD` = "diagnostics.td-sa-last")
c(rjd3toolkit::user_defined(all_jmod[[1]][[1]], liste_ind_res_seas))
# Maintenant on veut récupérer les p-values et pour le test combiné que la valeur du résultat 
# On veut tout ça sous forme de tableau :
list2DF(lapply(rjd3toolkit::user_defined(
  all_jmod[[1]][[1]], 
  liste_ind_res_seas), function(x) {
    if (length(x) > 1) 
      x <- x$pvalue
    x
  }))
# Maintenant il suffit de boucler sur tous mes SA-Processing et toutes mes séries :
all_res <- do.call(rbind, lapply(all_jmod, function(mp) {
  do.call(rbind, lapply(mp, function(sap) {
    res <- rjd3toolkit::user_defined(sap, liste_ind_res_seas)
    res <- lapply(res, function(x) {
      if (length(x) > 1) 
        x <- x$pvalue
      x
    })
    list2DF(res)
  }))
}))
all_res