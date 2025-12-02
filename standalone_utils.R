
get_umap_uniq_subj <- function(feat_meta, feat_strict, 
                               n_neighbors=7, spread=4) {
  
  feat_meta_pass <- feat_meta %>%
    filter(is.na(QC) | QC!="FAIL")
  
  feat_meta_uniq <- feat_meta_pass %>%
    filter(!is.na(Subject)) %>%
    # filter(Study!="PREPRO") %>%
    filter(!duplicated(Subject))
  
  mat_uniq <- get_mat_scal(feat_meta_uniq, feat_strict)
  mat_all <- get_mat_scal(feat_meta_pass, feat_strict)
  
  um_model <- umap(mat_uniq, n_neighbors = n_neighbors, 
                   spread=spread, ret_model = TRUE)
  
  message(paste("Learned UMAP from", nrow(mat_uniq), "immune profiles selected",
                "to have QC=PASS and a single timepoint from each unique donor."))
  
  um <- umap_transform(mat_all, model=um_model)
  colnames(um) <- c("umap1", "umap2")
  
  message(paste("Mapped all", nrow(mat_all), "which pass QC to the learned UMAP",
                "irrespective of donor identity."))
  
  df_um <- as_tibble(cbind(feat_meta_pass, um)) %>%
    relocate(file, Study, umap1, umap2) 
  
  return(df_um)
}


get_mat_scal <- function(df, feat_names) {
  mat <- df %>%
    select(all_of(feat_names)) %>%
    as.matrix() %>%
    scale()
  return(mat)
}


