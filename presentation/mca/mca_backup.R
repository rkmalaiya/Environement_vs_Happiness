#### Permutation Test

```{r}
for (i in c(1, 2, 7, 9)) {
  zeDim = i
  pH1 <- prettyHist(
    distribution = country_env_mca_inf$Inference.Data$components$eigs.perm[,zeDim], 
    observed = country_env_mca_inf$Fixed.Data$ExPosition.Data$eigs[zeDim], 
    #xlim = c(0, country_env_mca_inf$Fixed.Data$ExPosition.Data$eigs[zeDim]+2), # needs to be set by hand
    breaks = 20,
    border = "black", 
    main = paste0("Permutation Test for Eigenvalue ",zeDim),
    xlab = paste0("Eigenvalue ",zeDim), 
    ylab = "", 
    counts = FALSE, 
    cutoffs = c( 0.975))
}
```

#### Parallet Test

```{r}
country_env_mca_mc <- data4PCCAR::monteCarlo.eigen(X = country_env_df_for_mca, nIter = 1000)
for (i in c(1, 2, 7, 9)) {
  zeDim = i
  pH1.p <- prettyHist(country_env_mca_mc$rand.eigs[,zeDim], 
                      observed = country_env_pca_mc$fixed.eigs[zeDim], 
                      xlim = c(0, country_env_pca_mc$fixed.eigs[zeDim]+2), # needs to set by hand
                      breaks = 20,
                      border = "black", 
                      main = paste0("Monte Carlo (Parallel) Test for Eigenvalue ",zeDim),
                      xlab = paste0("Eigenvalue ",zeDim), 
                      ylab = "", 
                      counts = FALSE, 
                      cutoffs = c( 0.975))
}

```

#### Bootstrap Test

```{r}

country_env_pca_bs <- data4PCCAR::boot.eigen(X = country_env_df_for_pca, nIter = 1000)

for (i in c(1, 2, 7, 9)) {
  zeDim = i
  prettyHist(country_env_pca_bs$boot.eigs[,zeDim], 
             observed = country_env_pca_bs$fixed.eigs[zeDim], 
             xlim = c(0, country_env_pca_bs$fixed.eigs[zeDim]+2), # needs to set by hand
             breaks = 20,
             border = "black", 
             main = paste0("Bootstrapped distribution for Eigenvalue ",zeDim),
             xlab = paste0("Eigenvalue ",zeDim), 
             ylab = "", 
             counts = FALSE, 
             cutoffs = c(0.025, 0.975))
}
```

