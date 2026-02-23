library(stringr)

# Code adapted from :
# Kang, H., Glaser, B.D., Sartorius, A.I. et al. Effects of oxytocin administration on non-social executive 
# functions in humans: a preregistered systematic review and meta-analysis. Mol Psychiatry (2025). 
# https://doi.org/10.1038/s41380-024-02871-4

# Making label for forest plot 

mlabfun <- function(text, res) {
  W <- diag(1/res$vi)
  X <- model.matrix(res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  I2.b <- 100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P))) 
  I2.w <- 100 * res$sigma2 / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P))) 
  
  result <- bquote(paste(.(text),
                         " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                         ", df = ", .(res$k - res$p),
                         ", p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
                         "Between-", I^2, " = ", .(formatC(I2.b, digits=2, format="f")), "%, ",
                         "Within-", I^2, " = ", .(formatC(I2.w[2], digits=2, format="f")), "% ",
                         ")"))
  
  return(result)
}

multi_subset_analysis <- function(data, variable) {
  levels <- unique(data[[variable]])
  results <- list()
  
  for (level in levels) {
    # Skip the level "Perceptual Motor"
    if (level == "Perceptual Motor") {
      next
    }
    
    res <- rma.mv(yi, vi, 
                  subset = (get(variable) == level), 
                  random = ~ 1 | SID / UID, 
                  method = "REML",
                  data = data)
    results[[level]] <- res
  }
  
  return(results)
}

meta_mod <- function(data, mod) {
  ### fit meta-regression model to test for subgroup differences
  res <- rma.mv(yi, vi, 
                data = data,
                random = ~ 1 | SID/UID,
                mods = as.formula(paste("~", mod)),
                method = "REML",
                slab = CITEKEY)
  return(res)
}


generate_subplot <- function(data, type, res, idx) {
  settings <- list(
    "POPULATION" = list(
      ilab_columns = c("HRV_MEASURE", "COG_DOMAIN"),
      order_by = "POPULATION",
      ilab_headers = c("HRV Measure", "Cognitive Domain"),
      header_offset = 8.5,
      ylim_factor = 3 * 2.5,
      rows = c(idx:20, 15:2),
      subgroup_column = "POPULATION",
      subset_analysis = multi_subset_analysis(data, "POPULATION"),
      addpoly_rows = c(18.75, 0.75),
      subgroup_text_rows = c(28, 16)
    ),
    "HRV_MEASURE" = list(
      ilab_columns = c("POPULATION", "COG_DOMAIN"),
      order_by = "HRV_MEASURE",
      ilab_headers = c("Population", "HRV Measure"),
      header_offset = 11,
      ylim_factor = 5 * 2,
      rows = c(30:26, 21:17, idx:2),
      subgroup_column = "HRV_MEASURE",
      subset_analysis = multi_subset_analysis(data, "HRV_MEASURE"),
      addpoly_rows = c(0.75, 15.75, 24.75),
      subgroup_text_rows = c(31, 22, 14)
    ),
    "COG_DOMAIN" = list(
      ilab_columns = c("POPULATION", "HRV_MEASURE"),
      order_by = "COG_DOMAIN",
      ilab_headers = c("Population", "HRV Measure"),
      header_offset = 13.6,
      ylim_factor = 5 * 2.5,
      rows = c(32:28, idx:14, 10, 6:2),
      subgroup_column = "COG_DOMAIN",
      subset_analysis = multi_subset_analysis(data, "COG_DOMAIN"),
      addpoly_rows = c(12.75,0.75,26.75),
      subgroup_text_rows = c(33, 25, 11, 7)
    )
  )
  
  params <- settings[[type]]

  # Create ilab using the column names from data
  ilab <- cbind(data[[params$ilab_columns[1]]], data[[params$ilab_columns[2]]])
  
  forest(res,
         slab = CITEKEY,
         ilab = ilab,
         ilab.xpos = c(-9, -6),
         header = "Study",
         order = data[[params$order_by]],
         cex = 1,
         xlim = c(-12, 4),
         ylim = c(-3, length(data$X) + 2 + params$ylim_factor),
         at = c(-4, -1, 0, 1, 2),
         rows = params$rows,
         pch = 19,
         mlab = mlabfun("RE Model for All Studies", res)
  )
  
  text(c(-9, -6), res$k + params$header_offset, params$ilab_headers, font = 2, cex = 1)
  
  subgroup_levels <- levels(factor(data[[params$subgroup_column]]))
  text(-12, params$subgroup_text_rows, pos = 4, subgroup_levels, font = 2, cex = 1)
  
  for (i in seq_along(params$subset_analysis)) {
    addpoly(params$subset_analysis[[i]], row = params$addpoly_rows[i],
            mlab = list(mlabfun("RE Model for Subgroup", params$subset_analysis[[i]])))
  }
  
  ### fit meta-regression model to test for subgroup differences
  mod_res <- meta_mod(data,type)

  ### add text for the test of subgroup differences
  text(-12, -4, pos=4, cex=1, bquote(paste("Test for Subgroup Differences: ", Q[M], 
                                            " = ", .(formatC(mod_res$QM, digits=2, format="f")), 
                                            ", df = ", .(mod_res$p - 1),
                                            ", p = ", .(formatC(mod_res$QMp, digits=2, format="f")))))
  
}


