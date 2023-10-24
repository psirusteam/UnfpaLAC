Agregado_razon <-
  function(poststrat,
           epredmat_num,
           epredmat_den,
           byMap = c("depto", "etnia", "sexo")
           ) {
    
    ## Creación de variable para el calculo nacional
    if(is.null(byMap)){
      poststrat  %<>% mutate(Nacional = "Nacional")
      byMap <- "Nacional"
    }
    
    if(nrow(epredmat_num) != nrow(epredmat_den)){
          stop("revise el tamaño de num y den")
    }
    
    col_iter <- paste0("iter_",1:nrow(epredmat_num))
    
    epredmat_num <- t(epredmat_num) %>% as.data.frame()
    colnames(epredmat_num) <- col_iter
    
    epredmat_num <- cbind(poststrat[,c("n",byMap)],epredmat_num) %>% 
      group_by_at(byMap) %>% 
      summarise(across(starts_with("iter_"), ~ sum(.x * n),
                       .names = "num_{.col}"), .groups = "drop")
    
    epredmat_den <- t(epredmat_den)  %>% as.data.frame()
    colnames(epredmat_den) <- col_iter
    
    epredmat_den <- cbind(poststrat[,c("n",byMap)],epredmat_den) %>% 
      group_by_at(byMap) %>% 
      summarise(across(starts_with("iter_"), ~ sum(.x * n),
                       .names = "dem_{.col}"), .groups = "drop")
    
   temp_razon <- (select(epredmat_num, -byMap)/select(epredmat_den, -byMap))  
  
   Estimado_mrp <- epredmat_den %>% select(byMap) %>% 
     mutate( mrp_estimate = apply(temp_razon,1,mean),
             mrp_estimate_se = apply(temp_razon,1,sd))
   
    
    return(Estimado_mrp)
  }
