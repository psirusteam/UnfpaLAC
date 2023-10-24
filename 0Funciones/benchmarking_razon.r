benchmarking_razon <- function(poststrat_df, encuesta_sta, var_bench, metodo){
  conti <- NA
  
  ### total
  aux_total_razon <- function(encuesta = encuesta_sta, var_by = "dam"){
    diseno_temp <- as_survey_design(.data = encuesta, weights = fexp)
    
    diseno_temp %>% group_by_at(var_by) %>%
      summarise(
        Num = survey_total(Num),
        Den = survey_total(Den)
      ) %>%
      transmute(
        by_temp = get(var_by),
        Num,
        Den
      )  %>% 
      reshape2::melt(id = "by_temp") %>%
      transmute(paso = paste0(var_by,"_", by_temp,"_",variable),value) %>% 
      reshape2::dcast(.~paso)
  }
  
  
  while(!conti %in% c(1:2)) {
    Xk <- poststrat_df %>% 
      select(n, Num, Den, all_of(var_bench)) %>% 
      fastDummies::dummy_cols(select_columns = var_bench,
                              remove_selected_columns = FALSE)
    
    
    estimaciones <-
      map(var_bench ,~ Xk %>% group_by_at(all_of(.x)) %>%
            summarise(
              Nhat = sum(n),
              Total_Num = sum(Num *n),
              Total_Den = sum(Den *n),
              Razon = Total_Num/Total_Den,
            ))
    
    Xdummy <- Xk %>% select(-Num,-Den,-n) %>% 
      mutate_at(vars(matches("_\\d")) ,
                list(Num = function(x) x*poststrat_df$Num,
                     Den = function(x) x*poststrat_df$Den)) %>% 
      select((matches("*_(Num|Den)$"))) 
    
   
    Total <-
      map(var_bench,
          ~ encuesta_sta %>% aux_total_razon(var_by = .x) %>% select_if(is.numeric)) %>% 
      unlist()
    
    temp_len <- length(intersect(names(Total) , names(Xdummy)))
    
    if(temp_len != length(Total) ||
       temp_len != ncol(Xdummy)) {
      cat("Revisar covariables \n")
    } else{
      Total <- Total[colnames(Xdummy)]
    }
    
    
    names_Tasa <- colnames(Xdummy)
    gk_razon <- tryCatch(calib(
          Xs = Xdummy[,names_Tasa],
          d = poststrat_df$n,
          total = Total[names_Tasa],
          method = metodo,
          max_iter = 1000
        ), 
        error = function(e) {NULL}
        )
        
        if(is.null(gk_razon)){
          cat("Editar el parámetro  metodo (linear o logit) \n")
          metodo <- edit(metodo)
          conti <- NA
        }else{
          
          check_razon <- checkcalibration(Xs = Xdummy[,names_Tasa], 
                                         d = poststrat_df$n,
                                         total = Total[names_Tasa],
                                         g = gk_razon)$result
          
          if(!check_razon){cat("La calibración NO hizo convergencia \n")}
          if(min(gk_razon) <= 0){cat("Los gks tienen valores negativos \n")}
          
          cat("Resumen de los gks \n")
          print(summary(gk_razon))
          
          cat("
----------------------------------------------------
    Tabla de comparación de los gks \n")
          
          print(
            data.frame(
              Poblacion = colSums(Xdummy[, names_Tasa] * poststrat_df$n * gk_razon),
              Muestra = Total[names_Tasa]
            ) %>% mutate(diff = round(abs(Poblacion - Muestra), 3)) %>%
              arrange(diff)
            
          )
          
          
          while(!conti %in% c(1:2)) {
            cat("¿Desea continuar? \n 1. Sí \n 2. No\n")
            conti <- as.numeric(readline())
          }
          
          if (conti == 2) {
            cat("Editar el parámetro  metodo (linear o logit) \n")
            metodo <- edit(metodo)
            cat("Editar el parámetro  var_bench \n")
            var_bench <- edit(var_bench)
           conti <- NA
          }
        
        
      }
      
   plot1 <- ggplot(data = data.frame(x = gk_razon), aes(x = x)) +
        geom_histogram(bins = 100) +
        labs(y = "", x = "gks") +
        geom_vline(xintercept = 1,color = "red")

print(plot1)

      }   
  browser()
  return(list(gk_bench =  gk_razon,
              var_bench = var_bench, 
              metodo_bench = metodo,
              plot_bench = plot1))
  
}
