plot_compare_razon <- function(sample_diseno, poststrat, by1){
  
  
  if (all(by1 %in% names(sample_diseno$variables))) {
    dat_encuesta <- sample_diseno %>%
      group_by_at(vars(by1)) %>%
      summarise(directo = survey_ratio(yk_num,yk_den),
                n_sample = unweighted(n())) %>%
      ungroup()
    
  } else{
    dat_encuesta <- unique(poststrat[,by1])
    n <- nrow(dat_encuesta)
    dat_encuesta %<>%
      mutate(
        directo = NA,
        n_sample = 1,
        directo_se = NA
      )
    
  }
  
  dat_censo <- poststrat %>% group_by_at(vars(by1)) %>%
    summarise(lmer = sum(n * yk_lmer_num) / sum(n*yk_lmer_den),
              bench = sum(n * yk_bench_num) / sum(n*yk_bench_den), .groups = "drop") %>% 
    ungroup()
  
  dat_plot <- full_join(dat_encuesta,dat_censo, by = by1)
  
  dat_plot1 <- dat_plot %>% 
    reshape2::melt(id = c(by1,"n_sample","directo_se"))
  
  
  if(length(by1)== 2) {
    x <-
      apply(poststrat[, by1], MARGIN = 2, function(x)
        length(unique(x))) %>% sort(decreasing = TRUE) %>% names()
    
    xmax <- x[1]
    dat_plot1[["x"]] <- dat_plot1[[xmax]]
    xmin <- x[2]
    dat_plot1[["facet"]] <- dat_plot1[[xmin]]
  } else if (length(by1) == 1) {
    xmax = by1
    dat_plot1[["x"]] <- dat_plot1[[by1]]
  }else if(length(by1)== 3) {
    x <-
      apply(poststrat[, by1], MARGIN = 2, function(x)
        length(unique(x))) %>% sort(decreasing = TRUE) %>% names()
    xmax <- x[1] 
    dat_plot1[["x"]] <- dat_plot1[[xmax]]
    xcol <- x[2]
    dat_plot1[["xcol"]] <- dat_plot1[[xcol]]
    xfila <- x[3]
    dat_plot1[["xfila"]] <- dat_plot1[[xfila]]
  } 
  
  plot1 <- ggplot(data = dat_plot1) +
    geom_jitter(aes(
      x = fct_reorder2(x, x, n_sample, .na_rm = FALSE),
      y = value,
      color = variable
    ), size = 2.5,width = 0.3) +
    scale_color_manual(
      breaks = c("directo", "lmer", "bench"),
      values = c("red", "blue", "green")
    ) +
    theme_bw(20) + labs(x = xmax, y = "", color = "") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(
        angle = 90,
        size = 8,
        vjust = 0.3
      ),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15)
    )
  
  if(length(by1) == 2){
    plot1 <- plot1 + facet_wrap(vars(facet), ncol = 2)
  } else if(length(by1) == 3){
    plot1 <- plot1 + facet_grid(xcol~xfila)  
  }
  
  if(all(by1 %in% names(sample_diseno$variables))){
    dat_plot2 <-
      dat_plot %>% reshape2::melt(id = c(by1, "n_sample", "directo_se", "directo"))
    
    lsy = max(dat_plot2$value)
    lsx = max(dat_plot2$directo)
    
    plot2 <- ggplot(data = dat_plot2) +
      geom_point(aes(
        x = directo,
        y = value,
      ), size = 3) +
      geom_abline(slope = 1,intercept = 0, color = "red", size = 2) +
      facet_wrap(vars(variable), ncol = 2) +
      theme_bw(20) + labs(y = "") +
      xlim(c(0, lsx+0.5)) + ylim(c(0,lsy+0.5))
  }else{
    lsy = max(dat_plot$bench)
    lsx = max(dat_plot$lmer)
    
    plot2 <- ggplot(data = dat_plot) +
      geom_point(aes(
        x = lmer,
        y = bench,
      ), size = 3) +
      geom_abline(slope = 1,intercept = 0, color = "red", size = 2) +
      theme_bw(20) + labs(y = "") +
      xlim(c(0, lsx+0.5)) + ylim(c(0,lsy+0.5)) 
  }
  #print(plot2  + plot1)
  return(list(tabla = dat_plot,  Plot = list(plot1 = plot1, plot2= plot2)))
}
