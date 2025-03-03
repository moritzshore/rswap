if(FALSE){
# library
library(ggplot2)


# custom rswap color pallete ;)
# might remove ...
rswap_palette <- colorRampPalette(colors = c(
  "#ea7000",
  "#9f2b00",
  "#61bbff",
  "#1f6abf",
  "#00b900",
  "#375700"
))(length(mgt_scen))

if(var == "eact"){
  p <- ggplot(data=res_df, aes(x=SCEN_VAR, group=Scenario, fill=Scenario)) +
  geom_density(adjust=1.5, position="fill")+xlab(var)+ ggtitle(paste0("rswap scenario analysis [", stat, " values]"),paste0(var, " under following scenarios: ~", scen_dir, scen_name))+
  scale_fill_manual(values = rswap_palette)
p %>% print()
return()
}


if(is.null(stat)){
  p <- ggplot(res_df, aes(x = Scenario, y = SCEN_VAR, fill = Scenario)) +
    geom_boxplot() + ylab(var) +
    ggtitle(
      "rswap scenario analysis",
      paste0(var, " under following scenarios: ~", scen_dir, scen_name)
    ) +
    scale_fill_manual(values = rswap_palette) + theme(legend.position = "none") +
    theme(axis.title.x = element_blank())
  p %>% print()

}else if(stat == "mean"){
  p<-ggplot(res_df, aes(x=Scenario, y=SCEN_VAR, fill=Scenario)) +
    geom_bar(stat="identity")+theme_minimal()+scale_fill_manual(values = rswap_palette)+ ylab(var)+
    ggtitle(paste0("rswap scenario analysis [", stat, " values]"),paste0(var, " under following scenarios: ~", scen_dir, scen_name))
  p%>% print()
}else{
  warning("[rswap scenario analysis] Plotting not supported yet for", stat)
}

if(var == "drainage"){
  library(ggridges)
  # basic example
  ggplot(res_df, aes(x = SCEN_VAR, y = Scenario, fill = Scenario)) +
    geom_density_ridges(linewidth = 1, alpha = .5) +
    theme_ridges() + xlab(var)+
    theme(legend.position = "none")+ theme(axis.title.y=element_blank())+
    ggtitle(paste0("rswap scenario analysis [", stat, " values]"),paste0(var, " under following scenarios: ~", scen_dir, scen_name))+scale_fill_manual(values = rswap_palette) + theme(legend.position = "none")
}

}
