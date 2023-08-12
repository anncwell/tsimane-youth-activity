######################
# project functions #
######################

.c<-function(x) {foo<-(x - mean(x,na.rm=T)); return(foo)}
.std<-function(x) {foo<-(x-mean(x,na.rm=T))/sd(x,na.rm=T); return(foo)}

mean_sd <- function(x, prop=FALSE, digs="%.1f") {
  paste0(
    sprintf(digs, mean(x, na.rm = TRUE)*100^as.numeric(prop)),
    " (",
    sprintf(digs, sd(x, na.rm = TRUE)*100^as.numeric(prop)),
    ")"
  )
}

p_values <- function(model, round=TRUE, sig.diff=3){
	mod <- as.data.frame(summary(model)$coefficients)
	
	if(round==TRUE){
		foo<-round(mod[,4], sig.diff)
		foo[which(foo<10^-sig.diff)]<-paste0("<", format(10^-sig.diff, scientific=F))
		foo<-as.character(foo)
		}
	return(foo)
}

estimate_se<-function(model, labels = NA, dig.est = 3, dig.ci = 3, pval_stars = FALSE, exponentiate = FALSE){
	mod<-as.data.frame(summary(model)$coefficients)
	
	estimates<-format(round(mod$Estimate, digits = dig.est), scientific = F)
	std.error <- format(round(mod[,2], digits = dig.ci), scientific = FALSE)

	
	if(exponentiate==TRUE){
	  estimates[2:length(estimates)]<- round(exp(as.numeric(estimates[2:length(estimates)])),digits = dig.est)
	  std.error[2:length(std.error)] <- round(exp(as.numeric(std.error[2:length(std.error)])), digits = dig.ci)
	}
	
	pvalues<-mod[,4]
	p.out<-cut(pvalues, c(-Inf, 0.001, 0.01, 0.05, 0.1, 1.1), labels=c("***", "**", "*", "^", ""))
	
	if(pval_stars==FALSE){
    out<-data.frame(Variable = rownames(mod),Estimate = paste0(estimates, " [", std.error, "]"))
	return(out)
  }

	if(pval_stars == TRUE & any(class(model)  %in% c("glm", "lm"))) {
			out<-data.frame(Variable = rownames(mod),
		Estimate = paste0(estimates, " [", std.error, "]", p.out))
		return(out)
  }

	if(pval_stars == TRUE & any(!class(model)  %in% c("glm", "lm")))
 {stop("I haven't written code for classes besides glm and lm yet! Try it without p-values")}
}

#############################
# create path model figures #
#############################


path_model<-function(fit, output, quite=TRUE){
	exclude <- which(parameterEstimates(fit)$op==':=')
	paths <- parameterEstimates(fit)[c('lhs', 'op', 'rhs', 'est','pvalue')] 
	paths$est<-round(paths$est,2)
	paths$astrix<-NA
	for(i in 1:nrow(paths)){
			if(!is.na(paths$pvalue[i])){
				if(paths$pvalue[i]<=.05) paths$astrix[i]<-'*'
				if(paths$pvalue[i]<=.01) paths$astrix[i]<-'**'
				if(paths$pvalue[i]<=.001) paths$astrix[i]<-'***'
				}
		}
	paths$astrix[which(is.na(paths$astrix))]<-""
	paths$est<-paste0(paths$est,paths$astrix)
	my_id_df<-data.frame(labels=unique(c(paths$lhs[exclude], paths$rhs[exclude])), id=as.numeric(factor(unique(c(paths$lhs[exclude], paths$rhs[exclude])), levels=unique(c(paths$lhs[exclude], paths$rhs[exclude])))))
		
		paths$lhs[which(paths$lhs=='Tanner')]<-'Tanner'
		paths$rhs[which(paths$rhs=='Tanner')]<-'Tanner'
		paths$lhs[which(paths$lhs=='Edad')]<-'Age'
		paths$rhs[which(paths$rhs=='Edad')]<-'Age'
		paths$rhs[which(paths$rhs=='SumMinsSed')]<-'Sedentary'
		paths$lhs[which(paths$lhs=='SumMinsSed')]<-'Sedentary'
		paths$rhs[which(paths$rhs=='SumMinsLight')]<-'Light'
		paths$lhs[which(paths$lhs=='SumMinsLight')]<-'Light'
		paths$rhs[which(paths$rhs=='SumMinsMod')]<-'Moderate'
		paths$lhs[which(paths$lhs=='SumMinsMod')]<-'Moderate'
		paths$rhs[which(paths$rhs=='SumMinsVig')]<-'Vigorous'
		paths$lhs[which(paths$lhs=='SumMinsVig')]<-'Vigorous'
		paths$rhs[which(paths$rhs=='SumMinsMVPA')]<-'MVPA'
		paths$lhs[which(paths$lhs=='SumMinsMVPA')]<-'MVPA'
		paths$rhs[which(paths$rhs=='steps_per_day')]<-'Steps/day'
		paths$lhs[which(paths$lhs=='steps_per_day')]<-'Steps/day'
		paths$rhs[which(paths$rhs=='steps_per_day_full_day')]<-'Steps/day'
		paths$lhs[which(paths$lhs=='steps_per_day_full_day')]<-'Steps/day'
		paths$rhs[which(paths$rhs=='SumMinsSed_full_day')]<-'Sedentary'
		paths$lhs[which(paths$lhs=='SumMinsSed_full_day')]<-'Sedentary'
		paths$rhs[which(paths$rhs=='SumMinsLight_full_day')]<-'Light'
		paths$lhs[which(paths$lhs=='SumMinsLight_full_day')]<-'Light'
		paths$rhs[which(paths$rhs=='SumMinsMod_full_day')]<-'Moderate'
		paths$lhs[which(paths$lhs=='SumMinsMod_full_day')]<-'Moderate'
		paths$rhs[which(paths$rhs=='SumMinsVig_full_day')]<-'Vigorous'
		paths$lhs[which(paths$lhs=='SumMinsVig_full_day')]<-'Vigorous'
		paths$rhs[which(paths$rhs=='SumMinsMVPA_full_day')]<-'MVPA'
		paths$lhs[which(paths$lhs=='SumMinsMVPA_full_day')]<-'MVPA'

	manifest <- data.frame(label = unique(paths[which(!paths$op %in% c("~1", ":=")), 'lhs']), shape = "rectangle", type='manifest', fontsize=7)
	node_set<-rbind(manifest)
	all_paths <- paths %>% filter(op != "~1") %>% mutate(label = est) %>% select(-est,-astrix, -pvalue)
	loadings <- all_paths %>% filter(op == "=~") %>% mutate(from = lhs, to = rhs, style = "dashed", rel="loadings") %>% select(from, to, style, label, rel)
	regressions <- all_paths %>% filter(op == "~") %>% rename(to = lhs, from = rhs) %>% mutate(style = "solid", rel="regressions") %>% select(from, to, style, label, rel)
	edge_set <- rbind(loadings, regressions)
	
	my_graph <- create_graph(attr_theme='default') %>%  add_nodes_from_table(node_set, label_col=labels, type_col=type) %>% add_edges_from_table(edge_set, from_col =from, to_col = to, rel_col=rel, from_to_map= label) %>% set_node_position(node = 1, x=0, y=1) %>% set_node_position(node = 2, x=1, y=0) %>% set_node_position(node = 3, x=-1, y=0) %>% set_node_attrs(node_attr = "fixedsize", values = TRUE)

my_graph %>% export_graph(file_name=output, width=1500, height=1000)	
}



