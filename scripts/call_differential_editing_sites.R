#gets stats for our editing sites
rm(list=ls())

stability_value = 0.03 #value below which you may use a lower coverage for adding more samples to increase power
min_autism_people = 5 #min number people supporting higher coverage for whch you may base stability off measurements off of
min_normal_people = 5  #min number normal poeple supporting higher coverage for which you may base stability off of
min_autism_people_5_cov = 10 #min autism number of people of 5 coverage you must have if needing to use unstable 5x coverage
min_normal_people_5_cov = 10 #min normal number of people of 5 coverage you must have if needing to use unstable 5x coverage
config_file = '../data/config_file'
editing_file= '../data/editing_sites.txt'
output_file = '../results/editing_sites.with_stats.txt'

#######DONE CANGING PARAMETERS###################

#read in files
editing_table = read.table(editing_file,sep="\t",header=TRUE,quote="\"",check.names=FALSE,stringsAsFactors=FALSE)
config_table = read.table(config_file,sep="\t",header=FALSE,stringsAsFactors=FALSE)

all_people = config_table$V1
autism_people = with(config_table,config_table[V2=="autism",]$V1)
normal_people = with(config_table,config_table[V2=="normal",]$V1)

#now get just an editing table and coverage table
edit_level_table = editing_table[,c(all_people)]
cov_table = apply(edit_level_table,2,function(i){
	info = strsplit(i,"\\^",perl=TRUE)	
	editing_levels = sapply(info,function(j)j[3])
	return(editing_levels)
})
cov_table = apply(cov_table,2,as.numeric)

edit_level_table = apply(edit_level_table,2,function(i){
	info = strsplit(i,"\\^",perl=TRUE)	
	editing_levels = sapply(info,function(j)j[1])
	return(editing_levels)
})
edit_level_table = apply(edit_level_table,2,as.numeric)

#go down line by line and get the prevalence info and mean editing levels based off of stable coverages
coverage_threshold_used = c(rep(0,nrow(edit_level_table))) #will hold the coverage threshold required for this editing site
stability_based_on = c(rep(0,nrow(edit_level_table))) #will hold what coverage stability requirements were determined
stable_mean_autism_editing_level = c(rep(0,nrow(edit_level_table))) #mean autistic editing level using individuals passing coverage threshold
stable_std_dev_autism_editing_level = c(rep(0,nrow(edit_level_table))) #standard deviation of autistic editing level using individuals passing coverage threshold
stable_mean_normal_editing_level = c(rep(0,nrow(edit_level_table))) #mean normal editing level using individuals passing coverage threshold
stable_std_dev_normal_editing_level = c(rep(0,nrow(edit_level_table))) #standard deviation of normal editing level using individuals passing coverage threshold
stable_number_autism_with_at_least_min_coverage = c(rep(0,nrow(edit_level_table))) #number of autistic individuals passing the coverage threshold
stable_number_autism_nonzero_editing_and_min_coverage = c(rep(0,nrow(edit_level_table))) #number of autistic individuals without non zero editing level and passing coverage threshold
stable_autism_prevalence = c(rep(0,nrow(edit_level_table))) #proportion autistic individuals with nonzero editing
stable_number_normal_with_at_least_min_coverage = c(rep(0,nrow(edit_level_table))) #same as autism but for normal subjects
stable_number_normal_nonzero_editing_and_min_coverage = c(rep(0,nrow(edit_level_table)))
stable_normal_prevalence = c(rep(0,nrow(edit_level_table)))
stable_total_number_individuals_nonzero_editing_and_min_coverage = c(rep(0,nrow(edit_level_table))) #total number of autism and normal subjects passing the coverage threshold and having nonzero editing level
stable_mann_whitney_p_value = c(rep(0,nrow(edit_level_table))) #wilcoxon rank sum test p value using individuals passing the coverage threshold
stable_editing_level_effect_size = c(rep(0,nrow(edit_level_table))) #difference between mean autism and mean control
stable_frequency_fishers_p_value = c(rep(0,nrow(edit_level_table))) #prevalence p value determined using two-tailed fisher's exact test
stable_frequency_OR = c(rep(0,nrow(edit_level_table))) #odds ratio of the fisher's exact teest
stable_prevalence_effect_size = c(rep(0,nrow(edit_level_table))) #difference in editing level prevalences between autism and normal subjects

for(i in 1:nrow(edit_level_table)){
	print(i)  #keep track of progress
	autism_edit_row = edit_level_table[i,c(autism_people)]
	normal_edit_row = edit_level_table[i,c(normal_people)]
	autism_cov_row = cov_table[i,c(autism_people)]
	normal_cov_row = cov_table[i,c(normal_people)]
	#find what coverage we can base stability off of
	number_autism_20_cov = sum(autism_cov_row >= 20,na.rm=TRUE)
	number_normal_20_cov = sum(normal_cov_row >=20, na.rm=TRUE)
	number_autism_15_cov = sum(autism_cov_row >= 15,na.rm=TRUE)
	number_normal_15_cov = sum(normal_cov_row >= 15,na.rm=TRUE)
	number_autism_10_cov = sum(autism_cov_row >= 10,na.rm=TRUE)
	number_normal_10_cov = sum(normal_cov_row >= 10,na.rm=TRUE)
	number_autism_5_cov = sum(autism_cov_row >= 5,na.rm=TRUE)
	number_normal_5_cov = sum(normal_cov_row >= 5,na.rm=TRUE)
	if(number_autism_20_cov >= min_autism_people & number_normal_20_cov >= min_normal_people){
		stability_based_on[i] = 20
	}else if(number_autism_15_cov >= min_autism_people & number_normal_15_cov >= min_normal_people){
		stability_based_on[i] = 15
	}else if(number_autism_10_cov >= min_autism_people & number_normal_10_cov >= min_normal_people){
		stability_based_on[i] = 10
	}else if(number_autism_5_cov >= min_autism_people_5_cov & number_normal_5_cov >= min_normal_people_5_cov){
		stability_based_on[i] = 5
	}else{ 
		stability_based_on[i] = NA
	}

	#need to deal with cases where there just are not enough autism individuals or normal individuals to calculate mean
	if(is.na(stability_based_on[i])){

		coverage_threshold_used[i] = 5 #I warn users not to use editing sites that don't have any stability_based_on measurement. We include min coverage of 5 just to get statistical information anyways
		#stable_min_cov=5
	#otherwise we can now try to find the stable_min_cov that'll be used for calculation of all statistics
	}else{   
		current_stability_cov =  stability_based_on[i]
		stability_autism_mean = mean(autism_edit_row[autism_cov_row >= current_stability_cov],na.rm=TRUE)
		stability_normal_mean = mean(normal_edit_row[normal_cov_row >= current_stability_cov],na.rm=TRUE)
		for(j in seq(5,stability_based_on[i],5)){
			autism_mean = mean(autism_edit_row[autism_cov_row >= j],na.rm=TRUE)	
			normal_mean = mean(normal_edit_row[normal_cov_row >= j],na.rm=TRUE)
		if(abs(autism_mean-stability_autism_mean) <=stability_value & abs(normal_mean-stability_normal_mean) <=stability_value){
				coverage_threshold_used[i] = j	
				break
			}
		}
	}
	#now let's calculate all our statics based on the stable coverage threshold
	stable_min_cov = coverage_threshold_used[i]
	autism_adju_edit_row = autism_edit_row[!is.na(autism_edit_row) & !is.na(autism_cov_row) & autism_cov_row >= stable_min_cov]
	autism_adju_cov_row = autism_cov_row[!is.na(autism_cov_row) & autism_cov_row >= stable_min_cov]
	normal_adju_edit_row = normal_edit_row[!is.na(normal_edit_row) & !is.na(normal_cov_row) & normal_cov_row >= stable_min_cov]
	normal_adju_cov_row = normal_cov_row[!is.na(normal_cov_row) & normal_cov_row >= stable_min_cov]
	stable_mean_autism_editing_level[i] = mean(autism_adju_edit_row,na.rm=TRUE)
	stable_std_dev_autism_editing_level[i] = sd(autism_adju_edit_row,na.rm=TRUE)
	stable_mean_normal_editing_level[i] = mean(normal_adju_edit_row,na.rm=TRUE)
	stable_std_dev_normal_editing_level[i] = sd(normal_adju_edit_row,na.rm=TRUE)
	stable_number_autism_with_at_least_min_coverage[i] = sum(autism_adju_cov_row >=stable_min_cov,na.rm=TRUE)
	stable_number_autism_nonzero_editing_and_min_coverage[i] = sum(!is.na(autism_adju_cov_row) & autism_adju_cov_row >= stable_min_cov & autism_adju_edit_row > 0,na.rm=TRUE)
	stable_autism_prevalence[i] = stable_number_autism_nonzero_editing_and_min_coverage[i]/stable_number_autism_with_at_least_min_coverage[i]
	stable_number_normal_with_at_least_min_coverage[i] = sum(normal_adju_cov_row >=stable_min_cov,na.rm=TRUE)
	stable_number_normal_nonzero_editing_and_min_coverage[i] = sum(!is.na(normal_adju_cov_row) & normal_adju_cov_row >= stable_min_cov & normal_adju_edit_row > 0,na.rm=TRUE)
	stable_normal_prevalence[i] = stable_number_normal_nonzero_editing_and_min_coverage[i]/stable_number_normal_with_at_least_min_coverage[i]
	stable_total_number_individuals_nonzero_editing_and_min_coverage[i] = sum(stable_number_autism_nonzero_editing_and_min_coverage[i] + stable_number_normal_nonzero_editing_and_min_coverage[i])
	if(length(autism_adju_edit_row) >=1 & length(normal_adju_edit_row) >=1){
		stable_mann_whitney_p_value[i] = wilcox.test(autism_adju_edit_row,normal_adju_edit_row)$p.value
	}else{
		stable_mann_whitney_p_value[i] = NA
	}
	stable_editing_level_effect_size[i] =  abs(stable_mean_autism_editing_level[i] - stable_mean_normal_editing_level[i])
	#now frequency test
	fisher_matrix = matrix(c(stable_number_autism_nonzero_editing_and_min_coverage[i],stable_number_normal_nonzero_editing_and_min_coverage[i],stable_number_autism_with_at_least_min_coverage[i]-stable_number_autism_nonzero_editing_and_min_coverage[i],stable_number_normal_with_at_least_min_coverage[i]-stable_number_normal_nonzero_editing_and_min_coverage[i]),nrow=2)
	fisher_info = fisher.test(fisher_matrix)
	stable_frequency_fishers_p_value[i] = fisher_info$p.value
	stable_frequency_OR[i] = fisher_info$estimate
	stable_prevalence_effect_size[i] = abs(stable_autism_prevalence[i] - stable_normal_prevalence[i])
}

#now put everything back together as a table
header_info = editing_table[,c('chromosome','position','type_editing')]
stats_table = data.frame(coverage_threshold_used,stability_based_on,stable_mean_autism_editing_level,stable_std_dev_autism_editing_level,stable_mean_normal_editing_level,stable_std_dev_normal_editing_level,stable_number_autism_with_at_least_min_coverage,stable_number_autism_nonzero_editing_and_min_coverage,stable_autism_prevalence,stable_number_normal_with_at_least_min_coverage,stable_number_normal_nonzero_editing_and_min_coverage,stable_normal_prevalence,stable_total_number_individuals_nonzero_editing_and_min_coverage,stable_mann_whitney_p_value,stable_editing_level_effect_size,stable_frequency_fishers_p_value,stable_frequency_OR,stable_prevalence_effect_size)
full_table = do.call(cbind,list(header_info,stats_table,editing_table[,c(all_people)]))

#write the full_table to output
write.table(full_table,file=output_file,row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)

cat("job completed\n")

