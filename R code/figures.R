# The code uses the pipe %>% all along
# Load magrittr before running anything
# All the packages not already in base R are indicated along the code as follows 'package::function'
# Installl them BEFORE running the code

# The script d_manage includes some data management functions
# Load them with 'source("PATH/d_mange.R")'

# The following code has bee used to generate the figures/tables in the manuscript and the supplemental information
# All the final details added on other softwares such as adobe illustrator are obviously not present in this code

# Figure 1
##########

# Figure 1 was produced on adobe illustrator

# Figure 2
##########

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

# Additional details were modified on adobe illustrator and are not generated through R code

figure2 <- function(file=file.path(path.workspace, "fig2.tiff")){
	cat("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	cat("Plotting...\n")

	per_m <- function(m, val){
		perc <- ecdf(sort(val))
		return(perc(m))
	}

	data <- rbind(data2015, data2016) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), NA,
					ifelse(phone==1, "Mobile phone owners", "Non-phone owners"))) %>%
				dplyr::select(phone, clinic_time, dest) %>%
				tidyr::gather(., var, value, clinic_time:dest) %>%
				dplyr::mutate(
					var=ifelse(var=="clinic_time", "Travel time to\nhealth care (hours)", "Number of travel\ndestinations"),
					value2=ifelse(phone=="Mobile phone\nowners", value, -value)) %>%
				dplyr::filter(!is.na(phone) & !is.na(value)) %>%
				dplyr::group_by(phone, var) %>%
				dplyr::mutate(
					m=mean(value),
					m_p=per_m(m, value))

	fig2 <- ggplot2::ggplot() +
				ggridges::geom_density_ridges(
					data=data,
					ggplot2::aes(x=value, y=phone, color=phone, fill=phone),
					scale=0.9,
					alpha=0.4,
					size=1,
					jittered_points = TRUE,
				    position = ggridges::position_points_jitter(width = 0.15, height = 0),
				    point_shape = '|',
				    point_size = 3,
				    point_alpha = 1,
				    quantile_lines=TRUE,
				    quantile_fun=mean) +
				ggplot2::scale_colour_brewer(
					name="",
					palette="Set1") +
				ggplot2::scale_fill_brewer(
					name="",
					palette="Set1") +
				ggplot2::facet_wrap(
					.~var,
					ncol=2,
					scales="free_x") +
				# ggplot2::coord_flip() +
				ggplot2::xlab("") +
				ggplot2::ylab("") +
				ggplot2::theme_bw() +
				ggplot2::theme(legend.position="none")

	ggplot2::ggsave(
		file,
		fig2,
		width=10,
		height=6
		)
}

# Figure 3
##########

# boot is the number of resampling to estimate 95% confidence intervals

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

# Additional details were modified on adobe illustrator and are not generated through R code

figure3 <- function(boot=1000, file=file.path(path.workspace, "fig3.tiff")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	note("Multiple imputations...\n")
	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150, print=FALSE)

	note("Discrete kernel smoothing...\n")
	NA_clean <- function(x){
		return(ifelse(is.na(x), 0, x))
	}

	# Phone owers
	## Men
	max_t_p_m <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::filter(sex=="M" & phone==1)
					return(max(temp$clinic_time))		
	}) %>%
	do.call("c", .)

	min_t_p_m <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::filter(sex=="M" & phone==1)
					return(min(temp$clinic_time))		
	}) %>%
	do.call("c", .)

	data_temp <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									dplyr::filter(sex=="M" & phone==1)
						kern_temp <- kde1d::kde1d(ordered(temp$clinic_time, levels=min_t_p_m[x]:max_t_p_m[x]))

						return(
							data.frame(
								clinic_time=min_t_p_m[x]:max_t_p_m[x],
								pm=kde1d::dkde1d(min_t_p_m[x]:max_t_p_m[x], kern_temp)) %>%
								setNames(., c("clinic_time", paste0("pm", x)))
							)
					})

	data_imp_p_d_M <- data_temp[[1]]
	for(i in 2:length(data_temp)){
		data_imp_p_d_M <- dplyr::full_join(
							data_imp_p_d_M,
							data_temp[[i]],
							by="clinic_time")
	}

	data_imp_p_d_M[,-1] <- apply(data_imp_p_d_M[,-1], 2, NA_clean)
	# Pooling probability mass
	data_imp_p_d_M$pm <- rowMeans(data_imp_p_d_M[,-1])

	## women
	max_t_p_f <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::filter(sex=="F" & phone==1)
					return(max(temp$clinic_time))		
	}) %>%
	do.call("c", .)

	min_t_p_f <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::filter(sex=="F" & phone==1)
					return(min(temp$clinic_time))		
	}) %>%
	do.call("c", .)

	data_temp <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									dplyr::filter(sex=="F" & phone==1)
						kern_temp <- kde1d::kde1d(ordered(temp$clinic_time, levels=min_t_p_f[x]:max_t_p_f[x]))

						return(
							data.frame(
								clinic_time=min_t_p_f[x]:max_t_p_f[x],
								pm=kde1d::dkde1d(min_t_p_f[x]:max_t_p_f[x], kern_temp)) %>%
								setNames(., c("clinic_time", paste0("pm", x)))
							)
					})

	data_imp_p_d_F <- data_temp[[1]]
	for(i in 2:length(data_temp)){
		data_imp_p_d_F <- dplyr::full_join(
							data_imp_p_d_F,
							data_temp[[i]],
							by="clinic_time")
	}

	data_imp_p_d_F[,-1] <- apply(data_imp_p_d_F[,-1], 2, NA_clean)
	# Pooling probability mass
	data_imp_p_d_F$pm <- rowMeans(data_imp_p_d_F[,-1])

	# Non-owners
	## Men
	max_t_np_m <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::filter(sex=="M" & phone==0)
					return(max(temp$clinic_time))		
	}) %>%
	do.call("c", .)

	min_t_np_m <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::filter(sex=="M" & phone==0)
					return(min(temp$clinic_time))		
	}) %>%
	do.call("c", .)

	data_temp <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									dplyr::filter(sex=="M" & phone==0)
						kern_temp <- kde1d::kde1d(ordered(temp$clinic_time, levels=min_t_np_m[x]:max_t_np_m[x]))

						return(
							data.frame(
								clinic_time=min_t_np_m[x]:max_t_np_m[x],
								pm=kde1d::dkde1d(min_t_np_m[x]:max_t_np_m[x], kern_temp)) %>%
								setNames(., c("clinic_time", paste0("pm", x)))
							)
					})

	data_imp_np_d_M <- data_temp[[1]]
	for(i in 2:length(data_temp)){
		data_imp_np_d_M <- dplyr::full_join(
							data_imp_np_d_M,
							data_temp[[i]],
							by="clinic_time")
	}

	data_imp_np_d_M[,-1] <- apply(data_imp_np_d_M[,-1], 2, NA_clean)
	# Pooling probability mass
	data_imp_np_d_M$pm <- rowMeans(data_imp_np_d_M[,-1])

	## Women
	max_t_np_f <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::filter(sex=="F" & phone==0)
					return(max(temp$clinic_time))		
	}) %>%
	do.call("c", .)

	min_t_np_f <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::filter(sex=="F" & phone==0)
					return(min(temp$clinic_time))		
	}) %>%
	do.call("c", .)

	data_temp <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									dplyr::filter(sex=="F" & phone==0)
						kern_temp <- kde1d::kde1d(ordered(temp$clinic_time, levels=min_t_np_f[x]:max_t_np_f[x]))

						return(
							data.frame(
								clinic_time=min_t_np_f[x]:max_t_np_f[x],
								pm=kde1d::dkde1d(min_t_np_f[x]:max_t_np_f[x], kern_temp)) %>%
								setNames(., c("clinic_time", paste0("pm", x)))
							)
					})

	data_imp_np_d_F <- data_temp[[1]]
	for(i in 2:length(data_temp)){
		data_imp_np_d_F <- dplyr::full_join(
							data_imp_np_d_F,
							data_temp[[i]],
							by="clinic_time")
	}

	data_imp_np_d_F[,-1] <- apply(data_imp_np_d_F[,-1], 2, NA_clean)
	# Pooling probability mass
	data_imp_np_d_F$pm <- rowMeans(data_imp_np_d_F[,-1])

	# Pooled estimate of mean travel time to a health care center
	mean_rubin <- function(data=data_imp){
		temp_m <- lapply(1:15, function(x){
			temp <- mice::complete(data, x) %>%
						dplyr::group_by(sex, phone) %>%
						dplyr::summarise(clinic_time=mean(clinic_time))
			return(temp)
		}) %>%
		do.call("rbind", .) %>%
		dplyr::group_by(sex, phone) %>%
		dplyr::summarise(clinic_time=mean(clinic_time))
		return(temp_m)
	}

	cat_mean <- mean_rubin()

	note("Plotting Figure 3A...\n")
	phone_sex_k <- rbind(
				data.frame(
					sex="M",
					phone=1,
					x=data_imp_p_d_M$clinic_time,
					y=data_imp_p_d_M$pm),
				data.frame(
					sex="F",
					phone=1,
					x=data_imp_p_d_F$clinic_time,
					y=data_imp_p_d_F$pm),
				data.frame(
					sex="M",
					phone=0,
					x=data_imp_np_d_M$clinic_time,
					y=data_imp_np_d_M$pm),
				data.frame(
					sex="F",
					phone=0,
					x=data_imp_np_d_F$clinic_time,
					y=data_imp_np_d_F$pm)
				)

	fig3a <- ggplot2::ggplot() +
				ggplot2::geom_bar(
					data=phone_sex_k %>%
						dplyr::mutate(
							phone=ifelse(phone==1, "Mobile phone owners", "Non-phone owners"),
							sex=ifelse(sex=="M", "Men", "Women")),
					ggplot2::aes(x=x, y=y, color=phone, fill=phone),
					stat="identity",
					alpha=0.3) +
				ggplot2::geom_vline(
					data=cat_mean %>%
						dplyr::mutate(
							phone=ifelse(phone==1, "Mobile phone owners", "Non-phone owners"),
							sex=ifelse(sex=="M", "Men", "Women")),
					ggplot2::aes(xintercept=clinic_time, color=phone),
					size=1,
					linetype="dashed") +
				ggh4x::facet_nested(.~phone+sex) +
				ggplot2::scale_colour_brewer(
					name="",
					palette="Set1") +
				ggplot2::scale_fill_brewer(
					name="",
					palette="Set1") +
				ggplot2::xlab("Travel time to healthcare (hours)") +
				ggplot2::ylab("Probability mass") +
				ggplot2::xlim(c(0, 15)) +
				ggplot2::theme_bw() +
				ggplot2::theme(legend.position="none")

	note("Estimating mean tavel time to a health care center for phone owners and the whole sample...\n")
	BS_imput <- function(data=rbind(data2015, data2016)){
		 temp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
		 temp_i <- mice::mice(temp, m=1, print=FALSE)
		 return(cbind(mice::complete(temp_i, 1), id=temp$id))
	}

	estim <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time=mean(clinic_time)) %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time=mean(clinic_time)) %>%
						data.frame()

		mean_time <- data.frame(
						total=data_total_1$clinic_time,
						phone=data_phone_1$clinic_time[data_phone_1$phone==1],
						nphone=data_phone_1$clinic_time[data_phone_1$phone==0])

		return(mean_time)
	}

	mean_rubin_mix <- function(data=data_imp){
		temp <- lapply(1:15, function(x){
			temp_p <- mice::complete(data, x) %>%
						dplyr::filter(phone==1) %>%
						dplyr::summarise(
							clinic_time=mean(clinic_time)) %>%
						dplyr::mutate(phone="Mobile phone owners")
			temp_g <- mice::complete(data, x) %>%
						dplyr::summarise(
							clinic_time=mean(clinic_time)) %>%
						dplyr::mutate(phone="General population")

			return(rbind(temp_p, temp_g))
		}) %>%
		do.call("rbind", .) %>%
		dplyr::group_by(phone) %>%
		dplyr::summarise(
			clinic_time=mean(clinic_time))
		return(temp)
	}

	set.seed(150)
	boot_imput <- replicate(boot, BS_imput(rbind(data2015, data2016)), simplify=FALSE)
	est <- lapply(boot_imput, estim) %>%
			do.call("rbind", .)

	mean_mix <- mean_rubin_mix() %>%
					dplyr::mutate(
						l=ifelse(phone=="Mobile phone owners", quantile(est$phone, 0.025), quantile(est$total, 0.025)),
						up=ifelse(phone=="Mobile phone owners", quantile(est$phone, 0.975), quantile(est$total, 0.975)))


	note("Weighted average of probability masses for phone owners and general population...\n")
	mean_rubin_alt <- function(data=data_imp){
		temp_m <- lapply(1:15, function(x){
			temp <- mice::complete(data, x) %>%
						dplyr::mutate(
							prop_phone=mean(phone),
							sex=ifelse(sex=="M", 1, 0)) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							prop_M=mean(sex),
							prop_phone=mean(prop_phone)) %>%
						dplyr::mutate(
							prop_phone=ifelse(phone==0, 1-prop_phone, prop_phone))
			return(temp)
		}) %>%
		do.call("rbind", .) %>%
		dplyr::group_by(phone) %>%
		dplyr::summarise(
			prop_M=mean(prop_M),
			prop_phone=mean(prop_phone))
		return(temp_m)
	}

	mix <- mean_rubin_alt()

	w_avg_pm <- dplyr::full_join(
					data_imp_p_d_M %>%
						dplyr::select(clinic_time, pm1=pm),
					data_imp_p_d_F %>%
						dplyr::select(clinic_time, pm2=pm),
					by="clinic_time") %>%
					dplyr::mutate(
						pm1=NA_clean(pm1),
						pm2=NA_clean(pm2),
						x=clinic_time) %>%
					dplyr::group_by(x) %>%
					dplyr::summarize(
						y=mix$prop_M[mix$phone==1]*pm1+(1-mix$prop_M[mix$phone==1])*pm2) %>%
					dplyr::mutate(
						phone="Mobile phone owners") %>%
					rbind(.,
						dplyr::full_join(
							data_imp_p_d_M %>%
								dplyr::select(clinic_time, pm1=pm),
							data_imp_p_d_F %>%
								dplyr::select(clinic_time, pm2=pm),
							by="clinic_time") %>%
						dplyr::full_join(.,
							data_imp_np_d_M %>%
								dplyr::select(clinic_time, pm3=pm),
								by="clinic_time") %>%
						dplyr::full_join(.,
							data_imp_np_d_F %>%
								dplyr::select(clinic_time, pm4=pm),
								by="clinic_time") %>%
							dplyr::mutate(
								pm1=NA_clean(pm1),
								pm2=NA_clean(pm2),
								pm3=NA_clean(pm3),
								pm4=NA_clean(pm4),
								x=clinic_time) %>%
							dplyr::group_by(x) %>%
							dplyr::summarize(
								y=mix$prop_phone[mix$phone==1]*(mix$prop_M[mix$phone==1]*pm1+(1-mix$prop_M[mix$phone==1])*pm2)+mix$prop_phone[mix$phone==0]*(mix$prop_M[mix$phone==0]*pm3+(1-mix$prop_M[mix$phone==0])*pm4)) %>%
							dplyr::mutate(
								phone="General population"))


	note("Plotting Figure 3B...\n")
	fig3b <- ggplot2::ggplot() +
				ggplot2::geom_step(
					data=w_avg_pm,
					ggplot2::aes(x=x, y=y, color=phone, size=phone),
					stat="identity",
					position="identity"
					) +
				ggplot2::geom_vline(
					data=mean_mix,
					ggplot2::aes(xintercept=clinic_time, color=phone, size=phone),
					linetype="dashed",
					size=1) +
				ggplot2::scale_size_manual(
					name="",
					# values=c(1.5, 1)) +
					values=c(1.8, 1.5)) +
				ggplot2::scale_color_manual(
					name="",
					values=rev(c(RColorBrewer::brewer.pal(2, "Set1")[1], "darkviolet")),
					breaks=rev(c("Mobile phone owners", "General population"))) +
				ggplot2::xlab("Travel time to healthcare (hours)") +
				ggplot2::ylab("Probability mass") +
				ggplot2::guides(size="none") +
				ggplot2::xlim(c(-1, 15)) +
				ggplot2::theme_bw() +
				ggplot2::theme(
					legend.position="none",
					axis.title.x = ggplot2::element_blank(),
					axis.ticks.x = ggplot2::element_blank(),
					axis.text.x = ggplot2::element_blank())

	note("Plotting Figure 3C...\n")
	fig3c <- ggplot2::ggplot(
				data=mean_mix) +
				ggplot2::geom_errorbarh(
					ggplot2::aes(xmin=l, xmax=up, y=phone, color=phone),
					height=0.6,
					size=1) +
				ggplot2::geom_point(
					ggplot2::aes(x=clinic_time, y=phone, color=phone),
					size=4) +
				ggplot2::geom_vline(
					data=mean_mix,
					ggplot2::aes(xintercept=clinic_time, color=phone, size=phone),
					linetype="dashed",
					alpha=0.6
					) +
				ggplot2::scale_size_manual(
					name="",
					values=c(1, 1)) +
				ggplot2::scale_color_manual(
					name="",
					values=c(RColorBrewer::brewer.pal(2, "Set1")[1], "darkviolet"),
					breaks=c("Mobile phone owners", "General population")) +
				ggplot2::xlab("Travel time to healthcare (hours)") +
				ggplot2::ylab("") +
				ggplot2::xlim(c(0, 15)) +
				ggplot2::theme_bw() +
				ggplot2::theme(legend.position="none")


	note("Plotting Figure 3D...\n")
	fig3d <- ggplot2::ggplot() +
				ggplot2::geom_bar(
					data=w_avg_pm %>%
						dplyr::group_by(x) %>%
						dplyr::summarize(y=-diff(y)),
					ggplot2::aes(x=x, y=y, fill=y),
					color="darkgrey",
					stat="identity",
					position="identity"
					) +
				ggplot2::geom_hline(
					yintercept=0,
					linetype="dashed",
					size=1) +
				ggplot2::scale_fill_gradient2(
					name="",
					low="blue",
					high="red",
					midpoint=0) +
				ggplot2::xlab("Travel time to healthcare (hours)") +
				ggplot2::ylab("Difference in\nprobability mass") +
				ggplot2::guides(size="none") +
				ggplot2::xlim(c(-1, 15)) +
				ggplot2::theme_bw() +
				ggplot2::theme(
					legend.position="none")

	ggplot2::ggsave(
		file,
		cowplot::plot_grid(
			fig3a,
			cowplot::plot_grid(
				fig3b,
				fig3c,
				fig3d,
				nrow=3,
				labels=c("B", "C", "D"),
				rel_heights=c(3,1,1),
				align="v"),
			nrow=2,
			labels=c("A", ""),
			rel_heights=c(3,5)),
		width=10,
		height=10
		)
}


# Figure 4 and Table S12
########################

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

# Additional details were modified on adobe illustrator and are not generated through R code
# Table S12 was finalized on excel

figure4_tableS12 <- function(file=file.path(path.workspace, "fig4.tiff"), table=file.path(path.workspace, "tableS12.csv")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	nodes2016$phone_use_ever <- ifelse(nodes2016$phone_use_ever=="No data", NA,
									ifelse(nodes2016$phone_use_ever=="Already used a phone", "Yes", "No"))

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015$phone[nodes2015$id %in% c(96:98)] <- "Children"
	nodes2015 <- dplyr::filter(nodes2015, !(is.na(age) & id %in% c(96:98)))

	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	nodes2015$phone_use_ever <- ifelse(nodes2015$phone_use_ever=="No data", NA,
									ifelse(nodes2015$phone_use_ever=="Already used a phone", "Yes", "No"))

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(
						mob_dest0, mob_dest1, mob_dest2,
						mob_dest3, mob_dest4, mob_dest5, id, phone) %>%
					dplyr::mutate(year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(
						mob_dest0, mob_dest1, mob_dest2,
						mob_dest3, mob_dest4, mob_dest5, id, phone) %>%
					dplyr::mutate(year=2016)

	phone_yes <- c("Etanga", "Etengwa", "Okahama",
		"Okahua", "Ombutisauri", "Oromukandi", "Otjihende",
		"Otjituka", "Ungaraviho", "Walvis Bay", "Ohungumure",
		"Orombamba", "Ovillai", "Okapawe", "Karaviho",
		"Okanguati", "Epupa", "Oshakati")
	phone_no <- c("Okahua", "Omauatjivingo", "Ombepera",
		"Ombuyuwandume", "Omuatjivingo", "Otjihende",
		"Otjituka", "Otjizu", "Otutati", "Omaheke",
		"Otjandjeu", "Ongundi", "Roidrum", "Abroad")
	phone_mountain <- c("Embwende", "Okahua", "Orokatuwo",
		"Etengwa", "Okakora", "Ombuyuwandume", "Omuatjivingo",
		"Otjikoyo", "Otjitanda", "Otutati", "Ozija",
		"Ozohorongo", "Ozowonduombe", "Ovohungu",
		"Otjihipu", "Orukaue", "Otjize")
	phone_not_mountain <- c("Ohondungu", "Omuramba", "Omuhonga",
		"Etengwa", "Okakora", "Okanyandi", "Okapara",
		"Ongondjonambari", "Opuwo", "Oromukandi",
		"Otjinungwa", "Windhoek", "Ombanzu",
		"Okakondorokwa", "Ovireva dam",
		"Omungwiti Wakekoro", "Likokola")

	data_dest <- rbind(data2015, data2016) %>%
					tidyr::gather(., num, dest, mob_dest0:mob_dest5) %>%
					dplyr::group_by(phone, dest) %>%
					dplyr::summarize(N=dplyr::n()) %>%
					dplyr::arrange(phone, -N) %>%
					dplyr::filter(!is.na(dest))

	data_dest <- data_dest %>%
					dplyr::mutate(
						signal=ifelse(dest %in% c(phone_yes, phone_not_mountain), "Phone access widely and easily available",
							ifelse(dest %in% phone_mountain, "Phone access limited\nand difficult to access\n(up a montain)",
								ifelse(dest %in% phone_no, "No phone access",
									"Phone access unlikely/No information")))) %>%
					dplyr::mutate(
						signal=factor(signal,
								levels=c("Phone access unlikely/No information",
									"No phone access",
									"Phone access limited\nand difficult to access\n(up a montain)",
									"Phone access widely and easily available")))

	note("Generating Table S12...\n")
	data_dest %>%
		dplyr::filter(phone!="No data") %>%
		dplyr::filter(phone=="Phone") %>%
		dplyr::full_join(
			.,
			data_dest %>%
				dplyr::filter(phone!="No data") %>%
				dplyr::filter(phone=="No phone"),
			by="dest") %>%
		write.csv(
			.,
			table,
			row.names=FALSE)


	ord_names <- rbind(data2015, data2016) %>%
					tidyr::gather(., num, dest, mob_dest0:mob_dest5) %>%				dplyr::group_by(phone, dest) %>%
					dplyr::group_by(dest) %>%
					dplyr::summarize(N=dplyr::n()) %>%
					dplyr::mutate(
						signal=ifelse(dest %in% c(phone_yes, phone_not_mountain), "Phone access widely and easily available",
							ifelse(dest %in% phone_mountain, "Phone access limited\nand difficult to access\n(up a montain)",
								ifelse(dest %in% phone_no, "No phone access",
									"Phone access unlikely/No information")))) %>%
					dplyr::mutate(
						signal=factor(signal,
								levels=c("Phone access unlikely/No information",
									"No phone access",
									"Phone access limited\nand difficult to access\n(up a montain)",
									"Phone access widely and easily available"))) %>%
					dplyr::arrange(signal, -N) %>%
					dplyr::filter(!is.na(dest)) %>%
					dplyr::select(dest)

	data_dest_dist <- data_dest %>%
					dplyr::mutate(dest=factor(dest,
							levels=rev(ord_names$dest))) %>%
					dplyr::mutate(
						log=ifelse(phone=="No phone", "darkgrey",
							ifelse(signal %in% c("Phone access limited\nand difficult to access\n(up a montain)",
									"Phone access widely and easily available"), "lightgrey", "darkgrey"))) %>%
					dplyr::group_by(log) %>%
					dplyr::mutate(
						p=100*N/sum(N))

	data_dest <- data_dest %>%
					dplyr::mutate(dest=factor(dest,
							levels=rev(ord_names$dest))) %>%
					dplyr::group_by(phone) %>%
					dplyr::mutate(p=100*N/sum(N)) %>%
					dplyr::ungroup() %>%
					dplyr::mutate(
						log=ifelse(phone=="No phone", "darkgrey",
							ifelse(signal %in% c("Phone access limited\nand difficult to access\n(up a montain)",
									"Phone access widely and easily available"), "lightgrey", "darkgrey")))

	data_dest_cum <- data_dest %>%
						dplyr::filter(phone!="No data") %>%
						dplyr::mutate(
							signal=factor(signal,
									levels=rev(c("Phone access unlikely/No information",
										"No phone access",
										"Phone access limited\nand difficult to access\n(up a montain)",
										"Phone access widely and easily available")))) %>%
						dplyr::group_by(signal) %>%
						dplyr::summarize(
							y=sum(N),
							n=dplyr::n()) %>%
						dplyr::mutate(
							prop=n/sum(n),
							fill=ifelse(
								signal=="Phone access widely and easily available", RColorBrewer::brewer.pal(4, "Greens")[4],
								ifelse(signal=="Phone access limited\nand difficult to access\n(up a montain)", RColorBrewer::brewer.pal(4, "Greens")[3],
									ifelse(signal=="No phone access", RColorBrewer::brewer.pal(4, "Greens")[2],
										NA))))

	note("Plotting Figure 4...\n")
	fig4 <- ggplot2::ggplot() +
				ggplot2::geom_rect(
					data=data_dest_cum,
					ggplot2::aes(
						xmin=-Inf,
						xmax=Inf,
						ymin=-Inf,
						ymax=Inf,
						fill=I(fill)),
					alpha=0.3) +
				ggplot2::geom_bar(
					data=data_dest %>%
					dplyr::filter(phone!="No data"),
					ggplot2::aes(x=ifelse(phone=="Phone", -N, N), y=dest, fill=I(log)),
					stat="identity") +
				ggh4x::facet_manual(
					.~signal,
					matrix(
						c(rep(1, each=as.integer(100*data_dest_cum$prop[1])),
							rep(2, each=as.integer(100*data_dest_cum$prop[2])),
							rep(3, each=as.integer(100*data_dest_cum$prop[3])),
							rep(4, each=as.integer(100*data_dest_cum$prop[4]))),
						ncol=1),
					strip.position="left",
					scale="free_y") +
				ggplot2::ylab("Travel destinations") +
				ggplot2::xlab("Number of travelers") +
				ggplot2::geom_vline(
					xintercept=0,
					size=0) +
				ggplot2::scale_x_continuous(
				 	breaks=c(-20, -10, 0, 10, 20, 30, 40),
				 	labels = as.character(c(20, 10, 0, 10, 20, 30, 40))) +
				ggplot2::theme_bw() +
				ggplot2::theme(
					legend.position="none",
					panel.spacing.y=ggplot2::unit(0, "lines"),
					panel.border = ggplot2::element_blank(),
					axis.text.y=ggplot2::element_blank(),
					axis.ticks.y=ggplot2::element_blank())

	ggplot2::ggsave(
		file,
		fig4,
		width=12.6,
		height=20
		)
}

# Figure S1
###########

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

figureS1 <- function(file=file.path(path.workspace, "figS1.tiff")){
	note("Data management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	# l_list <- unique(c(links2015$x, links2015$y))
	# temp <- net2015[[2]][, c("id", "age", "sex")] %>%
	# 			dplyr::mutate(id=as.integer(id))
	# l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
	# 			dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$surv <- "2015 survey"
	nodes2016$surv <- "2016 survey"

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")))
	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")))

	note("PCA for the whole dataset...\n")
	prglob <- prcomp(model.matrix(formula(paste0("~",
					paste(c("dest", "clinic_time", "clinic_unable", "travel"), collapse="+"))),
				data=rbind(na.omit(data2015),na.omit(data2016)))[,-1])

	eigenv <- tidyr::gather(data.frame(summary(prglob)$rotation)) %>%
					dplyr::mutate(var=factor(rep(row.names(summary(prglob)$rotation), 4)))

	pc <- tidyr::gather(data.frame(summary(prglob)$importance)[2,]) %>%
			dplyr::mutate(key=factor(key, levels = paste0("PC", 1:4)),
				type="2015+2016")

	loading <- eigenv[eigenv$key %in% c("PC1", "PC2"),] %>%
				dplyr::mutate(type="2015+2016")

	note("PCA for the data collection year strata...\n")
	pr2015 <- prcomp(model.matrix(formula(paste0("~",
					paste(c("dest", "clinic_time", "clinic_unable", "travel"), collapse="+"))),
				data=na.omit(data2015))[,-1])
	pr2016 <- prcomp(model.matrix(formula(paste0("~",
					paste(c("dest", "clinic_time", "clinic_unable", "travel"), collapse="+"))),
				data=na.omit(data2016))[,-1])

	eigenv2015 <- tidyr::gather(data.frame(summary(pr2015)$rotation)) %>%
					dplyr::mutate(var=factor(rep(row.names(summary(pr2015)$rotation), 4)))
	eigenv2016 <- tidyr::gather(data.frame(summary(pr2016)$rotation)) %>%
					dplyr::mutate(var=factor(rep(row.names(summary(pr2016)$rotation), 4)))

	pc2015 <- tidyr::gather(data.frame(summary(pr2015)$importance)[2,]) %>%
				dplyr::mutate(key=factor(key, levels = paste0("PC", 1:4)),
				type="2015 stratum")
	pc2016 <- tidyr::gather(data.frame(summary(pr2016)$importance)[2,]) %>%
				dplyr::mutate(key=factor(key, levels = paste0("PC", 1:4)),
				type="2016 stratum")

	note("Plotting Figure S1...\n")
	figS1A <- rbind(pc, pc2015, pc2016) %>%
				dplyr::mutate(
					dum="Principal components",
					type=factor(
						type,
						levels=c("2015 stratum", "2016 stratum", "2015+2016"))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=key, y=value*100)) +
					ggplot2::geom_bar(stat = "identity") +
					ggplot2::ylim(0,100) +
					ggplot2::xlab("") +
					ggplot2::ylab("Percentage of variance explained") +
					ggplot2::facet_grid(type~dum) +
					ggplot2::theme_bw()

	figS1B <- rbind(loading,
					eigenv2015[eigenv2015$key %in% c("PC1", "PC2"),] %>%
						dplyr::mutate(type="2015 stratum"),
					eigenv2016[eigenv2016$key %in% c("PC1", "PC2"),] %>%
						dplyr::mutate(type="2016 stratum")) %>%
					dplyr::mutate(
						var=factor(var,
							levels=c("clinic_unable", "clinic_time", "travelCar", "dest"),
							labels=c("Unable to access a\nhealth care center", "Travel time to\nhealth care",
								"Travel using a car", "Number of\ntravel destinations")),
						type=factor(
							type,
							levels=c("2015 stratum", "2016 stratum", "2015+2016"))) %>%
					ggplot2::ggplot(., ggplot2::aes(x=var, y=value, color=value)) +
						ggplot2::geom_hline(yintercept = 0,
							linetype="dashed") +
						ggplot2::geom_point(size=3) +
						ggplot2::facet_grid(type~key) +
						ggplot2::coord_flip() +
						ggplot2::xlab("") +
						ggplot2::ylab("Loading") +
						ggplot2::scale_colour_gradient2(low="blue", mid="grey",
							high="red", midpoint=0) +
						ggplot2::theme_bw() +
						ggplot2::theme(legend.position = "none")

	figS1 <- multipanelfigure::multi_panel_figure(width=c(3,6), height=6, rows=1, unit = 'in') %>%
			multipanelfigure::fill_panel(figS1A, label='A', row=1, column=1) %>%
			multipanelfigure::fill_panel(figS1B, label='B', row=1, column=2)

	multipanelfigure::save_multi_panel_figure(figS1, file)
}

# Figure S2
###########

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

figureS2 <- function(file=file.path(path.workspace, "figS2.tiff")){
	note("Data management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))

	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$surv <- "2015 survey"
	nodes2016$surv <- "2016 survey"

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)
	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	note("Multiple imputations...\n")
	data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=150, print=FALSE)
		
	note("Iterative trimming...\n")
	trim_iter <- function(imp){
		mod_mat <- stats::model.matrix(
						phone~age+sex+dest+child_house+adult_house+travel+clinic_time+year,
						data=imp)
		imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
		colnames(imp) <- gsub("-", "_", colnames(imp))
		colnames(imp) <- gsub(">=", "_", colnames(imp))
		colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
		colnames(imp) <- gsub("\\.", "_", colnames(imp))
		repeat{
			n <- nrow(imp)
			mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
				data=imp)
			check <- apply(mod_mat, 2, mean) # Initiating a check
			check <- !(check==1 | check==0) & names(check)!="clinic_time"
			form <- paste0("phone~", paste(names(check)[check], collapse="+"))
			mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
			imp$p <- stats::predict(mod, type="response")
			cutoff_low <- min(imp$p[imp$phone==1])
			cutoff_high <- max(imp$p[imp$phone==0])
			imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
			n_trim <- nrow(imp)
			if(n_trim==n){
				break
			}
		}
		return(imp)
	}

	data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	for(i in 1:length(data_imp)){
		data_imp[[i]]$m <- i
	}

	note("Plotting Figure S2...\n")
	figS2 <- data_imp %>%
				do.call("rbind", .) %>%
				dplyr::mutate(m=factor(m,
						levels = 1:15,
						labels = paste0("Imputation ", 1:15)),
					phone=factor(phone,
						levels = 0:1,
						labels = c("No", "Yes"))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=p, color=phone)) +
					ggplot2::geom_density(ggplot2::aes(y=..scaled..), fill=NA, trim=TRUE) +
					ggplot2::xlim(0, 1) +
					ggplot2::facet_wrap(.~m, ncol=3) +
					ggplot2::xlab("Predicted probability of owning a mobile phone") +
					ggplot2::ylab("Scaled density") +
					ggplot2::guides(color=ggplot2::guide_legend(title="Mobile phone owner")) +
					ggplot2::theme_bw() +
					ggplot2::theme(legend.position = "bottom")

	ggplot2::ggsave(
		file,
		figS2,
		width=8,
		height=8)

}

# Figure S3
###########

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

# Additional details were modified on adobe illustrator and are not generated through R code

figureS3 <- function(file=file.path(path.workspace, "figS3.tiff")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	note("Pooling standardized differences...\n")
	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150, print=FALSE)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind) %>% select(-id), m=15, seed=150, print=FALSE)

	data_imp_M <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								filter(sex=="M") %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_F <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									filter(sex=="F") %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	mean_diff_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:11] <- mean_diff_fact[,2:11]/sqrt(var_fact[,2:11])

	mean_diff_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),,
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),,
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	t_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	glob <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Whole dataset",
					group="Whole dataset",
					stroke=2)

	glob_table <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num)))

	# Men
	mean_diff_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table_M <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),,
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table_M <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp_M[, c(1:6,9:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_M <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Gender strata",
						group="Men",
						stroke=2) %>%
					dplyr::filter(var %in% c("clinic_time", "dest")) %>%
					dplyr::mutate(var=ifelse(var=="clinic_time", "clinic_time_m", "dest_m"))

	stratum_table_M <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))

	# Women
	mean_diff_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table_F <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table_F <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	t_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_F <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Gender strata",
						group="Women",
						stroke=2) %>%
					dplyr::filter(var %in% c("clinic_time", "dest")) %>%
					dplyr::mutate(var=ifelse(var=="clinic_time", "clinic_time_f", "dest_f"))

	stratum_table_F <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))

	note("Plotting Figure S3...\n")
	figS3 <- rbind(glob,
				stratum_M,
				stratum_F) %>%
				dplyr::mutate(
					type=factor(type,
						levels=c("Whole dataset", "Gender strata")),
					var=factor(var,
					levels=rev(c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "adult_house", "hsv", "d_child0", "clinic_unable", "clinic_time",
						"clinic_time_m", "clinic_time_f","travelCar", "dest", "dest_m", "dest_f")),
					labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
						"Proportion of men", "Number of adults\nin the household","HSV shedding",
						"Reported at least\none deceased child", "Unable to access a\nhealth care center",
						"Travel time to a\nhealth care center", "Travel time to a health\ncare center (M/F)", "Travel time to a health\ncare center (M/F)",
						"Able to travel to a health\ncare center with a car",
						"Number of travel\ndestinations", "Number of travel\ndestinations (M/F)", "Number of travel\ndestinations (M/F)"))),
					fill=ifelse(p>0.05, NA, ifelse(group=="Men", "red", ifelse(group=="Women", "blue", "black"))),
					fake=ifelse(p>0.05, "p>0.05", "p<=0.05")) %>%
				ggplot2::ggplot(., ggplot2::aes(x=t, y=var, color=group)) +
					ggplot2::geom_point(
						ggplot2::aes(fill=I(fill), group=p2),
						stroke=0.8,
						shape=21,
						size=1.3) +
					ggplot2::geom_point(
						ggplot2::aes(shape=fake, group=fake),
						alpha=0,
						size=1.3) +
					ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
					ggplot2::scale_shape_manual(
						breaks=c("p<=0.05", "p>0.05"),
						values=c(16,21)) +
					ggplot2::scale_color_manual(
						name="",
						breaks=c("Whole dataset", "Men", "Women"),
						values=c("black", "red", "blue")) +
					ggplot2::guides(
						shape=ggplot2::guide_legend(
							title="p-value",
							override.aes = list(
								size= 0.9,
								alpha=1)),
						color=ggplot2::guide_legend(
							title="Strata",
							override.aes=list(
								size=0.9,
								stroke=0.7))) +
					ggplot2::xlab("Standardized difference between mobile phone owners and non-phone owners") +
					ggplot2::ylab("") +
					ggplot2::theme_bw() +
					ggplot2::theme(
						legend.position = c(0.94, 0.82),
						legend.box.background = ggplot2::element_rect(
							colour = "black",
							fill="white"),
						legend.key.size = unit(0.5, 'lines'),
						legend.margin = ggplot2::margin(0, 1, 1, 1),
						legend.box.margin = margin(0, 1, 1, 1),
						legend.text=ggplot2::element_text(size=4),
						legend.title=ggplot2::element_text(size=5),
						axis.text.y = ggplot2::element_text(size = 5),
						axis.text.x = ggplot2::element_text(size = 5),
						axis.title.x = ggplot2::element_text(size = 7))

	ggplot2::ggsave(
		file,
		figS3,
		width=16,
		height=8
		)
}


# Figure S4 and Table S3
########################

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

# Additional details were modified on adobe illustrator and are not generated through R code

figureS4_tableS3 <- function(file=file.path(path.workspace, "figS4.tiff"), table=file=file.path(path.workspace, "tableS3.csv")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(
						phone=ifelse(as.character(phone)=="No data", NA,
							ifelse(phone=="Phone",1,0)),
						phone_use_ever=ifelse(as.character(phone_use_ever)=="No data", NA,
							ifelse(phone_use_ever=="Already used a phone", 1, 0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(
						phone=ifelse(as.character(phone)=="No data", NA,
							ifelse(phone=="Phone",1,0)),
						phone_use_ever=ifelse(as.character(phone_use_ever)=="No data", NA,
							ifelse(phone_use_ever=="Already used a phone", 1, 0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(
						phone=ifelse(as.character(phone)=="No data", NA,
							ifelse(phone=="Phone",1,0)),
						phone_use_ever=ifelse(as.character(phone_use_ever)=="No data", NA,
							ifelse(phone_use_ever=="Already used a phone", 1, 0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(
						phone=ifelse(as.character(phone)=="No data", NA,
							ifelse(phone=="Phone",1,0)),
						phone_use_ever=ifelse(as.character(phone_use_ever)=="No data", NA,
							ifelse(phone_use_ever=="Already used a phone", 1, 0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	note("\nLove plot...\n")
	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150, print=FALSE)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind) %>% select(-id), m=15, seed=150, print=FALSE)

	data_imp_M <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								filter(sex=="M") %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_F <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									filter(sex=="F") %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_2015 <- lapply(1:15, function(x){
					temp <- mice::complete(data_ind_imp, x) %>%
								filter(year==2015) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_2016 <- lapply(1:15, function(x){
						temp <- mice::complete(data_ind_imp, x) %>%
									filter(year==2016) %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	comp <- function(temp, type, group){
		mean_diff_fact <- temp[, c(1:7,10:13,15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(age16_25=mean(age16_25),
							age26_35=mean(age26_35),
							age36_45=mean(age36_45),
							age46_59=mean(age46_59),
							age_60=mean(age_60),
							sexM=mean(sexM),
							clinic_unable=mean(clinic_unable),
							travelCar=mean(travelCar),
							d_child0=mean(d_child0),
							hsv=mean(hsv)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(age16_25=diff(age16_25),
							age26_35=diff(age26_35),
							age36_45=diff(age36_45),
							age46_59=diff(age46_59),
							age_60=diff(age_60),
							sexM=diff(sexM),
							clinic_unable=diff(clinic_unable),
							travelCar=diff(travelCar),
							d_child0=diff(d_child0),
							hsv=diff(hsv))

		var_fact <- temp[, c(1:7,10:13,15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::mutate(age16_25=mean(age16_25),
							age26_35=mean(age26_35),
							age36_45=mean(age36_45),
							age46_59=mean(age46_59),
							age_60=mean(age_60),
							sexM=mean(sexM),
							clinic_unable=mean(clinic_unable),
							travelCar=mean(travelCar),
							d_child0=mean(d_child0),
							hsv=mean(hsv)) %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
							age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
							age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
							age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
							age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
							sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
							clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
							travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
							d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
							hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(age16_25=sum(age16_25),
							age26_35=sum(age26_35),
							age36_45=sum(age36_45),
							age46_59=sum(age46_59),
							age_60=sum(age_60),
							sexM=sum(sexM),
							clinic_unable=sum(clinic_unable),
							travelCar=sum(travelCar),
							d_child0=sum(d_child0),
							hsv=sum(hsv))

		mean_diff_fact[,2:11] <- mean_diff_fact[,2:11]/sqrt(var_fact[,2:11])

		mean_diff_num <- temp[, c(1, 8:9, 14:15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(dest=mean(dest),
							clinic_time=mean(clinic_time),
							adult_house=mean(adult_house)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(dest=diff(dest),
							clinic_time=diff(clinic_time),
							adult_house=diff(adult_house))

		var_num <- temp[, c(1, 8:9, 14:15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(dest=var(dest)/dplyr::n(),
							clinic_time=var(clinic_time)/dplyr::n(),
							adult_house=var(adult_house)/dplyr::n()) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(dest=sum(dest),
							clinic_time=sum(clinic_time),
							adult_house=sum(adult_house))

		mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

		## Pooling difference testing
		z_fact <- temp[, c(1:7,10:13,15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(age16_25=mean(age16_25),
							age26_35=mean(age26_35),
							age36_45=mean(age36_45),
							age46_59=mean(age46_59),
							age_60=mean(age_60),
							sexM=mean(sexM),
							clinic_unable=mean(clinic_unable),
							travelCar=mean(travelCar),,
							d_child0=mean(d_child0),
							hsv=mean(hsv)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(age16_25=diff(age16_25),
							age26_35=diff(age26_35),
							age36_45=diff(age36_45),
							age46_59=diff(age46_59),
							age_60=diff(age_60),
							sexM=diff(sexM),
							clinic_unable=diff(clinic_unable),
							travelCar=diff(travelCar),
							d_child0=diff(d_child0),,
							hsv=diff(hsv))

		between_var_fact <- apply(z_fact[,-1], 2, function(x){
			(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
		})

		se_fact <- temp[, c(1:7,10:13,15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::mutate(n=dplyr::n()) %>%
						dplyr::group_by(m) %>%
						dplyr::mutate(n1=min(n), n2=max(n)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(age16_25=mean(age16_25),
							age26_35=mean(age26_35),
							age36_45=mean(age36_45),
							age46_59=mean(age46_59),
							age_60=mean(age_60),
							sexM=mean(sexM),
							clinic_unable=mean(clinic_unable),
							travelCar=mean(travelCar),
							d_child0=mean(d_child0),,
							hsv=mean(hsv),
							n1=mean(n1),
							n2=mean(n2)) %>%
						dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
							age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
							age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
							age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
							age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
							sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
							clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
							travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
							d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),,
							hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
						dplyr::select(m:hsv)

		within_var_fact <- apply(se_fact[,-1], 2, mean)
		r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
		df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

		t_num <- temp[, c(1, 8:9, 14:15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(dest=mean(dest),
							clinic_time=mean(clinic_time),
							adult_house=mean(adult_house)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(dest=diff(dest),
							clinic_time=diff(clinic_time),
							adult_house=diff(adult_house))

		between_var_num <- apply(t_num[,-1], 2, function(x){
			(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
		})

		se_num <- temp[, c(1, 8:9, 14:15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::mutate(n=dplyr::n()) %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(dest=var(dest),
							clinic_time=var(clinic_time),
							adult_house=var(adult_house),
							n=mean(n)) %>%
						dplyr::mutate(dest=dest/n,
							clinic_time=clinic_time/n,
							adult_house=adult_house/n) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(dest=sum(dest),
							clinic_time=sum(clinic_time),
							adult_house=sum(adult_house))

		within_var_num <- apply(se_num[,-1], 2, mean)
		r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
		df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

		result <- data.frame(
					t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num)))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type=type,
						group=group)
		return(result)
	}

	stratum_F <- comp(data_imp_F, "Gender strata", "Women")
	stratum_M <- comp(data_imp_M, "Gender strata", "Men")
	stratum_2015 <- comp(data_imp_2015, "Yearly strata", 2015)
	stratum_2016 <- comp(data_imp_2016, "Yearly strata", 2016)

	note("Plotting Figure S4...\n")
	figS4a <- rbind(
				stratum_M[-6,],
				stratum_F[-6,]) %>%
				dplyr::mutate(
					group=factor(group,
						levels=c("Women", "Men")), 
					var=factor(var,
					levels=rev(c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "adult_house", "hsv", "d_child0", "clinic_unable", "clinic_time", "travelCar", "dest")),
					labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
						"Proportion of men", "Number of adults\nin the household","HSV shedding",
						"Reported at least\none deceased child", "Unable to access a\nhealth care center", "Travel time to a\nhealth care center",
						"Able to travel to a health\ncare center with a car", "Number of\ntravel destinations")))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=t, y=var)) +
					ggplot2::geom_point(
						ggplot2::aes(shape=p2),
						stroke=1,
						size=2) +
					ggplot2::scale_shape_manual(values=c(16, 21)) +
					ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
					ggplot2::facet_grid(.~group) +
					ggplot2::guides(shape=ggplot2::guide_legend(title="")) +
					ggplot2::xlab("Standardized difference between mobile phone owners and non-phone owners") +
					ggplot2::ylab("") +
					ggplot2::theme_bw() +
					ggplot2::theme(legend.position="none")

	figS4b <- rbind(
				stratum_2015,
				stratum_2016) %>%
				dplyr::mutate(
					group=factor(group,
						levels=2015:2016,
						labels=c("2015 - Rainy season", "2016 - Dry season")), 
					var=factor(var,
					levels=rev(c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "adult_house", "hsv", "d_child0", "clinic_unable", "clinic_time", "travelCar", "dest")),
					labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
						"Proportion of men", "Number of adults\nin the household","HSV shedding",
						"Reported at least\none deceased child", "Unable to access a\nhealth care center", "Travel time to a\nhealth care center",
						"Able to travel to a health\ncare center with a car", "Number of\ntravel destinations")))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=t, y=var)) +
					ggplot2::geom_point(
						ggplot2::aes(shape=p2),
						# position=ggplot2::position_dodge(width=1),
						stroke=1,
						size=2) +
					ggplot2::scale_shape_manual(values=c(16, 21)) +
					ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
					ggplot2::facet_grid(.~group) +
					ggplot2::guides(shape=ggplot2::guide_legend(title="")) +
					ggplot2::xlab("Standardized difference between mobile phone owners and non-phone owners") +
					ggplot2::ylab("") +
					ggplot2::theme_bw() +
					ggplot2::theme(legend.position="none")

	tableS3_sex <- cbind(
					stratum_F[-6, c(2,1,3)] %>%
						dplyr::mutate(
							t=round(t, digit=3),
							p=round(p, digit=3)),
					stratum_M[-6, c(1,3)] %>%
						dplyr::mutate(
							t=round(t, digit=3),
							p=round(p, digit=3)))
	empty <- rep(NA, 4)
	tableS3 <- cbind(
				stratum_2015[, c(2,1,3)] %>%
					dplyr::mutate(
						t=round(t, digit=3),
						p=round(p, digit=3)),
				stratum_2016[, c(1,3)] %>%
					dplyr::mutate(
						t=round(t, digit=3),
						p=round(p, digit=3)))
	tableS3 <- cbind(
				love_table,
				rbind(
					tableS3_sex[1:5,-1],
					empty,
					tableS3_sex[6:12,-1]))

	fig <- multipanelfigure::multi_panel_figure(
				width=240,
				height=c(120, 120),
				columns=1) %>%
				multipanelfigure::fill_panel(
					figS4b,
					label="A",
					row=1, column=1) %>%
				multipanelfigure::fill_panel(
					figS4a, label="B",
					row=2, column=1) %>%
				multipanelfigure::save_multi_panel_figure(file, dpi=300)

	note("Generating Table S3...\n")
	write.csv(
		tableS3,
		table,
		row.names=FALSE)
}


# Figure S5 and Table S10
########################

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

# Additional details were modified on adobe illustrator and are not generated through R code

figureS5_tabaleS10 <- function(file=file.path(path.workspace, "figS5.tiff"), table=file.path(path.workspace, "tableS10.csv")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	note("Mean reduction in travel time...\n")
	note("Pooled estimate based on matching...\n")
	data_imp <- mice::mice(rbind(data2015_red, data2016_ind), m=15, seed=1234, print=FALSE)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind), m=15, seed=1234, print=FALSE)
	data_imp_man <- mice::mice(rbind(data2015, data2016), m=15, seed=1234, print=FALSE)

	trim_iter <- function(imp){
		mod_mat <- stats::model.matrix(phone~age+sex+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
		imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
		colnames(imp) <- gsub("-", "_", colnames(imp))
		colnames(imp) <- gsub(">=", "_", colnames(imp))
		colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
		colnames(imp) <- gsub("\\.", "_", colnames(imp))
		repeat{
			n <- nrow(imp)
			mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
				data=imp)
			check <- apply(mod_mat, 2, mean) # Initiating a check
			check <- !(check==1 | check==0) & names(check)!="clinic_time"
			form <- paste0("phone~", paste(names(check)[check], collapse="+"))
			mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
			imp$p <- stats::predict(mod, type="response")
			cutoff_low <- min(imp$p[imp$phone==1])
			cutoff_high <- max(imp$p[imp$phone==0])
			imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
			n_trim <- nrow(imp)
			if(n_trim==n){
				break
			}
		}
		return(imp)
	}

	data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	data_ind_imp <- lapply(1:15, function(x){mice::complete(data_ind_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	data_imp_man <- lapply(1:15, function(x){mice::complete(data_imp_man, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	for(i in 1:length(data_imp)){
		data_imp[[i]]$m <- i
	}
	for(i in 1:length(data_ind_imp)){
		data_ind_imp[[i]]$m <- i
	}
	for(i in 1:length(data_imp_man)){
		data_imp_man[[i]]$m <- i
	}

	# Doing the calculation with matching
	match_est <- data_imp %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined$varest)
	var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

	match_combined <- rbind(match_combined,
							data.frame(m=99,
								est=mean(match_combined$est),
								varest=var_within+(1+(1/nrow(match_combined)))*var_between))

	match_est <- data_ind_imp %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_ind_combined <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_ind_combined$varest)
	var_between <- (1/(nrow(match_ind_combined)-1))*sum((match_ind_combined$est-mean(match_ind_combined$est))^2)

	match_ind_combined <- rbind(match_ind_combined,
							data.frame(m=99,
								est=mean(match_ind_combined$est),
								varest=var_within+(1+(1/nrow(match_ind_combined)))*var_between))

	match_est <- data_imp_man %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined_man <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined_man$varest)
	var_between <- (1/(nrow(match_combined_man)-1))*sum((match_combined_man$est-mean(match_combined_man$est))^2)

	match_combined_man <- rbind(match_combined_man,
							data.frame(m=99,
								est=mean(match_combined_man$est),
								varest=var_within+(1+(1/nrow(match_combined_man)))*var_between))

	match_est <- data_imp_man %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined_man <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined_man$varest)
	var_between <- (1/(nrow(match_combined_man)-1))*sum((match_combined_man$est-mean(match_combined_man$est))^2)

	match_combined_man <- rbind(match_combined_man,
							data.frame(m=99,
								est=mean(match_combined_man$est),
								varest=var_within+(1+(1/nrow(match_combined_man)))*var_between))

	note("Plotting Figure S5...\n")
	figS5 <- rbind(
				match_combined_man[16,] %>% dplyr::mutate(type="2015 values for participants\ninterviewed twice", col="black"),
				match_combined[16,] %>% dplyr::mutate(type="2016 values for participants\ninterviewed twice", col=viridis::viridis(4, option="D")[2]),
				match_ind_combined[16,] %>% dplyr::mutate(type="2015 and 2016 values for\nparticipants interviewed twice", col=viridis::viridis(4, option="D")[1])) %>%
				dplyr::mutate(
					type=factor(type,
						levels=c("2015 and 2016 values for\nparticipants interviewed twice",
							"2016 values for participants\ninterviewed twice",
							"2015 values for participants\ninterviewed twice"))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=type, y=est, color=I(col))) +
					ggplot2::geom_errorbar(
						ggplot2::aes(ymin=est-1.96*sqrt(varest), ymax=est+1.96*sqrt(varest)),
						width=0.05) +
					ggplot2::geom_hline(yintercept = 0, linetype="dashed") +
					ggplot2::geom_point(
						shape=21,
						stroke=1.5,
						size=3) +
					ggplot2::guides(colour="none") +
					ggplot2::coord_flip() +
					ggplot2::ylim(c(-7, 2)) +
					ggplot2::xlab("") +
					ggplot2::ylab("Mean reduction in time (hours) necessary to reach a health care\ncenter for mobile phone owners compared to non-phone owners") +
					ggplot2::theme_bw()

	ggplot2::ggsave(
		file,
		figS,
		width=6.5,
		height=4.5)

	note("Generating Table S9...\n")
	tableS10 <- rbind(
					match_combined_man[16,] %>% dplyr::mutate(type="2015 values for participants\ninterviewed twice"),
					match_combined[16,] %>% dplyr::mutate(type="2016 values for participants\ninterviewed twice"),
					match_ind_combined[16,] %>% dplyr::mutate(type="2015 and 2016 values for\nparticipants interviewed twice")) %>%
					dplyr::mutate(
						red=round(est, 2),
						ci=paste(round(est-1.96*sqrt(varest), 2), round(est+1.96*sqrt(varest), 2), sep="-")) %>%
					dplyr::select(type, red, ci)

	write.csv(
		tableS10,
		table,
		row.names=FALSE)
}

# Figure S6 and Table S11
#########################

# path.workspace is an object with a path to the forlder where figures are generated
# Modify it to suit your needs

figureS6_tableS11 <- function(file=file.path(path.workspace, "figS6.tiff"), table=file.path(path.workspace, "tableS11.csv")){
	# 2015
	net <- net2015
	data <- net_d_manage(net, verbose=FALSE)
	nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration

	edges <- edge2015
	nodes <- nodes %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links$min <- apply(links[,1:2],1,min)
	links$max <- apply(links[,1:2],1,max)
	links <- links[links$min!=links$max,]
	links$comb <- paste(links$min, links$max,sep="-")
	links <- links[!duplicated(links$comb),c("x","y")]
	l_list <- unique(c(links$x, links$y))
	temp <- net[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=as.integer(l_list[!(l_list %in% as.character(nodes$id))])) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes <- nodes %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes[match(l_list$id, nodes$id), c("age", "sex")] <- l_list[na.omit(match(nodes$id, l_list$id)), c("age", "sex")]
	nodes <- nodes %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
					phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l <- links
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
						"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
						(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")

	edges <- setNames(edges, c("x", "y"))
	edges$link <- unlist(lapply(1:nrow(edges), function(x){
		paste(sort(t(edges)[,x]), collapse="-")
	}))
	links$link <- unlist(lapply(1:nrow(links), function(x){
		paste(sort(t(links)[,x]), collapse="-")
	}))
	links_df <- rbind(edges, links) %>%
					dplyr::group_by(link) %>%
					dplyr::mutate(n=dplyr::row_number()) %>%
					dplyr::ungroup() %>%
					dplyr::filter(n==1) %>%
					dplyr::select(x, y, link)
	links_df$x <- ifelse(substr(links_df$x, 1, 1)=="0", substr(links_df$x, 2, nchar(links_df$x)), links_df$x)
	links_df$y <- ifelse(substr(links_df$y, 1, 1)=="0", substr(links_df$y, 2, nchar(links_df$y)), links_df$y)

	added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))

	net <- igraph::graph_from_data_frame(
				d=unique(links_df[,c("x", "y")]),
				vertices=data.frame(id=added_id),
				directed=FALSE)

	d_dist_2015 <- data.frame(
				id=added_id,
				d=igraph::degree(net)) %>%
				dplyr::left_join(
					.,
					nodes %>%
						dplyr::mutate(
							id=as.character(id)) %>%
						dplyr::select(id, phone, sex),
					by="id")

	# 2016
	net <- net2016
	data <- net_d_manage(net, verbose=FALSE)
	nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	edges <- edge2016
	nodes <- nodes[!(nodes$id==216 & nodes$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links$min <- apply(links[,1:2],1,min)
	links$max <- apply(links[,1:2],1,max)
	links <- links[links$min!=links$max,]
	links$comb <- paste(links$min, links$max,sep="-")
	links <- links[!duplicated(links$comb),c("x","y")]
	links_l <- links
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
						"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
						(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")
	edges <- setNames(edges, c("x", "y"))
	edges$link <- unlist(lapply(1:nrow(edges), function(x){
		paste(sort(t(edges)[,x]), collapse="-")
	}))
	links$link <- unlist(lapply(1:nrow(links), function(x){
		paste(sort(t(links)[,x]), collapse="-")
	}))
	links_df <- rbind(edges, links) %>%
					dplyr::group_by(link) %>%
					dplyr::mutate(n=dplyr::row_number()) %>%
					dplyr::ungroup() %>%
					dplyr::filter(n==1) %>%
					dplyr::select(x, y, link)
	links_df$x <- ifelse(substr(links_df$x, 1, 1)=="0", substr(links_df$x, 2, nchar(links_df$x)), links_df$x)
	links_df$y <- ifelse(substr(links_df$y, 1, 1)=="0", substr(links_df$y, 2, nchar(links_df$y)), links_df$y)

	added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))

	net <- igraph::graph_from_data_frame(
				d=unique(links_df[,c("x", "y")]),
				vertices=data.frame(id=added_id),
				directed=FALSE)

	d_dist_2016 <- data.frame(
				id=added_id,
				d=igraph::degree(net)) %>%
				dplyr::left_join(
					.,
					nodes %>%
						dplyr::mutate(
							id=as.character(id)) %>%
						dplyr::select(id, phone, sex),
					by="id")

	note("Plotting Figure S6...\n")
	gg_color_hue <- function(n) {
	  hues = seq(15, 375, length = n + 1)
	  hcl(h = hues, l = 65, c = 100)[1:n]
	}

	figS6A <- rbind(
			d_dist_2015 %>%
				dplyr::mutate(
					year="Rainy season - 2015"),
			d_dist_2016 %>%
				dplyr::mutate(
					year="Dry season - 2016")) %>%
			dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
			dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
			ggplot2::ggplot(., ggplot2::aes(x=d)) +
				ggplot2::geom_histogram(
					ggplot2::aes(fill=phone),
					color="black",
					binwidth=1) +
				ggplot2::scale_fill_manual(
					name="",
					values=rev(gg_color_hue(4))) +
				ggplot2::facet_grid(.~year) +
				ggplot2::xlab("Number of edges") +
				ggplot2::ylab("Count") +
				ggplot2::theme_bw()

	figS6B <- rbind(
			d_dist_2015 %>%
				dplyr::mutate(
					year="Rainy season - 2015"),
			d_dist_2016 %>%
				dplyr::mutate(
					year="Dry season - 2016")) %>%
			dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
			dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
			dplyr::filter(phone %in% c("No phone", "Phone")) %>%
			ggplot2::ggplot(., ggplot2::aes(x=d)) +
				ggplot2::geom_histogram(
					ggplot2::aes(fill=phone),
					color="black",
					binwidth=1) +
				ggplot2::scale_fill_manual(
					name="",
					values=rev(gg_color_hue(4)[1:2])) +
				ggplot2::facet_grid(.~year) +
				ggplot2::xlab("Number of edges") +
				ggplot2::ylab("Count") +
				ggplot2::theme_bw()

	fig <- multipanelfigure::multi_panel_figure(
				width=220,
				height=c(120, 120),
				columns=1) %>%
				multipanelfigure::fill_panel(
					figS6A,
					label="A",
					row=1, column=1) %>%
				multipanelfigure::fill_panel(
					figS6B, label="B",
					row=2, column=1) %>%
				multipanelfigure::save_multi_panel_figure(file, dpi=300)

	note("Generating Table S11...\n")
	tableS11 <- rbind(
					d_dist_2015 %>%
						dplyr::mutate(
							year="Rainy season - 2015"),
					d_dist_2016 %>%
						dplyr::mutate(
							year="Dry season - 2016")) %>%
					dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
					dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
					dplyr::filter(phone %in% c("No phone", "Phone")) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						d_mean=round(mean(d), 2),
						d_med=round(median(d), 2),
						IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
					dplyr::ungroup() %>%
					dplyr::mutate(
						p=c(wilcox.test(
							d~phone,
							data=rbind(
								d_dist_2015 %>%
									dplyr::mutate(
										year="Rainy season - 2015"),
								d_dist_2016 %>%
									dplyr::mutate(
										year="Dry season - 2016")) %>%
								dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
								dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
								dplyr::filter(phone %in% c("No phone", "Phone")))$p.value %>% round(., 3), "")) %>%
					dplyr::mutate(data="Complete") %>%
					dplyr::bind_rows(.,
						rbind(
							d_dist_2015 %>%
								dplyr::mutate(
									year="Rainy season - 2015"),
							d_dist_2016 %>%
								dplyr::mutate(
									year="Dry season - 2016")) %>%
							dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
							dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
							dplyr::filter(phone %in% c("No phone", "Phone") & sex=="F") %>%
							dplyr::group_by(phone) %>%
							dplyr::summarise(
								d_mean=round(mean(d), 2),
								d_med=round(median(d), 2),
								IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
							dplyr::ungroup() %>%
							dplyr::mutate(
								p=c(wilcox.test(
									d~phone,
									data=rbind(
										d_dist_2015 %>%
											dplyr::mutate(
												year="Rainy season - 2015"),
										d_dist_2016 %>%
											dplyr::mutate(
												year="Dry season - 2016")) %>%
										dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
										dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
										dplyr::filter(phone %in% c("No phone", "Phone") & sex=="F"))$p.value %>% round(., 3), "")) %>%
							dplyr::mutate(data="Women")) %>%
					dplyr::bind_rows(.,
						rbind(
							d_dist_2015 %>%
								dplyr::mutate(
									year="Rainy season - 2015"),
							d_dist_2016 %>%
								dplyr::mutate(
									year="Dry season - 2016")) %>%
							dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
							dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
							dplyr::filter(phone %in% c("No phone", "Phone") & sex=="M") %>%
							dplyr::group_by(phone) %>%
							dplyr::summarise(
								d_mean=round(mean(d), 2),
								d_med=round(median(d), 2),
								IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
							dplyr::ungroup() %>%
							dplyr::mutate(
								p=c(wilcox.test(
									d~phone,
									data=rbind(
										d_dist_2015 %>%
											dplyr::mutate(
												year="Rainy season - 2015"),
										d_dist_2016 %>%
											dplyr::mutate(
												year="Dry season - 2016")) %>%
										dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
										dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
										dplyr::filter(phone %in% c("No phone", "Phone") & sex=="M"))$p.value %>% round(., 3), "")) %>%
							dplyr::mutate(data="Men")) %>%
					dplyr::bind_rows(.,
						rbind(
							d_dist_2015 %>%
								dplyr::mutate(
									year="Rainy season - 2015"),
							d_dist_2016 %>%
								dplyr::mutate(
									year="Dry season - 2016")) %>%
							dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
							dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
							dplyr::filter(phone %in% c("No phone", "Phone") & year=="Rainy season - 2015") %>%
							dplyr::group_by(phone) %>%
							dplyr::summarise(
								d_mean=round(mean(d), 2),
								d_med=round(median(d), 2),
								IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
							dplyr::ungroup() %>%
							dplyr::mutate(
								p=c(wilcox.test(
									d~phone,
									data=rbind(
										d_dist_2015 %>%
											dplyr::mutate(
												year="Rainy season - 2015"),
										d_dist_2016 %>%
											dplyr::mutate(
												year="Dry season - 2016")) %>%
										dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
										dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
										dplyr::filter(phone %in% c("No phone", "Phone") & year=="Rainy season - 2015"))$p.value %>% round(., 3), "")) %>%
							dplyr::mutate(data="2015")) %>%
					dplyr::bind_rows(.,
						rbind(
							d_dist_2015 %>%
								dplyr::mutate(
									year="Rainy season - 2015"),
							d_dist_2016 %>%
								dplyr::mutate(
									year="Dry season - 2016")) %>%
							dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
							dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
							dplyr::filter(phone %in% c("No phone", "Phone") & year=="Dry season - 2016") %>%
							dplyr::group_by(phone) %>%
							dplyr::summarise(
								d_mean=round(mean(d), 2),
								d_med=round(median(d), 2),
								IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
							dplyr::ungroup() %>%
							dplyr::mutate(
								p=c(wilcox.test(
									d~phone,
									data=rbind(
										d_dist_2015 %>%
											dplyr::mutate(
												year="Rainy season - 2015"),
										d_dist_2016 %>%
											dplyr::mutate(
												year="Dry season - 2016")) %>%
										dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
										dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
										dplyr::filter(phone %in% c("No phone", "Phone") & year=="Dry season - 2016"))$p.value %>% round(., 3), "")) %>%
							dplyr::mutate(data="2016"))
	write.csv(
		tableS11,
		table,
		row.names=FALSE)
}

# Table 1
#########

## Table 1 was finalized on excel and word

table1 <- function(table=file.path(path.workspace, "table1.csv"), table_alt=file.path(path.workspace, "table1_alt.csv")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	nodes2016$phone_use_ever <- ifelse(nodes2016$phone_use_ever=="No data", NA,
									ifelse(nodes2016$phone_use_ever=="Already used a phone", "Yes", "No"))

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015$phone[nodes2015$id %in% c(96:98)] <- "Children"
	nodes2015 <- dplyr::filter(nodes2015, !(is.na(age) & id %in% c(96:98)))

	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	nodes2015$phone_use_ever <- ifelse(nodes2015$phone_use_ever=="No data", NA,
									ifelse(nodes2015$phone_use_ever=="Already used a phone", "Yes", "No"))

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2016)

	data2016_red <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2016)

	data <- rbind(data2015, data2016)
	data_red <- rbind(data2015, data2016_red)

	note("Generating Table 1...\n")

	desc <- function(X){
		if(length(unique(na.omit(X$sex)))==1){
			sex <- data.frame(var=rep(NA, 3),
						n=rep(NA, 3),
						p=rep(NA, 3))
		}else{
			sex <- X %>%
					dplyr::select(sex) %>%
					dplyr::group_by(sex) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
						sex=c("Women", "Men"))
			sex[nrow(sex)+1, 1] <- "Gender"
			sex <- data.frame(sex[c(nrow(sex), 1:(nrow(sex)-1)),])
			colnames(sex)[1] <- "var"
		}

		if(any(na.omit(X$age)==">=60")){
			age <- X %>%
					dplyr::select(age) %>%
					dplyr::group_by(age) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
						age=as.character(age))
			if(any(is.na(age$age))){
				age$age[is.na(age$age)] <- "Missing value"
			}else{
				age[nrow(age)+1, 1] <- "Missing value"
				age[nrow(age), 2:3] <- 0
			}
			age[nrow(age)+1, 1] <- "Age group"
			age <- data.frame(age[c(nrow(age), 1:(nrow(age)-1)),])
			colnames(age)[1] <- "var"
		}else{
			age <- X %>%
					dplyr::select(age) %>%
					dplyr::group_by(age) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
						age=as.character(age))
			age[nrow(age),1] <- "Missing value"
			age[nrow(age)+1, 1] <- "Age group"
			age <- data.frame(age[c(nrow(age), 1:(nrow(age)-1)),])
			colnames(age)[1] <- "var"
		}

		if(length(unique(na.omit(X$year)))==1){
			year <- data.frame(var=rep(NA, 3),
						n=rep(NA, 3),
						p=rep(NA, 3))
		}else{
			year <- X %>%
						dplyr::select(year) %>%
						dplyr::group_by(year) %>%
						dplyr::summarise(n=dplyr::n()) %>%
						dplyr::mutate(p=round(100*n/nrow(X), digits=1),
							year=as.character(year))
			year[nrow(year)+1, 1] <- "Recruitment year"
			year <- data.frame(year[c(nrow(year), 1:(nrow(year)-1)),])
			colnames(year)[1] <- "var"
		}

		empty <- rep("", 3)

		res <- rbind(year,
					empty,
					age,
					empty,
					sex)
		return(res)
	}

	global <- desc(data)
	global_alt <- desc(data_red)
	stratum_p <- desc(data[data$phone=="Phone",])
	stratum_np <- desc(data[data$phone=="No phone",])
	stratum_na <- desc(data[data$phone=="No data",])
	stratum_p_alt <- desc(data_red[data_red$phone=="Phone",])
	stratum_np_alt <- desc(data_red[data_red$phone=="No phone",])
	stratum_na_alt <- desc(data_red[data_red$phone=="No data",])

	table1 <- cbind(
			global,
			stratum_p[,-1],
			stratum_np[,-1],
			stratum_na[,-1])

	table1_alt <- cbind(
			global_alt,
			stratum_p_alt[,-1],
			stratum_np_alt[,-1],
			stratum_na_alt[,-1])

	write.csv(
		table,
		table1,
		row.names=FALSE)


	write.csv(
		table_alt,
		table1_alt,
		row.names=FALSE)
}

# Table S1
##########

## Table S1 is a wider version of Table 1. Calculatoins only inlcude percentages and means.
## No specific code for this.

# Table S2
##########

## Table S2 was finalized on excel and word

tableS2 <- function(table=file.path(path.workspace, "tableS2.html")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	nodes2016$phone_use_ever <- ifelse(nodes2016$phone_use_ever=="No data", NA,
									ifelse(nodes2016$phone_use_ever=="Already used a phone", "Yes", "No"))

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015$phone[nodes2015$id %in% c(96:98)] <- "Children"
	nodes2015 <- dplyr::filter(nodes2015, !(is.na(age) & id %in% c(96:98)))

	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	nodes2015$phone_use_ever <- ifelse(nodes2015$phone_use_ever=="No data", NA,
									ifelse(nodes2015$phone_use_ever=="Already used a phone", "Yes", "No"))

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))


	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2016)

	note("Multiple imputations...\n")
	data_imp <- mice::mice(
					rbind(
						data2015 %>%
							dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
								hsv=ifelse(hsv=="Positive", 1, 0),
								sex=factor(sex, levels = c("F", "M")),
								phone_use_ever=factor(tolower(phone_use_ever), levels=c("yes", "no")),
								age=factor(as.character(age),
									levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
								year=2015),
						data2016 %>%
							dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
								hsv=ifelse(hsv=="Positive", 1, 0),
								sex=factor(sex, levels = c("F", "M")),
								phone_use_ever=factor(tolower(phone_use_ever), levels=c("yes", "no")),
								age=factor(as.character(age),
									levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
								year=2016)
					) %>%
						select(-id),
					m=15, seed=150, print=FALSE)

	note("Pooling estimates...\n")
	desc <- function(tmp=data_imp, year_val=NULL, sex_val=NULL){
		tmp_imp <- lapply(1:15, function(x){
			imp <- mice::complete(tmp, x)
			if(!is.null(year_val)){
				imp <- imp %>%
						dplyr::filter(year==year_val)
			}
			if(!is.null(sex_val)){
				imp <- imp %>%
						dplyr::filter(sex==sex_val)
			}
			imp <- imp %>%
					dplyr::mutate(
						sex=ifelse(sex=="F", 1, 0),
						travel=ifelse(travel=="Car", 1, 0),
						phone_use_ever=ifelse(phone_use_ever=="yes", 1, 0),
						year=ifelse(year==2015, 1, 0))
			imp <- data.frame(
						stats::model.matrix(rep(1, nrow(imp))~year+age+sex+phone+phone_use_ever+dest+clinic_time+clinic_cost_travel+travel+clinic_unable+d_child0+hsv,
							data=imp))[,-1]
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			imp$age16_25 <- ifelse(imp$age26_35==0 & imp$age36_45==0 & imp$age46_59==0 & imp$age_60==0, 1, 0)
			imp <- imp[,c(1, 16, 2:15)]
			imp <- apply(imp, 2, mean)
			return(imp)
		})
		var_names <- names(tmp_imp[[1]])
		tmp_imp <- do.call("cbind", tmp_imp)
		return(
			data.frame(
				var=var_names,
				value=apply(tmp_imp, 1, mean)))
	}
	res_imp <- desc()
	res_imp_2015 <- desc(year_val=2015)
	res_imp_2016 <- desc(year_val=2016)
	res_imp_F <- desc(sex_val="F")
	res_imp_M <- desc(sex_val="M")

	note("Perfomring tests...\n")
	data_imp_s <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(
									sex=ifelse(sex=="F", 1, 0),
									travel=ifelse(travel=="Car", 1, 0),
									phone_use_ever=ifelse(phone_use_ever=="yes", 1, 0),
									year=ifelse(year==2015, 1, 0))
					temp <- cbind(sex=temp$sex,
								data.frame(
									stats::model.matrix(sex~year+age+sex+phone+phone_use_ever+dest+clinic_time+clinic_cost_travel+travel+clinic_unable+d_child0+hsv,
										data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						temp$age16_25 <- ifelse(temp$age26_35==0 & temp$age36_45==0 & temp$age46_59==0 & temp$age_60==0, 1, 0)
						temp <- temp[,c(1:2, 17, 3:16)]
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_y <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(
									sex=ifelse(sex=="F", 1, 0),
									travel=ifelse(travel=="Car", 1, 0),
									phone_use_ever=ifelse(phone_use_ever=="yes", 1, 0),
									year=ifelse(year==2015, 1, 0))
					temp <- cbind(year=temp$year,
								data.frame(
									stats::model.matrix(year~year+age+sex+phone+phone_use_ever+dest+clinic_time+clinic_cost_travel+travel+clinic_unable+d_child0+hsv,
										data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						temp$age16_25 <- ifelse(temp$age26_35==0 & temp$age36_45==0 & temp$age46_59==0 & temp$age_60==0, 1, 0)
						temp <- temp[,c(1:2, 17, 3:16)]
						return(temp)}) %>%
						do.call("rbind", .)

	# Sex difference
	mean_diff_fact <- data_imp_s[, -c(10:12)] %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						year=mean(year),
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						travel=mean(travel),
						clinic_unable=mean(clinic_unable),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						year=diff(year),
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever),
						travel=diff(travel),
						clinic_unable=diff(clinic_unable),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	var_fact <- data_imp_s[, -c(10:12)] %>%
					dplyr::group_by(m, sex) %>%
					dplyr::mutate(
						year=mean(year),
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						travel=mean(travel),
						clinic_unable=mean(clinic_unable),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						year=mean(year)*(1-mean(year))/dplyr::n(),
						age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						phone=mean(phone)*(1-mean(phone))/dplyr::n(),
						phone_use_ever=mean(phone_use_ever)*(1-mean(phone_use_ever))/dplyr::n(),
						travel=mean(travel)*(1-mean(travel))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						year=sum(year),
						age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						phone=sum(phone),
						phone_use_ever=sum(phone_use_ever),
						travel=sum(travel),
						clinic_unable=sum(clinic_unable),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,-1] <- mean_diff_fact[,-1]/sqrt(var_fact[,-1])

	mean_diff_num <- data_imp_s[, c(1, 10:12, 17)] %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						clinic_cost_travel=mean(clinic_cost_travel)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=diff(dest),
						clinic_time=diff(clinic_time),
						clinic_cost_travel=diff(clinic_cost_travel))

	var_num <- data_imp_s[, c(1, 10:12, 17)] %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						clinic_cost_travel=var(clinic_cost_travel)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=sum(dest),
						clinic_time=sum(clinic_time),
						clinic_cost_travel=sum(clinic_cost_travel))

	mean_diff_num[,-1] <- mean_diff_num[,-1]/sqrt(var_num[,-1])

	## Pooling difference testing
	z_fact <- data_imp_s[, -c(10:12)] %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						year=mean(year),
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						travel=mean(travel),
						clinic_unable=mean(clinic_unable),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						year=diff(year),
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever),
						travel=diff(travel),
						clinic_unable=diff(clinic_unable),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_s[, -c(10:12)] %>%
					dplyr::group_by(m, sex) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						year=mean(year),
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						travel=mean(travel),
						clinic_unable=mean(clinic_unable),
						d_child0=mean(d_child0),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(
						year=year*(1-year)*((1/n1)+(1/n2)),
						age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						phone=phone*(1-phone)*((1/n1)+(1/n2)),
						phone_use_ever=phone_use_ever*(1-phone_use_ever)*((1/n1)+(1/n2)),
						travel=travel*(1-travel)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	t_num <- data_imp_s[, c(1, 10:12, 17)] %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						clinic_cost_travel=mean(clinic_cost_travel)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=diff(dest),
						clinic_time=diff(clinic_time),
						clinic_cost_travel=diff(clinic_cost_travel))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_s[, c(1, 10:12, 17)] %>%
					dplyr::group_by(m, sex) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						dest=var(dest),
						clinic_time=var(clinic_time),
						clinic_cost_travel=var(clinic_cost_travel),
						n=mean(n)) %>%
					dplyr::mutate(
						dest=dest/n,
						clinic_time=clinic_time/n,
						clinic_cost_travel=clinic_cost_travel/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=sum(dest),
						clinic_time=sum(clinic_time),
						clinic_cost_travel=sum(clinic_cost_travel))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	sex_comp <- data.frame(
				var=colnames(cbind(mean_diff_fact[,-1], mean_diff_num[,-1])),
				t=apply(cbind(mean_diff_fact[,-1], mean_diff_num[,-1]), 2, mean),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")))

	# Year difference
	mean_diff_fact <- data_imp_y[, -c(10:12)] %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						sex=mean(sex),
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						travel=mean(travel),
						clinic_unable=mean(clinic_unable),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						sex=diff(sex),
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever),
						travel=diff(travel),
						clinic_unable=diff(clinic_unable),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	var_fact <- data_imp_y[, -c(10:12)] %>%
					dplyr::group_by(m, year) %>%
					dplyr::mutate(
						sex=mean(sex),
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						travel=mean(travel),
						clinic_unable=mean(clinic_unable),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						sex=mean(sex)*(1-mean(sex))/dplyr::n(),
						age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						phone=mean(phone)*(1-mean(phone))/dplyr::n(),
						phone_use_ever=mean(phone_use_ever)*(1-mean(phone_use_ever))/dplyr::n(),
						travel=mean(travel)*(1-mean(travel))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						sex=sum(sex),
						age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						phone=sum(phone),
						phone_use_ever=sum(phone_use_ever),
						travel=sum(travel),
						clinic_unable=sum(clinic_unable),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,-1] <- mean_diff_fact[,-1]/sqrt(var_fact[,-1])

	mean_diff_num <- data_imp_y[, c(1, 10:12, 17)] %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						clinic_cost_travel=mean(clinic_cost_travel)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=diff(dest),
						clinic_time=diff(clinic_time),
						clinic_cost_travel=diff(clinic_cost_travel))

	var_num <- data_imp_y[, c(1, 10:12, 17)] %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						clinic_cost_travel=var(clinic_cost_travel)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=sum(dest),
						clinic_time=sum(clinic_time),
						clinic_cost_travel=sum(clinic_cost_travel))

	mean_diff_num[,-1] <- mean_diff_num[,-1]/sqrt(var_num[,-1])

	## Pooling difference testing
	z_fact <- data_imp_y[, -c(10:12)] %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						sex=mean(sex),
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						travel=mean(travel),
						clinic_unable=mean(clinic_unable),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						sex=diff(sex),
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever),
						travel=diff(travel),
						clinic_unable=diff(clinic_unable),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_y[, -c(10:12)] %>%
					dplyr::group_by(m, year) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						sex=mean(sex),
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						travel=mean(travel),
						clinic_unable=mean(clinic_unable),
						d_child0=mean(d_child0),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(
						sex=sex*(1-sex)*((1/n1)+(1/n2)),
						age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						phone=phone*(1-phone)*((1/n1)+(1/n2)),
						phone_use_ever=phone_use_ever*(1-phone_use_ever)*((1/n1)+(1/n2)),
						travel=travel*(1-travel)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	t_num <- data_imp_y[, c(1, 10:12, 17)] %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						clinic_cost_travel=mean(clinic_cost_travel)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=diff(dest),
						clinic_time=diff(clinic_time),
						clinic_cost_travel=diff(clinic_cost_travel))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_y[, c(1, 10:12, 17)] %>%
					dplyr::group_by(m, year) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						dest=var(dest),
						clinic_time=var(clinic_time),
						clinic_cost_travel=var(clinic_cost_travel),
						n=mean(n)) %>%
					dplyr::mutate(
						dest=dest/n,
						clinic_time=clinic_time/n,
						clinic_cost_travel=clinic_cost_travel/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=sum(dest),
						clinic_time=sum(clinic_time),
						clinic_cost_travel=sum(clinic_cost_travel))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	year_comp <- data.frame(
				var=colnames(cbind(mean_diff_fact[,-1], mean_diff_num[,-1])),
				t=apply(cbind(mean_diff_fact[,-1], mean_diff_num[,-1]), 2, mean),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")))

	note("Generating Table S2...\n")
	tableS2 <- cbind(
			res_imp %>%
				dplyr::mutate(
					value=ifelse(var %in% c("dest", "clinic_time", "clinic_cost_travel"), value, 100*value)),
			res_imp_2015 %>%
				dplyr::mutate(
					value=ifelse(var %in% c("dest", "clinic_time", "clinic_cost_travel"), value, 100*value)) %>%
				dplyr::select(value_2015=value),
			res_imp_2016 %>%
				dplyr::mutate(
					value=ifelse(var %in% c("dest", "clinic_time", "clinic_cost_travel"), value, 100*value)) %>%
				dplyr::select(value_2016=value),
			p1=c(NA, year_comp$p[c(2:6, 1, 7:8, 13:15,9:12)]),
			res_imp_F %>%
				dplyr::mutate(
					value=ifelse(var %in% c("dest", "clinic_time", "clinic_cost_travel"), value, 100*value)) %>%
				dplyr::select(value_F=value),
			res_imp_M %>%
				dplyr::mutate(
					value=ifelse(var %in% c("dest", "clinic_time", "clinic_cost_travel"), value, 100*value)) %>%
				dplyr::select(value_M=value),
			p2=c(sex_comp$p[c(1:6)], NA, sex_comp$p[c(7:8, 13:15,9:12)]))

	write.csv(
		table,
		tableS2,
		row.names=FALSE)
}

# Table S4
##########

tableS4 <- function(table=file.path(path.workspace, "tableS4.csv")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	note("Multiple imputations...\n")
	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150, print=FALSE)

	note("Pooling estimates and performing tests...\n")
	data_imp_M <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								filter(sex=="M") %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_F <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									filter(sex=="F") %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	mean_diff_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:11] <- mean_diff_fact[,2:11]/sqrt(var_fact[,2:11])

	mean_diff_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),,
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),,
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	t_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	glob <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Whole dataset",
					group="Whole dataset",
					stroke=2)

	glob_table <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num)))

	# Men
	mean_diff_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table_M <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),,
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table_M <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp_M[, c(1:6,9:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	t_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_M <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Gender strata",
						group="Men",
						stroke=2) %>%
					dplyr::filter(var %in% c("clinic_time", "dest")) %>%
					dplyr::mutate(var=ifelse(var=="clinic_time", "clinic_time_m", "dest_m"))

	stratum_table_M <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))

	# Women
	mean_diff_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table_F <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table_F <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	t_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_F <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Gender strata",
						group="Women",
						stroke=2) %>%
					dplyr::filter(var %in% c("clinic_time", "dest")) %>%
					dplyr::mutate(var=ifelse(var=="clinic_time", "clinic_time_f", "dest_f"))

	stratum_table_F <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))

	mean_diff_fact_table <- mean_diff_fact_table %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(100*value, 1))
	
	mean_diff_fact_table <- cbind(
								mean_diff_fact_table %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_fact_table %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_num_table <- mean_diff_num_table %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(value, 1))
	mean_diff_num_table <- cbind(
								mean_diff_num_table %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_num_table %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_table <- rbind(
						mean_diff_fact_table,
						mean_diff_num_table) %>%
						dplyr::left_join(.,
							glob_table %>%
								dplyr::select(-t),
							by="var") %>%
						dplyr::mutate(p=round(p,3))

	mean_diff_fact_table_M <- mean_diff_fact_table_M %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(100*value, 1))
	mean_diff_fact_table_M <- cbind(
								mean_diff_fact_table_M %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_fact_table_M %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_num_table_M <- mean_diff_num_table_M %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(value, 1))
	mean_diff_num_table_M <- cbind(
								mean_diff_num_table_M %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_num_table_M %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_table_M <- rbind(
						mean_diff_fact_table_M,
						mean_diff_num_table_M) %>%
						dplyr::left_join(.,
							stratum_table_M %>%
								dplyr::select(-t),
							by="var") %>%
						dplyr::mutate(p=round(p,3))

	mean_diff_fact_table_F <- mean_diff_fact_table_F %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(100*value, 1))
	mean_diff_fact_table_F <- cbind(
								mean_diff_fact_table_F %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_fact_table_F %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_num_table_F <- mean_diff_num_table_F %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(value, 1))
	mean_diff_num_table_F <- cbind(
								mean_diff_num_table_F %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_num_table_F %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_table_F <- rbind(
						mean_diff_fact_table_F,
						mean_diff_num_table_F) %>%
						dplyr::left_join(.,
							stratum_table_F %>%
								dplyr::select(-t),
							by="var") %>%
						dplyr::mutate(p=round(p,3))

	empty <- rep(NA, 4)

	mean_diff_table_M <- rbind(
							mean_diff_table_M[1:5,],
							empty,
							mean_diff_table_M[6:12,])

	mean_diff_table_F <- rbind(
							mean_diff_table_F[1:5,],
							empty,
							mean_diff_table_F[6:12,])

	tableS4 <- cbind(
				mean_diff_table,
				mean_diff_table_F[,-1],
				mean_diff_table_M[,-1])

	write.csv(
		tableS4,
		table,
		row.names=FALSE)
}

# Table S5
##########

# It is only a visual way to explain the variables included in the logistic regression models considered.
# No calculation involved.

# Table S6 and Table S7
#######################

tableS6_tableS7 <- function(table=file.path(path.workspace, "tableS6.csv"), table_alt=file.path(path.workspace, "tableS7.csv")){
	note("Data management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	nodes2016$phone_use_ever <- ifelse(nodes2016$phone_use_ever=="No data", NA,
									ifelse(nodes2016$phone_use_ever=="Already used a phone", "Yes", "No"))

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015$phone[nodes2015$id %in% c(96:98)] <- "Children"
	nodes2015 <- dplyr::filter(nodes2015, !(is.na(age) & id %in% c(96:98)))

	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	nodes2015$phone_use_ever <- ifelse(nodes2015$phone_use_ever=="No data", NA,
									ifelse(nodes2015$phone_use_ever=="Already used a phone", "Yes", "No"))

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					# dplyr::mutate(year=2015)
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					# dplyr::mutate(year=2016)
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2015_mis <- data2015 %>%
						dplyr::filter(!is.na(phone))

	data2016_mis <- data2016 %>%
						dplyr::filter(!is.na(phone))

	note("Multiple imputations...\n")
	data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=150)
	data_imp_mis <- mice::mice(rbind(data2015_mis, data2016_mis), m=15, seed=150)

	note("Calculating agreement...\n")
	model1 <- "phone~age+sex+clinic_unable+adult_house+hsv+year+clinic_time+dest+travel+d_child0"
	model2 <- "phone~age+sex+clinic_unable+adult_house+hsv+clinic_time+dest+travel+d_child0"
	model3 <- "phone~age+sex+clinic_unable+adult_house+year+clinic_time+dest+travel+d_child0"
	model4 <- "phone~age+sex+clinic_unable+adult_house+hsv+clinic_time+dest+d_child0"
	model5 <- "phone~age+sex+clinic_unable+adult_house+year+clinic_time+dest+travel"
	model6 <- "phone~age+sex+clinic_unable+adult_house+clinic_time+dest+d_child0"
	model7 <- "phone~age+clinic_unable+adult_house+clinic_time+dest*sex+d_child0"
	model8 <- "phone~age+clinic_unable+clinic_time+dest+sex+d_child0"
	model9 <- "phone~age+clinic_unable+clinic_time+dest*sex+d_child0"
	model10 <- "phone~age+sex+clinic_time+dest+travel+d_child0"
	model11 <- "phone~age+clinic_unable+adult_house+hsv+year+clinic_time*sex+dest+travel+d_child0"
	model12 <- "phone~age+sex+clinic_unable+adult_house+hsv+year+clinic_time+clinic_time2+dest+dest2+travel+d_child0"
	model13 <- "phone~age+sex+clinic_unable+adult_house+hsv+year+clinic_time+clinic_time2+dest+travel+d_child0"
	model14 <- "phone~age+sex+clinic_unable+adult_house+hsv+year+clinic_time+dest+dest2+travel+d_child0"

	CV <- lapply(1:15, function(x){
		mice::complete(data_imp, x) %>%
			dplyr::mutate(
				dest2=dest^2,
				clinic_time2=clinic_time^2)})

	CV_estimate <- function(data, model=model1){
		estimate <- function(x){
			mod <- stats::glm(stats::formula(model), family = "binomial", data=data[-x,])
			agreement <- data.frame(
							obs=data$phone[x],
							pred=ifelse(stats::predict(mod, newdata=data[x,])>0.5, 1, 0))
			return(agreement)
		}
		CV_est <- lapply(1:nrow(data), estimate) %>%
					do.call("rbind", .) %>%
					dplyr::mutate(accuracy=ifelse(obs==pred, 1, 0))
		return(sum(CV_est$accuracy/nrow(CV_est)))
	}

	agreement <- data.frame(
					model=paste0("Model ", 1:14),
					agreement=NA)

	models <- c(model1, model2, model3,
				model4, model5, model6,
				model7, model8, model9,
				model10, model11, model12,
				model13, model14)

	pb <- txtProgressBar(min = 0, max = 14, style=3) 

	for(i in 1:14){
		agreement[i, 2] <- lapply(CV, function(X){CV_estimate(data=X, model=models[i])}) %>%
							do.call("c", .) %>%
							mean(.)
		setTxtProgressBar(pb,i)
	}

	close(pb)

	CV <- lapply(1:15, function(x){
		mice::complete(data_imp_mis, x) %>%
			dplyr::mutate(
				dest2=dest^2,
				clinic_time2=clinic_time^2)})

	agreement_mis <- data.frame(
					model=paste0("Model ", 1:14),
					agreement=NA)

	pb <- txtProgressBar(min = 0, max = 14, style=3) 

	for(i in 1:14){
		agreement_mis[i, 2] <- lapply(CV, function(X){CV_estimate(data=X, model=models[i])}) %>%
							do.call("c", .) %>%
							mean(.)
		setTxtProgressBar(pb,i)
	}

	close(pb)

	note("Generating Tables S6 and S7...\n")
	write.csv(
		agreement,
		table,
		row.names=FALSE)

	write.csv(
		agreement_mis,
		table_alt,
		row.names=FALSE)
}

# Table S8
##########

tableS8 <- function(table=file.path(path.workspace, "tableS8.csv")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016_ind <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016_ind <- nodes2016_ind[!(nodes2016_ind$id==216 & nodes2016_ind$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"]) | (links_l2016$y %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016_ind$phone_hh <- ifelse(nodes2016_ind$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016_ind$id %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone" | is.na(nodes2016_ind$phone_own)]) |
						(links_l2016$y %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone" | is.na(nodes2016_ind$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016_ind$phone_hh_opt <- ifelse(nodes2016_ind$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016_ind$id %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015_ind <- unique(data[[2]])
	nodes2015_ind <- nodes2015_ind %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015_ind$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")

	nodes2015_ind <- nodes2015_ind %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015_ind[match(l_list$id, nodes2015_ind$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015_ind$id, l_list$id)), c("age", "sex")]
	nodes2015_ind <-  nodes2015_ind %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"]) | (links_l2015$y %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015_ind$phone_hh <- ifelse(nodes2015_ind$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015_ind$id %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone" | is.na(nodes2015_ind$phone_own)]) |
						(links_l2015$y %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone" | is.na(nodes2015_ind$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015_ind$phone_hh_opt <- ifelse(nodes2015_ind$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015_ind$id %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015_ind$age <- factor(nodes2015_ind$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016_ind$age <- factor(nodes2016_ind$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015_ind$data <- ifelse(nodes2015_ind$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016_ind$data <- ifelse(nodes2016_ind$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015_ind$travel <- factor(ifelse(is.na(nodes2015_ind$clinic_travel), NA,
							ifelse(nodes2015_ind$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016_ind$travel <- factor(ifelse(is.na(nodes2016_ind$clinic_travel), NA,
							ifelse(nodes2016_ind$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015_ind$d_child0 <- ifelse(is.na(nodes2015_ind$deceased_child), NA,
							ifelse(nodes2015_ind$deceased_child>0, 1, 0))
	nodes2016_ind$d_child0 <- ifelse(is.na(nodes2016_ind$deceased_child), NA,
							ifelse(nodes2016_ind$deceased_child>0, 1, 0))

	data2015_ind <- nodes2015_ind[nodes2015_ind$phone!="Children",] %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015_ind[nodes2015_ind$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(as.character(nodes2016_ind$other_id)))) %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016_ind <- nodes2016_ind[nodes2016_ind$phone!="Children",] %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_red <- nodes2016_ind[nodes2016_ind$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015_ind$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	model3 <- "phone~age+sex+dest+clinic_unable+travel+adult_house+year+clinic_time"
	model3_s <- "phone~age+dest+clinic_unable+travel+adult_house+year+clinic_time"
	model3_y <- "phone~age+sex+dest+clinic_unable+travel+adult_house+clinic_time"

	note("Multipl imputations...\n")
	data_imp <- mice::mice(
					rbind(data2015_ind, data2016_red) %>%
					dplyr::select(-id),
					m=15, seed=150, verbose=FALSE)
	data_imp_ind <- mice::mice(
						rbind(data2015_ind, data2016_ind) %>%
						dplyr::select(-id),
						m=15, seed=150, verbose=FALSE)

	note("Fitting logistic regression models...\n")
	global <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3),
				family = "binomial"))),
				conf.int = TRUE,
				exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]
	men <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3_s),
				subset=sex=="M", 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]
	women <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3_s),
				subset=sex=="F", 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]
	d_2015 <- summary(mice::pool(with(data_imp_ind,
				glm(stats::formula(model3_y),
				subset=year==2015, 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]
	d_2016 <- summary(mice::pool(with(data_imp_ind,
				glm(stats::formula(model3_y),
				subset=year==2016, 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]

	tableS8 <- cbind(
			global %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p),
			rbind(
				d_2015[1:9,],
				rep(NA, 5),
				d_2015[10,]) %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p),
			rbind(
				d_2016[1:9,],
				rep(NA, 5),
				d_2016[10,]) %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p),
			rbind(women[1:4,],
				rep(NA, 5),
				women[5:10,]) %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p),
			rbind(men[1:4,],
				rep(NA, 5),
				men[5:10,]) %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p))[,c(1:4, 6:8, 10:12, 14:16, 18:20)]

	wite.csv(
		tableS8,
		table,
		row.names=FALSE)
}

# Table S13 and Table S14
#########################

## Tables S13 and S14 were finalized on excel and word

tableS13_tableS14 <- function(boot=1000, table=file.path(path.workspace, "tableS13.csv"), table_alt=file.path(path.workspace, "tableS14.csv")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- as.integer(unique(c(links2015$x, links2015$y)))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data <- rbind(data2015, data2016)

	note("Imputations and bootstrap...\n")
	BS_imput <- function(data=data){
		 temp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
		 temp_i <- mice::mice(temp, m=1, print=FALSE)
		 return(cbind(mice::complete(temp_i, 1), id=temp$id))
	}

	set.seed(150)
	boot_imput <- replicate(boot, BS_imput(data), simplify=FALSE)

	note("Making estimates and their 95% confidence intervals...\n")
	estim <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_total_2[1,]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_total_1[1,]))
		return(ratio)
	}

	estim_ratio <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_phone_2[data_phone_2$phone==0, -1]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_phone_1[data_phone_1$phone==0, -1]))
		return(ratio)
	}

	estim_abs <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest_75=quantile(dest, probs=0.75),
							dest_25=quantile(dest, probs=0.25),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						dplyr::ungroup() %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest_75=quantile(dest, probs=0.75),
							dest_25=quantile(dest, probs=0.25),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						dplyr::mutate(phone=2) %>%
						dplyr::select(
							phone, clinic_time_75, clinic_time_25,
							clinic_time, clinic_unable,
							dest_75, dest_25, dest,
							d_child0, d_child1) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						dplyr::ungroup() %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						data.frame()

		return(
			rbind(
				cbind(data_phone_1, data_phone_2 %>% dplyr::select(-phone)),
				cbind(data_total_1, data_total_2)
				)
			)
	}

	est <- lapply(boot_imput, estim) %>%
			do.call("rbind", .)

	est_M <- lapply(boot_imput, function(X){
				estim(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F <- lapply(boot_imput, function(X){
				estim(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_phone <- lapply(boot_imput, estim_ratio) %>%
			do.call("rbind", .)

	est_M_phone <- lapply(boot_imput, function(X){
				estim_ratio(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F_phone <- lapply(boot_imput, function(X){
				estim_ratio(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_abs <- lapply(boot_imput, estim_abs) %>%
			do.call("rbind", .)

	est_M_abs <- lapply(boot_imput, function(X){
				estim_abs(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F_abs <- lapply(boot_imput, function(X){
				estim_abs(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est <- est %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M <- est_M %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F <- est_F %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)

	est_phone <- est_phone %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M_phone <- est_M_phone %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F_phone <- est_F_phone %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)

	est_abs <- est_abs %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M_abs <- est_M_abs %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F_abs <- est_F_abs %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)

	data_fig <- rbind(
					data.frame(var=colnames(est),
						est=apply(est, 2, mean),
						ci_l=apply(est, 2, quantile, prob=0.025),
						ci_up=apply(est, 2, quantile, prob=0.975),
						type="complete",
						group="Whole dataset"),
					data.frame(var=colnames(est_M),
						est=apply(est_M, 2, mean),
						ci_l=apply(est_M, 2, quantile, prob=0.025),
						ci_up=apply(est_M, 2, quantile, prob=0.975),
						type="Men",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_F),
						est=apply(est_F, 2, mean),
						ci_l=apply(est_F, 2, quantile, prob=0.025),
						ci_up=apply(est_F, 2, quantile, prob=0.975),
						type="Women",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest"))
					) %>%
					mutate(
						group=factor(group,
							levels = c("Whole dataset", "Gender strata")),
						var=factor(var,
							levels = rev(c("dest",
								"clinic_time_25","clinic_time", "clinic_time_75",
								"clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable",
								"d_child0", "d_child1", "n", "n_adult", "n_child")),
							labels = rev(c(
								"Mean number of\ntravel destinations",
								"25%ile travel time to\na health care center","Mean travel time to\na health care center", "75%ile travel time to\na health care center",
								"Mean travel cost", "75%ile travel cost",
								"Proportion of individuals unable\nto reach a health care center",
								"Proportion of individuals with\nat least 1 deceased child",
								"Proportion of individuals with\nmore than 1 deceased child",
								"Mean number of people\nliving in the household",
								"Mean number of adults\nliving in the household",
								"Mean number of children\nliving in the household")))
						)

	data_phone_fig <- rbind(
					data.frame(var=colnames(est_phone),
						est=apply(est_phone, 2, mean),
						ci_l=apply(est_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_phone, 2, quantile, prob=0.975),
						type="complete",
						group="Whole dataset"),
					data.frame(var=colnames(est_M_phone),
						est=apply(est_M_phone, 2, mean),
						ci_l=apply(est_M_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_M_phone, 2, quantile, prob=0.975),
						type="Men",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_F_phone),
						est=apply(est_F_phone, 2, mean),
						ci_l=apply(est_F_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_F_phone, 2, quantile, prob=0.975),
						type="Women",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest"))
					) %>%
					mutate(
						group=factor(group,
							levels = c("Whole dataset", "Gender strata")),
						var=factor(var,
							levels = rev(c("dest",
								"clinic_time_25","clinic_time", "clinic_time_75",
								"clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable",
								"d_child0", "d_child1", "n", "n_adult", "n_child")),
							labels = rev(c(
								"Mean number of\ntravel destinations",
								"25%ile travel time to\na health care center","Mean travel time to\na health care center", "75%ile travel time to\na health care center",
								"Mean travel cost", "75%ile travel cost",
								"Proportion of individuals unable\nto reach a health care center",
								"Proportion of individuals with\nat least 1 deceased child",
								"Proportion of individuals with\nmore than 1 deceased child",
								"Mean number of people\nliving in the household",
								"Mean number of adults\nliving in the household",
								"Mean number of children\nliving in the household"))))

	var_digest <- function(df, ref, comp, direction=-1){
		data_fig <- df %>%
						dplyr::mutate(sim=rep(1:boot, each=3)) %>%
						dplyr::filter(phone %in% c(ref, comp)) %>%
						dplyr::group_by(sim)

		if(direction==-1){
			data_fig <- data_fig %>%
							dplyr::arrange(sim, -phone)
		}else{
			data_fig <- data_fig %>%
							dplyr::arrange(sim, phone)
		}
	
		data_fig <- data_fig %>%
						dplyr::summarise(
							clinic_cost_travel=diff(clinic_cost_travel),
							clinic_cost_travel_75=diff(clinic_cost_travel_75),
							clinic_unable=diff(clinic_unable),
							clinic_time=diff(clinic_time),
							clinic_time_25=diff(clinic_time_25),
							clinic_time_75=diff(clinic_time_75), 
							d_child0=diff(d_child0),
							dest=diff(dest),
							n=diff(n),
							n_adult=diff(n_adult),
							n_child=diff(n_child)) %>%
						dplyr::ungroup() %>%
						dplyr::select(-sim)

		data_fig <- data.frame(
						var=colnames(data_fig),
						est=apply(data_fig, 2, mean),
						ci_l=apply(data_fig, 2, quantile, prob=0.025),
						ci_up=apply(data_fig, 2, quantile, prob=0.975))

		return(data_fig)
	}

	data_abs_fig <- rbind(
			var_digest(est_abs, 2, 1, -1) %>%
				dplyr::mutate(
					type="complete",
					group="Whole dataset",
					ref="All participants"),
			var_digest(est_abs, 0, 1, 1) %>%
				dplyr::mutate(
					type="complete",
					group="Whole dataset",
					ref="Non-phone owners")) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_M_abs, 2, 1, -1) %>%
						dplyr::mutate(
							type="Men",
							group="Gender strata",
							ref="All participants"),
					var_digest(est_M_abs, 0, 1, 1) %>%
						dplyr::mutate(
							type="Men",
							group="Gender strata",
							ref="Non-phone owners")) %>%
				dplyr::filter(
					var %in% c("clinic_cost_travel", "clinic_cost_travel_75", "clinic_unable",
					"clinic_time", "clinic_time_25", "clinic_time_75", "dest"))) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_F_abs, 2, 1, -1) %>%
						dplyr::mutate(
							type="Women",
							group="Gender strata",
							ref="All participants"),
					var_digest(est_F_abs, 0, 1, 1) %>%
						dplyr::mutate(
							type="Women",
							group="Gender strata",
							ref="Non-phone owners")) %>%
				dplyr::filter(var %in% c("clinic_cost_travel", "clinic_cost_travel_75", "clinic_unable",
					"clinic_time", "clinic_time_25", "clinic_time_75", "dest"))) %>%
			dplyr::mutate(
				group_var=ifelse(var %in% c("clinic_time", "clinic_time_25", "clinic_time_75"),
					"Time to reach a\nhealth care center",
					ifelse(var %in% c("clinic_cost_travel", "clinic_cost_travel_75"),
						"Travel cost",
						ifelse(var %in% c("clinic_unable"),
							"Being unable to access a\nhealth care center",
							ifelse(var %in% c("d_child0"),
								"At least 1 deceased child",
								ifelse(var %in% c("dest", "dest_75", "dest_25"),
									"Mean number of travel\ndestinations",
									"Household characteristics")))))) %>%
			dplyr::filter(var %in% c(
				"clinic_time", "clinic_time_25", "clinic_time_75",
				"dest", "dest_25", "dest_75")) %>%
			dplyr::mutate(
				group_var=ifelse(var %in% c("clinic_time", "clinic_time_25", "clinic_time_75"), "Travel time to\na health care center",
						"Mean number of travel\ndestinations"),
				var=factor(var,
					levels = rev(c("dest",
						"clinic_time_25","clinic_time", "clinic_time_75")),
					labels = rev(c(
						"Mean number of\ntravel destinations",
						"25%ile travel time to\na health care center","Mean travel time to\na health care center", "75%ile travel time to\na health care center"))),
				type=factor(
					type,
					levels=rev(c("complete", "Men", "Women")),
					labels=rev(c("Whole dataset", "Men", "Women"))))

	note("Generating tables...\n")
	abs_table <- cbind(
					data_abs_fig %>%
						dplyr::mutate(group=factor(group, levels=c("complete", "Women", "Men"))) %>%
						dplyr::arrange(group_var, var, group) %>%
						dplyr::filter(ref=="All participants") %>%
						dplyr::mutate(
							est=round(est, 3),
							ci=paste(round(ci_l, 3), round(ci_up, 3), sep="-")) %>% 
						dplyr::select(var, type, est, ci),
					data_abs_fig %>%
						dplyr::mutate(group=factor(group, levels=c("complete", "Women", "Men"))) %>%
						dplyr::arrange(group_var, var, group) %>%
						dplyr::filter(ref=="Non-phone owners") %>%
						dplyr::mutate(
							est=round(est, 3),
							ci=paste(round(ci_l, 3), round(ci_up, 3), sep="-")) %>% 
						dplyr::select(est, ci))

	ratio_table <- cbind(
					data_fig %>%
						dplyr::mutate(group=factor(group, levels=c("complete", "Women", "Men"))) %>%
						dplyr::arrange(var, type) %>%
						dplyr::mutate(
							est=round(est, 3),
							ci=paste(round(ci_l, 3), round(ci_up, 3), sep="-")) %>% 
						dplyr::select(var, type, est, ci),
					data_phone_fig %>%
						dplyr::mutate(group=factor(group, levels=c("complete", "Women", "Men"))) %>%
						dplyr::arrange(var, type) %>%
						dplyr::mutate(
							est=round(est, 3),
							ci=paste(round(ci_l, 3), round(ci_up, 3), sep="-")) %>% 
						dplyr::select(est, ci))

	write.csv(
		ratio_table,
		table,
		rown.names=FALSE)

	write.csv(
		abs_table,
		table_alt,
		rown.names=FALSE)
}
