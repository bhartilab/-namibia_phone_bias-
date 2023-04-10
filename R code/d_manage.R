###############################
## Data management functions ##
###############################

net_d_manage <- function(data=net2016, verbose=TRUE){
	var <- c("id", paste0("id_l", 1:10))
	ind <- cbind(data[[1]]$id, data[[1]][,-which(colnames(data[[1]]) %in% var)])
	colnames(ind)[1] <- "id"
	ind$id <- as.integer(ind$id)

	child <- data[[2]] %>%
				dplyr::mutate(id=as.integer(id))

	net1 <- data[[1]][,var] %>%
				tidyr::gather(., link, id_link, id_l1:id_l10) %>%
				dplyr::filter(!is.na(id_link)) %>%
				dplyr::select(id, id_link) %>%
				dplyr::mutate(id=as.integer(id)) %>%
				dplyr::filter(id!=id_link)
	net1$link <- apply(net1[, c("id", "id_link")], 1, function(x){
		paste(sort(x), collapse="-")
	})

	net2 <- child[,var] %>%
				tidyr::gather(., link, id_link, id_l1:id_l10) %>%
				dplyr::filter(!is.na(id_link)) %>%
				dplyr::select(id, id_link) %>%
				dplyr::mutate(id_link=as.integer(id_link)) %>%
				dplyr::filter(id!=id_link)
	net2$link <- apply(net2[, c("id", "id_link")], 1, function(x){
		paste(sort(x), collapse="-")
	})

	net <- rbind(net1, net2) %>%
				dplyr::mutate(dup=duplicated(link)) %>%
				dplyr::filter(dup==FALSE) %>%
				dplyr::select(id, id_link)

	if(verbose){
		note(paste0("\nNumber of children with links to id not present in the main dataset: ", length(unique(net2$id[!(net2$id_link %in% ind$id)])), "\n"))
	
		if(length(unique(net2$id[!(net2$id_link %in% ind$id)]))>0){
			note(paste0("ID of the children: ", paste(unique(net2$id[!(net2$id_link %in% ind$id)]), collapse=", "), "\n"))
		}
	}
	return(list(links=net, nodes=ind))
}

clean_village_file <- function(data=villages){
	note("\nRearranging the columns...\n")
	temp <- data[,1:3]
	temp[2:(nrow(temp)+1),] <- temp
	temp[1,] <- colnames(temp)
	colnames(temp) <- c("village", "lat", "long")
	note("Keeping only coordinates...\n")
	temp$lat[grepl("*[a-z]", temp$lat)] <- NA
	temp[,2:3] <- apply(temp[,2:3], 2, as.numeric)
	note("Looking for duplicates...\n")
	note(paste("	Number of potential duplicates found:",(nrow(temp)-length(unique(temp$village))),"!\n", sep=" "))
	note("Attibuting ID villages for anonimity...\n")
	temp$loc_ID[temp$village=="Otjitanda"] <- 101
	temp$loc_ID[temp$village=="Etengwa"] <- 102
	temp$loc_ID[!(temp$loc_ID %in% c(101,102))] <- 200:(199+length(temp$loc_ID[!(temp$loc_ID %in% c(101,102))]))
	return(temp)
}

NA_clean <- function(X){
	return(ifelse(X=="NA", NA, X))
}

village_name_clean <- function(X){
	X[X=="Orokatouo"] <- "Orokatuwo"
	X[X=="Orokatuo"] <- "Orokatuwo"
	X[grepl("Ombujaondume", X)] <- "Ombwanjandume"
	X[X=="Okanjandi"] <- "Okanyandi"
	X[X=="Okauua"] <- "Okaua"
	X[X=="Etengua"] <- "Etengwa"
	X[grepl("Angola", X)] <- "Abroad"
	X[X=="Otjizi"] <- "Otjizu"
	X[X=="Okahua"] <- "Okauwa"
	X[X=="Ozia"] <- "Ozija"
	X[X=="Okakuyo"] <- "Okakuyu"
	X[X=="Omatjivingo"] <- "Omuatjivingo"
	X[X=="Ovikotorungo"] <- "Ovikotorongo"
	X[X=="Embuende"] <- "Embwende"
	X[X=="Otjinungwe"] <- "Otjinungwa"
	X[X=="Ongondavembari"] <- "Ongondjonambari"
	X[X=="Okangwati"] <- "Okuanguati"
	X[X=="Ekaranjuwo"] <- "Ekarwondjiwo"
	X[X=="Omatjivingo"] <- "Omuatjivingo"
	X[X==""] <- NA
	return(X)
}
