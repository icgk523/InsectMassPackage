#' Extract taxonomic information (genus, family, order)
#'
#' This function extracts the genus, family, order, and species name from a list of taxonomic data frames. 
#' It is designed to handle nested lists with data frames, and outputs a cleaned data frame.
#'
#' @param outputs A list of lists, where each sublist contains a data frame with taxonomic information, including a 'rank' and 'unique_name' column.
#' @return A data frame with columns for Species, Genus, Family, and Order.
#' @examples
#' # Example list of taxonomic data
#' tax_data_1 = data.frame(rank = c("genus", "family", "order"),
#'                          unique_name = c("Mus", "Muridae", "Rodentia"),
#'                          stringsAsFactors = FALSE)
#' tax_data_2 = data.frame(rank = c("genus", "family", "order"),
#'                          unique_name = c("Homo", "Hominidae", "Primates"),
#'                          stringsAsFactors = FALSE)
#' outputs = list("Mus musculus" = list(tax_data_1), "Homo sapiens" = list(tax_data_2))
#' taxonomy_table = extract_taxonomy_info_table(outputs)
#' print(taxonomy_table)
#' @export

extract_taxonomy = function(Species){
    rotl::tax_lineage(rotl::taxonomy_taxon_info(rotl::ott_id(rotl::tnrs_match_names(Species)), include_lineage = TRUE))
}

extract_taxonomy_info = function(tax_data, species_name) {
  # Extract genus, family, and order if they exist, otherwise use NA
  genus = ifelse(any(tax_data$rank == "genus"), tax_data[tax_data$rank == "genus", "name"], NA)
  family = ifelse(any(tax_data$rank == "family"), tax_data[tax_data$rank == "family", "name"], NA)
  order = ifelse(any(tax_data$rank == "order"), tax_data[tax_data$rank == "order", "name"], NA)
  
  # Return a data frame with the extracted information
  return(data.frame(Species = species_name, Genus = genus, Family = family, Order = order, stringsAsFactors = FALSE))
}

extract_taxonomy_info_table = function(outputs) {
  # Apply the function to each element of the list
  taxonomy_table = do.call(rbind, lapply(seq_along(outputs), function(i) {
    # Check if the element is valid and contains a sublist with a data frame
    if (!is.null(outputs[[i]]) && length(outputs[[i]]) > 0 && is.data.frame(outputs[[i]][[1]])) {
      # Safely extract the data frame from the sub-list
      tax_data = outputs[[i]][[1]]  # Extract the only element, which is the data frame
      # Get the species name
      species_name = names(outputs)[i]
      
      # Extract and return the relevant information
      extract_taxonomy_info(tax_data, species_name)
    } else {
      # Return a data frame with NA values if no valid data is available
      return(data.frame(Species = names(outputs)[i], Genus = NA, Family = NA, Order = NA, stringsAsFactors = FALSE))
    }
  }))
  
  return(taxonomy_table)
}



parallel_extract_otl_taxonomy = function(Data, n.cores) {
  # Check if 'Genus', 'Family', or 'Order' columns exist
  if ("Genus" %in% colnames(Data) || "Family" %in% colnames(Data) || "Order" %in% colnames(Data)) {
    
    # Get the unique species list from the data
    unique_species = unique(Data$Species)
    
    # Extract taxonomy for all unique species
    outputs = setNames(parallel::mclapply(as.list(unique(Data$Species)), extract_taxonomy, mc.cores = n.cores), unique(Data$Species))
    taxonomy_info = extract_taxonomy_info_table(outputs)
    
    # Merge the new taxonomy data with the original data, updating missing values
    for (i in 1:nrow(Data)) {
      # Find the corresponding taxonomy info for the species
      species_name = Data$Species[i]
      tax_info = taxonomy_info[taxonomy_info$Species == species_name, ]
      
      # Update missing values in the original data
      if (is.na(Data$Genus[i])) Data$Genus[i] = tax_info$Genus
      if (is.na(Data$Family[i])) Data$Family[i] = tax_info$Family
      if (is.na(Data$Order[i])) Data$Order[i] = tax_info$Order
    }
    
  } else {
    # If no taxonomic columns exist, just extract taxonomy and add the columns to the data
    outputs = setNames(parallel::mclapply(as.list(unique(Data$Species)), extract_taxonomy, mc.cores = n.cores), unique(Data$Species))
    taxonomy_info = extract_taxonomy_info_table(outputs)
    
    # Merge the taxonomy information directly into the original data
    Data = dplyr::left_join(Data, taxonomy_info, by = "Species")
  }
  
  return(Data)
}



parallel_extract_gbif_taxonomy = function(data, species_column, n.cores = NULL){
  colnames(data)[colnames(data) == species_column] = "Species" # Rename species column to "Species"
  species_list = as.list(unique(data$Species)) # Export a list of unique species names
  gbif_function = function(species_name){rgbif::name_lookup(query = species_name, limit=1, rank = "species")$data} # make a function that can be parallelised
  
  if (is.null(n.cores)){n.cores = parallel::detectCores()-1} # If n.cores is not supplied then use the max number of cores -1
  
  gbif_output = parallel::mclapply(species_list, gbif_function, mc.cores = n.cores) # export taxonomy in parallel
  gbif_output = dplyr::bind_rows(gbif_output) # bind parallel output
  
  taxonomy = gbif_output[, c("species", "genus", "family", "order", "phylum")] # subset taxonomy
  colnames(taxonomy) = c("Species", "Genus", "Family", "Order", "Phylum") # rename taxonomy
  taxonomy = taxonomy[!duplicated(taxonomy$Species), ] # remove duplicated taxonomies
  taxonomy$Genus <- sub(" .*", "", taxonomy$Species)  # Extract the first word from the Species column and assign it to Genus
  
  unique_species = setdiff(data$Species, taxonomy$Species)
  new_rows = data.frame(Species = unique_species, 
                         Phylum = NA, 
                         Order = NA, 
                         Family = NA, 
                         Genus = sub(" .*", "", unique_species))
  taxonomy = rbind(taxonomy, new_rows)
  
  taxonomy$Genus = stringr::str_to_title(taxonomy$Genus)
  taxonomy$Family = stringr::str_to_title(taxonomy$Family)
  taxonomy$Order = stringr::str_to_title(taxonomy$Order)
  taxonomy$Phylum = stringr::str_to_title(taxonomy$Phylum)
  
  data = merge(data,  taxonomy, by = "Species", all.x = TRUE, suffixes = c("", ".new"))
  
  for (col in c("Genus", "Family", "Order", "Phylum")) {
    if (paste0(col, ".new") %in% colnames(data)) {  # Check if ".new" column exists
      to_replace = is.na(data[[col]]) | data[[col]] == ""
      if (any(to_replace)) {
        data[[col]][to_replace] = data[[paste0(col, ".new")]][to_replace]
      }
      data[[paste0(col, ".new")]] = NULL # Remove the ".new" columns
    } else {
      data[[col]] = data[[paste0(col, ".new")]]
      data[[paste0(col, ".new")]] = NULL
    }
  }
  
  
  data$Family <- ave(data$Family, data$Genus, FUN = function(x) x[which(!is.na(x))[1]]) # Fill Family with the Family from same Genus species
  data$Order <- ave(data$Order, data$Family, FUN = function(x) x[which(!is.na(x))[1]]) # Fill Order with the Order from same Family species
  data$Phylum <- ave(data$Phylum, data$Order, FUN = function(x) x[which(!is.na(x))[1]]) # Fill Phylum with the Phylum from same Order species
  
  data = data[, c("Phylum", "Order", "Family", "Genus", "Species", setdiff(names(data), c("Phylum", "Order", "Family", "Genus", "Species")))]
  
  return(data)
}
