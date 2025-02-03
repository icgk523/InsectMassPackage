convert_mass = function(data, Species = NULL, Value = NULL, Metric = NULL, Live_to_Dry = FALSE, n.cores = NULL){
  data = check_column(data, Species, "Species")
  data = check_column(data, Value, "Value")
  data = check_column(data, Metric, "Metric")

  data = check_data(data, Species, Value) # Check will remove species which include "sp." and other non-valid species names from the data frame
  data = convert_data(data)

  taxonomy = parallel_extract_gbif_taxonomy(data, species_column = Species, n.cores = n.cores)
  
  return(taxonomy)
}
