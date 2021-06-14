library(GEOquery)
download_genexp_data<-function(geo_txt){

  geo_matrix <- getGEO(geo_txt, GSEMatrix=FALSE)
  
  gsmlist<-GSMList(geo_matrix)
  # get the probeset ordering
  probesets <- Table(GPLList(geo_matrix)[[1]])$ID
  # make the data matrix from the VALUE columns from each GSM
  # being careful to match the order of the probesets in the platform
  # with those in the GSMs
  data.matrix <- do.call('cbind',lapply(gsmlist,function(x) 
  {tab <- Table(x)
  mymatch <- match(probesets,tab$ID_REF)
  return(tab$VALUE[mymatch])
  }))
  data.matrix <- apply(data.matrix,2,function(x) {as.numeric(as.character(x))})
  data.matrix <- log2(data.matrix)
  rownames(data.matrix) <- probesets
  
  tejidos <- sapply(gsmlist,function(x) Meta(x)$characteristics_ch1)
  t_df<-data.frame(t(data.matrix))
  
  list(genexp=t_df, tejidos=tejidos)
}

download_probeset_data<-function(){
  #Descarga datos del microarray ABI Human Genome Survey Microarray Version 2
  geo_matrix <- getGEO('GPL2986', GSEMatrix=FALSE)
  Table(geo_matrix)
}