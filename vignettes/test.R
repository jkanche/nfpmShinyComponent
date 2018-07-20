library(networkViz)

test_network <- list("nodes"=list(list( "id"='a'), list("id"='b')),
  "links"=list(list("source"='a', "target"='b', "value"=1)))

chart <- networkViz:::NfpmViz(data=test_network)
chart


library(dplyr)



dataParser <- function(input, fda, fda_other, rec) {
  
  input <- distinct(input)
  fda <- distinct(fda)
  fda_other <- distinct(fda_other)
  rec <- distinct(rec)
  
  ###
  ## groups denotes the id and title for each section in the sankey diagram.
  ###
  json <- list(
    "links"=list(), 
    "nodes"=list(), 
    "groups"=list(
      "mp"=list(
        "type"="process",
        "title"="Molecular Profile",
        "bundle"=NULL,
        "id"="mp",
        "nodes"=c(),
        "def_pos"=NULL
      ),
      "fda"=list(
        "type"="process",
        "title"="FDA Approved Drugs",
        "bundle"=NULL,
        "id"="fda",
        "nodes"=c(),
        "def_pos"=NULL
      ),
      "it"=list(
        "type"="process",
        "title"="Inferred Targets",
        "bundle"=NULL,
        "id"="it",
        "nodes"=c(),
        "def_pos"=NULL
      ),
      "rec"=list(
        "type"="process",
        "title"="Recommended Therapies",
        "bundle"=NULL,
        "id"="rec",
        "nodes"=c(),
        "def_pos"=NULL
      )
    )
  )
  
  link <- list(      
    "color"="rgb(204, 235, 197)",
    "source"=NULL,
    "value"=10,
    "type"=NULL,
    "target"=NULL,
    "opacity"=1,
    "time"="*",
    "title"=NULL,
    "id"=NULL
  )
  
  node <- list(      
    "bundle"=NULL,
    "title"=NULL,
    "visibility"="visible",
    "def_pos"=NULL,
    "id"=NULL,
    "style"="process",
    "direction"="r"
  )
  
  node_count <- 1
  link_count <- 1
  
  for (i in rownames(input)) {
    row <- input[i, ]
    mp_node <- node
    mp_node$title <- row$Gene_protein
    mp_node$id <- paste0("mp^", row$Gene_protein)
    
    json$nodes[[node_count]] <- mp_node
    node_count <- node_count + 1
    
    json$groups$mp$nodes <- c(json$groups$mp$nodes, paste0("mp^", row$Gene_protein))
  }
  
  if(nrow(fda) >= 1 && !length(fda$Note) >= 1 ) {
    
    unique_fda <- aggregate(fda, by=list(fda$Drug, fda$`Gene or Protein`), FUN=paste)
    
    
    for (i in rownames(unique_fda)) {
      row <- unique_fda[i, ]
      
      fda_node <- node
      fda_node$title <- row$Drug
      fda_node$id <- paste0("fda^", row$Drug)
      
      json$nodes[[node_count]] <- fda_node
      node_count <- node_count + 1
      
      json$groups$fda$nodes <- c(json$groups$fda$nodes, paste0("fda^", row$Drug))
      
      fda_link <- link
      fda_link$source <- paste0("mp^", row[["Gene or Protein"]]) 
      fda_link$target <- paste0("fda^", row$Drug)
      fda_link$type <- "FDA approved Drugs"
      fda_link$title <- paste0("alteration: ", row$Alteration)
      fda_link$id <- paste0(fda_link$source, " -> ", fda_link$target)
      
      json$links[[link_count]] <- fda_link
      link_count <- link_count + 1
    }
  }
  
  if(nrow(fda_other) >= 1) {
    
    unique_fda_other <- aggregate(fda_other, by=list(fda_other$Drug, fda_other$`Gene or Protein`), FUN=paste)
    
    
    for (i in rownames(unique_fda_other)) {
      row <- unique_fda_other[i, ]
      
      fda_other_node <- node
      fda_other_node$title <- row[["Group.1"]]
      fda_other_node$id <- paste0("fda^", row[["Group.1"]], " (other tumor type)")
      
      json$nodes[[node_count]] <- fda_other_node
      node_count <- node_count + 1
      
      json$groups$fda$nodes <- c(json$groups$fda$nodes, 
                                 paste0("fda^", row[["Group.1"]], " (other tumor type)"))
      
      fda_link <- link
      fda_link$source <- paste0("mp^", row[["Group.2"]]) 
      fda_link$target <- paste0("fda^", row[["Group.1"]], " (other tumor type)")
      fda_link$type <- "FDA approved Drugs"
      fda_link$title <- paste0("alteration: ", row$Alteration, "\n <br> tumor drug is approved for: ", row[["Tumor in which it is approved"]])
      fda_link$id <- paste0(fda_link$source, " -> ", fda_link$target)
      
      json$links[[link_count]] <- fda_link
      link_count <- link_count + 1
    }
  }
  
  if(nrow(rec) >= 1) {
    unique_rec <- aggregate(rec, by=list(rec$Drug, rec$`Gene or Protein`), FUN=paste)
    
    for (i in rownames(unique_rec)) {
      row <- unique_rec[i, ]
      
      # rec_node <- node
      # rec_node$title <- row$Drug
      # rec_node$id <- paste0("rec^", row$Drug)
      
      # json$nodes[[node_count]] <- rec_node
      # node_count <- node_count + 1
      
      # json$groups$rec$nodes <- c(json$groups$rec$nodes, paste0("rec^", row$Drug))
      
      fda_link <- link
      fda_link$source <- paste0("it^", row[["Group.2"]]) 
      fda_link$target <- paste0("rec^", row[["Group.1"]])
      fda_link$type <- "Recommended therapies"
      fda_link$title <- paste0("alteration: ", row$Alteration, "\n <br> tumor drug is approved for: ", row[["Tumor in which it is approved"]],
                               "\n <br> pathway: ", row$Path)
      fda_link$id <- paste0(fda_link$source, " -> ", fda_link$target)
      
      json$links[[link_count]] <- fda_link
      link_count <- link_count + 1
    }
    
    for (i in unique(rec[["Drug"]])) {
      rec_node <- node
      rec_node$title <- i
      rec_node$id <- paste0("rec^", i)
      
      json$nodes[[node_count]] <- rec_node
      node_count <- node_count + 1
      
      json$groups$rec$nodes <- c(json$groups$rec$nodes, paste0("rec^", i))
    }
    
    for (i in unique(rec[["Gene or Protein"]])) {
      rec_node <- node
      rec_node$title <- i
      rec_node$id <- paste0("it^", i)
      
      json$nodes[[node_count]] <- rec_node
      node_count <- node_count + 1
      
      json$groups$it$nodes <- c(json$groups$it$nodes, paste0("it^", i))
    }
    
    for (i in unique(rec[["Path"]])) {
      
      paths <- strsplit(i, "->")
      
      rec_link <- link
      rec_link$source <- paste0("mp^", trimws(paths[[1]][1]))
      rec_link$target <- paste0("it^", trimws(paths[[1]][length(paths[[1]])]))
      rec_link$type <- "Pathway"
      rec_link$title <- i
      rec_link$id <- i
      
      json$links[[link_count]] <- rec_link
      link_count <- link_count + 1
    }
  }
  
  gmp <- json$groups$mp
  gfda <- json$groups$fda
  git <- json$groups$it
  grec <- json$groups$rec
  
  # order <- list(list(gmp$nodes, gfda$nodes), list(list(git$nodes)), list(list(grec$nodes)))
  # json$order <- order
  
  groups <- list(gmp, gfda, git, grec)
  json$groups <- groups
  json
}




# aggregate(Type3_df, by=list(Type3_df$Drug, Type3_df$`Gene or Protein`), FUN=paste)


res <- dataParser(MP, Type1_df, Type2_df, Type3_df)

chart <- networkViz:::NfpmViz(data=res)
chart




