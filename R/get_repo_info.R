pull_repos <- function() {
  
  # Creating some Venn Diagrams from Repository subject lists in the RE3Data repository:
  
  if (!all(c('repo_subjects.RDS', 'repo_list.RDS') %in% list.files('data'))) {
  
    # Pulls repositories from the re3data API:
    repos <- httr::GET(url = "http://service.re3data.org/api/v1/repositories")
    repo_list <- xml2::as_list(read_xml(repos$content))
    
    repo_subjects <- lapply(repo_list, function(x) {
      indiv_repo <- httr::GET(paste0("http://service.re3data.org/api/v1/repository/", x$id[[1]]))
      indiv_repo <- read_xml(indiv_repo)
      list(repository = as.character(xml_contents(xml_find_all(indiv_repo, "//r3d:repositoryName"))),
           subjects = as.character(xml_contents(xml_find_all(indiv_repo, "//r3d:subject"))))
    })
  } else {
    repo_list <- readRDS('data/repo_list.RDS')
    repo_subjects <- readRDS('data/repo_subjects.RDS')
  }
  
  full_expanse <- do.call(rbind.data.frame, lapply(repo_subjects, 
                              function(x){
                                if (length(x$subjects) == 0) {
                                  return(data.frame(repo = x$repository, 
                                                    subject = NA, stringsAsFactors = FALSE))
                                } else {
                                  return(data.frame(repo = x$repository, 
                                             subject = x$subjects, stringsAsFactors = FALSE))
                                }
                              }))
  
  discip <- full_expanse[grep("^\\d{3} ", full_expanse$subject),]
  broad_discip <- table(full_expanse[grep("^\\d{1} ", full_expanse$subject),])
  colnames(broad_discip) <- substr(colnames(broad_discip), 3, nchar(colnames(broad_discip)))
  
  setup <- table(apply(broad_discip, 1, function(x)paste0(colnames(broad_discip)[x > 0], collapse = '&')))
  aa <- as.numeric(setup)
  names(aa) <- names(setup)
  
  broad <- venneuler::venneuler(aa)

  short_repos <- unique(full_expanse$repo[full_expanse$subject %in% c("2 Life Sciences", "3 Natural Sciences")])
  short_expanse <- full_expanse[full_expanse$repo %in% short_repos,]
  
  sub_discip <- table(short_expanse[grep("^\\d{2} ", short_expanse$subject),])
  colnames(sub_discip) <- substr(colnames(sub_discip), 4, nchar(colnames(sub_discip)))
  
  setup <- table(apply(sub_discip, 1, function(x)paste0(colnames(sub_discip)[x > 0], collapse = '&')))
  aa <- as.numeric(setup)
  names(aa) <- names(setup)
  aa <- aa[aa > 1]
  
  sub <- venneuler::venneuler(aa)
  
  #subsub_discip <- table(full_expanse[grep("^\\d{3} ", full_expanse$subject),])
  #colnames(subsub_discip) <- substr(colnames(subsub_discip), 5, nchar(colnames(subsub_discip)))
  
  #setup <- table(apply(subsub_discip, 1, function(x)paste0(colnames(subsub_discip)[x > 0], collapse = '&')))
  #aa <- as.numeric(setup)
  #names(aa) <- names(setup)
  #subsub <- venneuler::ven
  
  return(list(broad, sub))
}
