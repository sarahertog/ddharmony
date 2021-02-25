
births_load <- function(){

req <- GET("https://api.github.com/repos/sarahertog/ddharmony/git/trees/main?recursive=1")
stop_for_status(req)
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)

for (filename in filelist) {
  
  one_function <- paste0("https://github.com/sarahertog/ddharmony/blob/main/", filename, "?raw=TRUE")
  source_url(one_function)
  rm(one_function)
}
rm(req, filelist, filename)

# Births by age of mother: vr
births_vr <- try(DDharmonize_validate_BirthCounts(locid = lid, process = "vr", times = 1950:2020, retainKeys = TRUE))
assign("births_vr", births_vr , envir = .GlobalEnv)

# Births by age of mother: census
births_census <- try(DDharmonize_validate_BirthCounts(locid = lid, process = "census", times = 1950:2020, retainKeys = TRUE))
assign("births_census", births_census , envir = .GlobalEnv)

}