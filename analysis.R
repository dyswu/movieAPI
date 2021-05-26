#install.packages("httr")
#install.packages("jsonlite")

#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("knitr")

library("knitr")

library("httr")
library("jsonlite")

library(dplyr)
library(tidyr)

source("apikey.R")
#print(tmdb_key)

key_param <- list("api_key" = tmdb_key, "base_url" = "https://api.themoviedb.org/3/", 
                  "profile_path" = "https://image.tmdb.org/t/p/h100", "actor_name" = "Brad Pitt")

get_trending_table <- function(query) {
  trending_table <- GET(paste0(query$base_url, "trending/person/week?api_key=", query$api_key))
  body <- fromJSON(content(trending_table, "text"))
  trending_df <- as.data.frame(body$results) %>%
    filter(known_for_department == "Acting") %>%
    mutate(Photo = paste0("!","[",name, "]", "(" ,paste0(key_param$profile_path, profile_path), ")")) %>%
    select(name, Photo) %>%
    head(5)
  return(trending_df)
}

trending_df <- get_trending_table(key_param)

get_actor_data <- function(query){
  actor_table <- GET(paste0(key_param$base_url, "search/person/?api_key=", key_param$api_key, "&query=", URLencode(query)))
  body <- fromJSON(content(actor_table, "text"))
  actor_df <- as.data.frame(body$results) #%>%
  actor_id <- actor_df$id
  actor_vector <- actor_df$known_for %>%
    as.data.frame() %>%
    select(title)
  extra_table <- GET(paste0(key_param$base_url, "person/", actor_id, "?api_key=", key_param$api_key))
  body <- fromJSON(content(extra_table, "text"))
  imdb <- as.data.frame(body$imdb_id)
  biography <- body$biography

  results <- list(query, actor_id, imdb, biography, actor_vector)
  return(results)
}

#actor_data <- get_actor_data("Brad Pitt")

analyze_actor <- function(query){
  analyze_table <- GET(paste0(query$base_url, "person/", 287,  "/movie_credits?api_key=", query$api_key))
  body <- fromJSON(content(analyze_table, "text"))
  body <- body[-c(1, 49, 88)]
  analyze_df <- as.data.frame(body, stringAsFactors = FALSE)
  analysis <- analyze_df %>%
    group_by(crew.job) %>%
    summarise(popularity = mean(crew.popularity), vote = mean(crew.vote_average))
  #print(class(analysis))
  #print(class(analyze_df))
  results <- list()
  results[[1]] <- analysis
  results[[2]] <- analyze_df
  #View(analyze_df)
  return(results)
}

analysis <- analyze_actor(key_param)
