## server.R

# load functions
#source('functions/cf_algorithm.R') # collaborative filtering
#source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]

  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  mv_ids = match(dat$MovieID, movies$MovieID)
  user_ratings <- sparseMatrix(i = mv_ids,
                               j = rep(1,nrow(dat)), 
                               x = dat$Rating,
                               dims = c(ncol(ratingmat), 1))

}

# read sys1 data
get_user_genre_movie = function(user_input){
  sys1_data = read.csv('data/sys1_by_rating.csv', sep='\t')
  user_genre_moive <- subset(sys1_data, single_genre == user_input)
}


# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)

#ratingmat = Rmat
ratingmat = new('realRatingMatrix', data = Rmat)
#rec = Recommender(ratingmat, method = "POPULAR")
rec = Recommender(ratingmat, method = "UBCF", param=list(nn=500))

shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  output$sys1_results <- renderUI({
    num_rows <- 2
    num_movies <- 5 # movies per row
	
	user_genre_moive <- get_user_genre_movie(input$genre)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = user_genre_moive$img_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(user_genre_moive$title[(i - 1) * num_movies + j])),
				 div(style = "text-align:center", paste0('Avg Rating: ', user_genre_moive$rating[(i - 1) * num_movies + j])),
				 ))
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list) #movie x user
        newdata = new('realRatingMatrix', data = t(user_ratings))

        pred = predict(rec, newdata, n = 10)
        user_results = as(pred, "list")  
        clean_user_results = substr(user_results[[1]], 2, nchar(user_results[[1]]))      
        user_predicted_ids = match(clean_user_results, movies$MovieID)

        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = movies$MovieID[user_predicted_ids], 
                                    MovieID_ids = user_predicted_ids,                                                               
                                    Title = movies$Title[user_predicted_ids],                                     
                                    Predicted_rating =  user_results)

    }) # still busy
    
  }) # clicked on button
  

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
#print(recom_result$MovieID[(1 - 1) * num_movies + 1])
#print(recom_result$MovieID[(1 - 1) * num_movies + 2])
#print(recom_result$MovieID[(2 - 1) * num_movies + 1])
#print(recom_result$MovieID[(2 - 1) * num_movies + 2])
#print(movies$Title[recom_result$MovieID_ids[(1 - 1) * num_movies + 1]])
#print(movies$Title[recom_result$MovieID_ids[(1 - 1) * num_movies + 2]])
#print(movies$Title[recom_result$MovieID_ids[(2 - 1) * num_movies + 1]])
#print(movies$Title[recom_result$MovieID_ids[(2 - 1) * num_movies + 2]])
#print(movies$Title[recom_result$MovieID_ids[(2 - 1) * num_movies + 3]])

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
          div(style = "text-align:center", 
              a(img(src = movies$image_url[recom_result$MovieID_ids[(i - 1) * num_movies + j]], height = 150))
             ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies$Title[recom_result$MovieID_ids[(i - 1) * num_movies + j]])
             )
          
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
