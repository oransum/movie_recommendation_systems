Movie Recommendation Systems
================
Oran Chan, Edward Ma
Dec-8-2020

-----


```r
library(readr)
library(recommenderlab)
```

```
## Loading required package: Matrix
```

```
## Loading required package: arules
```

```
## 
## Attaching package: 'arules'
```

```
## The following objects are masked from 'package:base':
## 
##     abbreviate, write
```

```
## Loading required package: proxy
```

```
## 
## Attaching package: 'proxy'
```

```
## The following object is masked from 'package:Matrix':
## 
##     as.matrix
```

```
## The following objects are masked from 'package:stats':
## 
##     as.dist, dist
```

```
## The following object is masked from 'package:base':
## 
##     as.matrix
```

```
## Loading required package: registry
```

```
## Registered S3 methods overwritten by 'registry':
##   method               from 
##   print.registry_field proxy
##   print.registry_entry proxy
```

```r
library(Matrix)
```

## System 1 - by Movie Popularity

- In this system, we discussed two recommendation schemes which are top-ten most popular movies and top-ten highly-rated movies. 

- For top-ten most popular, it counts the number of reviews per movie and selects top 10 counts per genre. Meanwhile, top-ten highly-rated movies are averaging ratings across reviews per movie and selecting top 10 average ratings movies per genre. We decided to go for an enhanced high-rated approach. Popular movies do not mean good choices for the user. On the other hand, it may be misleading if only averaging rating because it can only exist with one 5-star rating.

- Therefore, we consider both popular and highly-rated movies. We average rating by movie and filter movies if number of reviews is less than 60. The reason for using 60 is because it is approximately 1% of all users. It can be further fine-tuned to achieve a better user experience. Finally, we show top-10 highly-rated movies by genre to users.

- The pre-processed file, "sys1_by_rating.csv", is storing the top-10 highly-rated movies by genre.

```r
sys1_by_rating <- read_delim("sys1_by_rating.csv", 
    "\t", escape_double = FALSE, trim_ws = TRUE)
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   movie_id = col_double(),
##   title = col_character(),
##   year = col_double(),
##   single_genre = col_character(),
##   rating = col_double(),
##   count = col_double(),
##   img_url = col_character()
## )
```

```r
str(sys1_by_rating)
```

```
## tibble [180 × 7] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ movie_id    : num [1:180] 2905 2019 1198 260 858 ...
##  $ title       : chr [1:180] "Sanjuro (1962)" "Seven Samurai (The Magnificent Seven) (Shichinin no samurai) (1954)" "Raiders of the Lost Ark (1981)" "Star Wars: Episode IV - A New Hope (1977)" ...
##  $ year        : num [1:180] 1962 1954 1981 1977 1972 ...
##  $ single_genre: chr [1:180] "Action" "Action" "Action" "Action" ...
##  $ rating      : num [1:180] 4.6 4.6 4.5 4.5 4.5 4.4 4.3 4.3 4.3 4.3 ...
##  $ count       : num [1:180] 69 628 2514 2991 2223 ...
##  $ img_url     : chr [1:180] "https://liangfgithub.github.io/MovieImages/2905.jpg?raw=true" "https://liangfgithub.github.io/MovieImages/2019.jpg?raw=true" "https://liangfgithub.github.io/MovieImages/1198.jpg?raw=true" "https://liangfgithub.github.io/MovieImages/260.jpg?raw=true" ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   movie_id = col_double(),
##   ..   title = col_character(),
##   ..   year = col_double(),
##   ..   single_genre = col_character(),
##   ..   rating = col_double(),
##   ..   count = col_double(),
##   ..   img_url = col_character()
##   .. )
```

```r
head(sys1_by_rating)
```

```
## # A tibble: 6 x 7
##   movie_id title               year single_genre rating count img_url           
##      <dbl> <chr>              <dbl> <chr>         <dbl> <dbl> <chr>             
## 1     2905 Sanjuro (1962)      1962 Action          4.6    69 https://liangfgit…
## 2     2019 Seven Samurai (Th…  1954 Action          4.6   628 https://liangfgit…
## 3     1198 Raiders of the Lo…  1981 Action          4.5  2514 https://liangfgit…
## 4      260 Star Wars: Episod…  1977 Action          4.5  2991 https://liangfgit…
## 5      858 Godfather, The (1…  1972 Action          4.5  2223 https://liangfgit…
## 6     1221 Godfather: Part I…  1974 Action          4.4  1692 https://liangfgit…
```

- There are 18 genres and each of them has top-10 highly-rated movies. 

```r
length(unique(sys1_by_rating$single_genre))
```

```
## [1] 18
```

```r
nrow(sys1_by_rating)
```

```
## [1] 180
```

```r
subset(sys1_by_rating, single_genre == "Animation", select=c(title, rating))
```

```
## # A tibble: 10 x 2
##    title                                                  rating
##    <chr>                                                   <dbl>
##  1 Close Shave, A (1995)                                     4.5
##  2 Wrong Trousers, The (1993)                                4.5
##  3 Wallace & Gromit: The Best of Aardman Animation (1996)    4.4
##  4 Grand Day Out, A (1992)                                   4.4
##  5 Creature Comforts (1990)                                  4.3
##  6 Toy Story 2 (1999)                                        4.2
##  7 Princess Mononoke, The (Mononoke Hime) (1997)             4.1
##  8 Toy Story (1995)                                          4.1
##  9 Ghost in the Shell (Kokaku kidotai) (1995)                4.1
## 10 Iron Giant, The (1999)                                    4
```

```r
subset(sys1_by_rating, single_genre == "Romance", select=c(title, rating))
```

```
## # A tibble: 10 x 2
##    title                            rating
##    <chr>                             <dbl>
##  1 Casablanca (1942)                   4.4
##  2 City Lights (1931)                  4.4
##  3 Cinema Paradiso (1988)              4.3
##  4 Princess Bride, The (1987)          4.3
##  5 Singin' in the Rain (1952)          4.3
##  6 African Queen, The (1951)           4.3
##  7 Notorious (1946)                    4.3
##  8 Philadelphia Story, The (1940)      4.3
##  9 Run Lola Run (Lola rennt) (1998)    4.2
## 10 Graduate, The (1967)                4.2
```

## System 2 - Recommend by Collbaortive Filtering

### Data Pre-processing

```r
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
```

#### Create utility matrix

```r
set.seed(830)
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
```

### Evaluation Scheme
- An evaluation scheme is created and it splits by user with 80% of them in training set and a 20% in test set. For the test set, 15 items will be given to the recommender algorithm and the other items will be held out for computing the error. 
- The cross validation process will be carried out in 10 runs.


```r
# EvaluationScheme
es = evaluationScheme(Rmat, method="cross-validation", k=10, train = 0.8, goodRating = 5, given = 15)
es
```

```
## Evaluation scheme with 15 items given
## Method: 'cross-validation' with 10 run(s).
## Good ratings: >=5.000000
## Data set: 6040 x 3706 rating matrix of class 'realRatingMatrix' with 1000209 ratings.
```
#### Evaluate different algorithms
Four collaborative filtering algorithms will be evaluated.
  - Random items
  - Popular items*
  - User-based CF
  - Item-based CF
*Popular items algorithm is to average existing ratings of a particular movie during prediction of that movie.


```r
algorithms = list( "Random items" = list(name="RANDOM", param=NULL),
                    "Popular items" = list(name="POPULAR", param=NULL),
                    "User-based CF" = list(name="UBCF", param=list(nn=500)),
                    "Item-based CF" = list(name="IBCF", param=list(k=50))
                    )

ev_results <- evaluate(es, algorithms, type = "ratings")
```

```
## RANDOM run fold/sample [model time/prediction time]
## 	 1  [0.014sec/0.839sec] 
## 	 2  [0.006sec/0.559sec] 
## 	 3  [0.008sec/0.555sec] 
## 	 4  [0.006sec/0.538sec] 
## 	 5  [0.006sec/0.554sec] 
## 	 6  [0.006sec/0.544sec] 
## 	 7  [0.006sec/0.533sec] 
## 	 8  [0.006sec/0.556sec] 
## 	 9  [0.005sec/0.553sec] 
## 	 10  [0.006sec/0.542sec] 
## POPULAR run fold/sample [model time/prediction time]
## 	 1  [0.053sec/0.327sec] 
## 	 2  [0.047sec/0.287sec] 
## 	 3  [0.051sec/0.345sec] 
## 	 4  [0.044sec/0.357sec] 
## 	 5  [0.043sec/0.439sec] 
## 	 6  [0.055sec/0.244sec] 
## 	 7  [0.049sec/0.492sec] 
## 	 8  [0.051sec/0.604sec] 
## 	 9  [0.046sec/0.406sec] 
## 	 10  [0.044sec/0.378sec] 
## UBCF run fold/sample [model time/prediction time]
## 	 1  [0.04sec/46.37sec] 
## 	 2  [0.037sec/45.63sec] 
## 	 3  [0.034sec/45.75sec] 
## 	 4  [0.036sec/45.72sec] 
## 	 5  [0.035sec/45.45sec] 
## 	 6  [0.035sec/46.28sec] 
## 	 7  [0.038sec/45.12sec] 
## 	 8  [0.04sec/44.53sec] 
## 	 9  [0.035sec/44.74sec] 
## 	 10  [0.036sec/46.44sec] 
## IBCF run fold/sample [model time/prediction time]
## 	 1  [95.39sec/0.16sec] 
## 	 2  [96.74sec/0.179sec] 
## 	 3  [97.59sec/0.284sec] 
## 	 4  [100.1sec/0.279sec] 
## 	 5  [98.18sec/0.272sec] 
## 	 6  [97.94sec/0.301sec] 
## 	 7  [97.47sec/0.143sec] 
## 	 8  [98.59sec/0.177sec] 
## 	 9  [98.06sec/0.134sec] 
## 	 10  [96.27sec/0.147sec]
```
### Evaluation Results


```r
plot(ev_results, annotate=c(1,3), type='b', legend="bottomright", col=c('darkred','cyan','orange','darkgreen'))
```

![](PSL_project4_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### Prediction Error comparison

```r
names(ev_results)
```

```
## [1] "Random items"  "Popular items" "User-based CF" "Item-based CF"
```

```r
error = rbind(
  avg(ev_results[[1]]),
  avg(ev_results[[2]]),
  avg(ev_results[[3]]),
  avg(ev_results[[4]])
)

rownames(error) = names(ev_results)
error
```

```
##                 RMSE    MSE    MAE
## Random items  1.4070 1.9798 1.1061
## Popular items 0.9596 0.9209 0.7574
## User-based CF 1.0180 1.0365 0.8056
## Item-based CF 1.4745 2.2250 1.1066
```
- Although Popular method has a lower prediction error among others, it will return the same rank of movies for the same user. So the next better algorithm will be considered.
- User-based collaborative filtering achieves a lower prediction error.
- Such that, UBCF will be used for system 2, the Movie Recommendation app.

### Simulation

- To simulate System 2, we will train a UBCF recommender with full set of rating data.

```r
rec = Recommender(Rmat, method = "UBCF", param=list(nn=500))
```

- Pre process movies.dat

```r
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
```

- Rate three movies as an input

```r
match("Toy Story (1995)", movies$Title) #1
```

```
## [1] 1
```

```r
match("Wings of Courage (1995)", movies$Title) #33
```

```
## [1] 33
```

```r
match("Pocahontas (1995)", movies$Title) #48
```

```
## [1] 48
```


```r
movie_idx = c(1,33,48)
new_rating = c(5,3,4)
new_r = data.frame("MovieID" = movie_idx, "Rating" = new_rating)

new_user_rating <- sparseMatrix(i = new_r$MovieID, 
                               j = rep(1,nrow(new_r)), 
                               x = new_r$Rating,
                               dims = c(ncol(Rmat), 1))

newdata = new('realRatingMatrix', data = t(new_user_rating))
```

- Recommend top 10 movies

```r
pred = predict(rec, newdata, n = 10)

user_results = as(pred, "list")
movies$Title[match(substr(user_results[[1]], 2, nchar(user_results[[1]])), movies$MovieID)]
```

```
##  [1] "Some Mother's Son (1996)"                       
##  [2] "Four Days in September (1997)"                  
##  [3] "Steal Big, Steal Little (1995)"                 
##  [4] "Battling Butler (1926)"                         
##  [5] "Better Living Through Circuitry (1999)"         
##  [6] "Belizaire the Cajun (1986)"                     
##  [7] "I Am Cuba (Soy Cuba/Ya Kuba) (1964)"            
##  [8] "Best Man, The (Il Testimone dello sposo) (1997)"
##  [9] "Nil By Mouth (1997)"                            
## [10] "Lady of Burlesque (1943)"
```

## Reference
- Michael Hahsler (2020). recommenderlab: Lab for Developing and
  Testing Recommender Algorithms. R package version 0.2-6.
  https://github.com/mhahsler/recommenderlab
- https://rdrr.io/cran/recommenderlab/src/R/evaluate.R
- UIUC - Practical Statistical Learning - Movielense Data: Exploratory Data Analysis
- UIUC - Practical Statistical Learning - Movielense Data: Recommender System


