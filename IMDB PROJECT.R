
## Profit Obtained By Different Countries Films in the Corresponding Years


library(ggplot2)
movie_data = read.csv("movie_metadata.csv")
movie_data$profit = movie_data$gross - movie_data$budget
ggplot(movie_data, aes(x = title_year, y = profit ,fill = country)) + 
  geom_bar(width = 0.85, stat="identity", main = "Profit Of Different Country Films in Different Years")

## Frequency Of Movies Under Different Category Of Ratings ##

movie_data = read.csv("movie_metadata.csv")
library(ggplot2)
p = table(movie_data$content_rating)
y = data.frame(p)
y
rate = y$Var1
frequncy = y$Freq
rate
library(ggplot2)
ggplot(y, main = "Percentage of ratings For all Movies", aes(x=rate, y=frequncy, fill = rate)) +
  geom_bar(stat='identity') +
  coord_flip()

## Number Of  Movies in a year in different countries ##

movie_data = read.csv("movie_metadata.csv")
library(ggplot2)
p = table(movie_data$title_year)
p
y = data.frame(p)
y
year = y$Var1
frequncy = y$Freq
g = ggplot(y, aes(x=year, y=frequncy, fill = year)) +
  geom_bar(stat = "identity", main = "Movies in a year in different countries") 
g + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))


## Number Of Movies Under Each Genre ##

movie_data = read.csv("movie_metadata.csv")
library(ggplot2)
sp1.l <- strsplit(as.character(movie_data$genres),"|", fixed = TRUE)
sp1.l
MovieGenres = unlist(sp1.l, recursive = FALSE, use.names = TRUE)
MovieGenres
count <- table(MovieGenres)
count
ss <-  data.frame(count)
ss
g = ggplot(ss, aes(x = MovieGenres, y = Freq, fill = MovieGenres)) +
  geom_bar(stat = "identity", main = "Number Of Movies Under Each Genre") 
g + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

g + coord_polar(theta = "x", start = 30)


## Regression Between  Critic Reviews and Gross ##

movie_data = read.csv("movie_metadata.csv")
movie_data
gross_rating <- data.frame(movie_data$imdb_score, movie_data$gross)
gross_rating
gross <- c(gross_rating$movie_data.gross)
gross
rating <- c(gross_rating$movie_data.imdb_score)
rating
relation <- lm(gross ~ rating)
print(relation)
print(summary(relation))
a <- data.frame(rating = 6.78)
result <- predict(relation,a)
print(result)
library(ggplot2)
qplot(rating, gross, col =  movie_data$country, size = gross,main = "Critic Reviews & Gross ~ Regression",abline(lm(rating~gross),xlab = "IMDB Score",ylab = "Gross of the Movie",cex = 0.9))


##countries ~ densities of their imdb_score
Movies3 <- movie_data
ggplot(Movies3, aes(x=imdb_score,fill = country)) + geom_density()


