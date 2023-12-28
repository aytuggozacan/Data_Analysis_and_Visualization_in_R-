#Exercise 2
library(data.table)
library(magrittr)

#Section 1

#to see working directory
getwd() 

#to importing data as a data.table from working directory
books_dt <- fread(file.path("extdata", "BX-Books.csv")) 
class(book_dt)
users_dt <- fread(file.path("extdata", "BX-Users.csv"))
class(users_dt)
ratings_dt <- fread(file.path("extdata", "BX-Book-Ratings.csv")) 
class(ratings_dt)

#1.3
sapply(users_dt, class) 
#sapply apply the class funtion on all cloumn of users_dt

users_dt$Age <- as.numeric(users_dt$Age)
class(users_dt$Age)

#1.4
summary(books_dt)

#1.5
head(ratings_dt, n = 5)
tail(ratings_dt, n = 5)

#1.6
s <- names(books_dt)
names(books_dt) <- gsub("-","_", s)

s1 <- names(users_dt)
names(users_dt) <-  gsub("-","_",s1)

s2 <- names(ratings_dt)
names(ratings_dt) <- gsub("-","_",s2)

#1.7

#Selecting single and multiple columns without $
books_dt[, Image_URL_S]
books_dt[,c("Image_URL_S", "Image_URL_M", "Image_URL_L")]

# use - to creating subsets without selected colums
books_st <- books_dt[,-c("Image_URL_S", "Image_URL_M", "Image_URL_L")]
# operator := can also be used for deleting columns
books_dt[,-c("Image_URL_S", "Image_URL_M", "Image_URL_L") := NULL]

#1.8

book_dt_2 <- books_dt[1900 <= Year_Of_Publication & Year_Of_Publication <=2019]

#Section 2

#2.1
books_dt[, unique(Book_Author)]

#2.2
books_dt[Year_Of_Publication %in% 2000:2010, unique(Book_Author), 
         by = Year_Of_Publication]

#2.3
table(is.na(users_dt$Age))
#or
users_dt[is.na(Age), .N]

#2.4
max(ratings_dt$Book_Rating)
#or
ratings_dt[, max(Book_Rating)]

#2.5
ratings_dt[Book_Rating > 0, .N, by = Book_Rating][N == max(N),]

#2.6
ratings_dt[Book_Rating == max(Book_Rating), "ISBN"]

#2.7
indices <- order(ratings_dt$Book_Rating, decreasing = TRUE)
ratings_dt <- ratings_dt[indices]

setorder(ratings_dt, -Book_Rating)
ratings_dt

#Section 3

ratings_dt[, High_Rating := ifelse(Book_Rating > 7, 1, 0)]

ratings_dt[, sum(High_Rating)] # absolute
ratings_dt[, sum(High_Rating)/.N] # relative

users_who_rated <- ratings_dt[,User_ID] users_dt[! (User_ID %in%users_who_rated)]

users_dt[User_ID%in%users_who_rated & !is.na(Age), .N, by=Age][N==max(N)]

ratings_dt[, .N, by=User_ID][, mean(N, na.rm=TRUE)]
ratings_dt[order(Year_Of_Publication, -Book_Rating),
           .(Book_Title, Year_Of_Publication, Book_Rating)] %>% head(1)

ratings_dt[, Rating_Count:=.N, by=ISBN]
ratings_dt[ Rating_Count == max(Rating_Count), max(Year_Of_Publication)]

ratings_dt[, Max_Book_Rating := max(Book_Rating), by=ISBN] ratings_dt

authors <- c("Agatha Christie", "William Shakespeare", "Stephen King", 
             "Ann M. Martin", "Carolyn Keene", "Francine Pascal",
             "Isaac Asimov", "Nora Roberts", "Barbara Cartland", "Charles Dickens")
authors

ratings_dt_sub <- ratings_dt[Book_Author %in% authors]
ratings_dt_sub[, .(mean(Book_Rating), max(Book_Rating), .N), by=Book_Author]
           
#Section 4
library(readxl)

oly_file <- file.path("extdata","summer_olympic_medals.xlsx") 
oly_df <- read_xlsx(oly_file, sheet='ALL MEDALISTS') 
head(oly_df)

# There are different solutions for this
oly_dt <- as.data.table(oly_df) 
bronze <- oly_dt[Medal == "Bronze",]
# 4. Using .N command from data.table. More to this in Data Table lecture
bronze[, N := .N, by = Athlete] bronze[N == max(N), unique(Athlete)]

# There was a male Bronze-medal winner in ladies marathon in 2000.
oly_dt[, unique(Gender)]

oly_dt[, unique(Event_gender)]

oly_dt[Gender == "Men" & !Event_gender %in% c("M", "X")]
oly_dt[Gender == "Women" & !Event_gender %in% c("W", "X")]

# There is also a summary sheet for nations
nation_medal_df <- read_excel(oly_file, sheet='COUNTRY TOTALS', range="A147:F286") 
nation_medal_dt <- as.data.table(nation_medal_df)
head(nation_medal_dt)

# Remove Grand.Total row
nation_medal_dt <- nation_medal_dt[!is.na(Country)]
# Get max
nation_medal_dt[`Grand Total` == max(`Grand Total`, na.rm = T)]

# Get highest ratio
nation_medal_dt[, silver.ratio := Silver/`Grand Total`, by = Country] 
nation_medal_dt[silver.ratio == max(silver.ratio, na.rm = TRUE), Country]

participants <- read_excel(oly_file, sheet='IOC COUNTRY CODES', range="A1:C202") 
participants.dt <- as.data.table(participants)
head(participants.dt)

## make sure to have proper variable names
colnames(participants.dt) <- make.names(colnames(participants.dt))
no_medals <- setdiff(participants.dt[, Int.Olympic.Committee.code], nation_medal_dt[, NOC])
length(no_medals)

participants.dt[Int.Olympic.Committee.code %in% no_medals, Country]






