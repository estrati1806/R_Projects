-- In this script, I will create a new table, fill it with data from philosophical_books.csv, and then query it.

-- DATA DEFINITION LANGUAGE
-- CREATING AN EMPTY TABLE USING THE COLUMNS IN THE CSV
CREATE TABLE Philosophical_Books (
    Title VARCHAR(255),
    Author VARCHAR(255),
    Avg_Rating DECIMAL(3,2),
    Ratings INT,
    Score INT,
    Votes INT
);

-- LOADING DATA INTO THE TABLE
LOAD DATA INFILE '/Cloud/project/philosophical_books.csv'
INTO TABLE Philosophical_Books
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
(Title, Author, Avg_Rating, Ratings, Score, Votes);

-- ADDING PRIMARY KEY
ALTER TABLE Philosophical_Books
ADD COLUMN ID INT AUTO_INCREMENT PRIMARY KEY;


-- DATA MANIPULATION LANGUAGE
-- HOW MANY BOOKS ARE IN THE TABLE?
SELECT COUNT(*) FROM Philosophical_Books;

-- TOP 20 AUTHORS BASED ON AVERAGE RATING OF ALL BOOKS IN THE LIST & HOW MANY PEOPLE VOTED FOR THEM IN TOTAL
SELECT 
    Author,
    AVG(Avg_Rating) AS AverageRating,
    SUM(Votes) AS TotalVotes
FROM Philosophical_Books
GROUP BY Author
ORDER BY AverageRating DESC
LIMIT 20;

-- TOP 15 HIGHEST RATED BOOKS, BASED ON AVG_RATING
-- THEIR ID SHOWS THEIR RANK ON THE LIST, BUT IT IS NOT DEPENDENT ON AVG-RATING
SELECT
    ID,
    Title,
    Author,
    Avg_Rating,
    RANK() OVER (ORDER BY Avg_Rating DESC) AS Rank
FROM Philosophical_Books
ORDER BY Avg_Rating DESC
LIMIT 15;

-- AUTHORS WITH TWO OR MORE BOOKS ON THE LIST, RANKED BASED ON THE TOTAL NUMBER OF BOOKS
SELECT
    Author,
    COUNT(*) AS NumberOfBooks,
    RANK() OVER (ORDER BY COUNT(*) DESC) AS AuthorRank
FROM Philosophical_Books
GROUP BY Author
HAVING COUNT(*) >= 2
ORDER BY AuthorRank;
