# Set-up ----
library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)

dbListTables(con)

dbGetQuery(con, "PRAGMA table_info(actor)")

# E1 ----
dbGetQuery(con, "
  SELECT actor_id, first_name, last_name
  FROM actor
  ORDER BY last_name, first_name
")


# E2 ----
dbGetQuery(con, "
  SELECT actor_id, first_name, last_name
  FROM actor
  WHERE last_name IN ('WILLIAMS', 'DAVIS')
")


# E3 ----
dbGetQuery(con, "
  SELECT DISTINCT customer_id
  FROM rental
  WHERE date(rental_date) = '2005-07-05'
")


# E4.1 ----
dbGetQuery(con, "
  SELECT *
  FROM payment
  WHERE amount IN (1.99, 7.99, 9.99)
")

# E4.2 ----
dbGetQuery(con, "
  SELECT *
  FROM payment
  WHERE amount > 5
")

# E4.3 ----
dbGetQuery(con, "
  SELECT *
  FROM payment
  WHERE amount > 5 AND amount < 8
")


# E5 ----
dbGetQuery(con, "
  SELECT payment.payment_id, payment.amount
  FROM payment
    INNER JOIN customer ON payment.customer_id = customer.customer_id
  WHERE customer.last_name = 'DAVIS'
")


# E6.1 ----
dbGetQuery(con, "
  SELECT COUNT(*)
  FROM rental
")

# 6.2 ----
dbGetQuery(con, "
  SELECT customer_id, COUNT(*) AS rental_count
  FROM rental
  GROUP BY customer_id
")

# 6.3 ----
dbGetQuery(con, "
  SELECT customer_id, COUNT(*) AS rental_count
  FROM rental
  GROUP BY customer_id
  ORDER BY rental_count DESC
")

# 6.4 ----
dbGetQuery(con, "
  SELECT customer_id, COUNT(*) AS rental_count
  FROM rental
  GROUP BY customer_id
  HAVING COUNT(*) >= 40
  ORDER BY rental_count DESC
")


# 7.0 ----
dbGetQuery(con, "
  SELECT 
    MAX(amount) AS max_amount,
    MIN(amount) AS min_amount,
    AVG(amount) AS avg_amount,
    SUM(amount) AS total_amount
  FROM payment
")

# 7.1 ----
dbGetQuery(con, "
  SELECT 
    customer_id,
    MAX(amount) AS max_amount,
    MIN(amount) AS min_amount,
    AVG(amount) AS avg_amount,
    SUM(amount) AS total_amount
  FROM payment
  GROUP BY customer_id
")

# 7.2 ----
dbGetQuery(con, "
  SELECT 
    customer_id,
    MAX(amount) AS max_amount,
    MIN(amount) AS min_amount,
    AVG(amount) AS avg_amount,
    SUM(amount) AS total_amount
  FROM payment
  GROUP BY customer_id
  HAVING COUNT(*) > 5
")

# Clean up ----
dbDisconnect(con)
