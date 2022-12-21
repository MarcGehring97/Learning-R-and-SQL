
### 1. What is the last transaction day?

SELECT MAX(purchase_date) FROM BnL.Transactions;

#2020-11-30 21:19:00.0
 
### 2. What division is most profitable? 

SELECT division_desc, SUM(amount) FROM BnL.Transactions LEFT JOIN BnL.Products USING (item) GROUP BY 1 ORDER BY 2 DESC LIMIT 1;

#Beauty

### 3. What proportion of items in the Children-divison are returned?

SELECT sum(returned), count(returned) FROM BnL.Transactions LEFT JOIN BnL.Products USING (item) WHERE division_desc = 'Children'

#0

### 4. What is the class/dept/group/division of most sold product? 

SELECT * FROM (SELECT item, COUNT(*) FROM BnL.Transactions GROUP BY 1 ORDER BY 2 DESC LIMIT 5) t LEFT JOIN BnL.Products USING (item)

#NULL

### 5.	What is the class of the most sold item, excluding all items of unknown class? 

SELECT p.item AS item, MAX(class_desc) AS class, COUNT(*) AS n 
FROM BnL.Products p
LEFT JOIN BnL.Transactions t
	ON p.item = t.item AND class_desc IS NOT NULL 
GROUP BY item ORDER BY n DESC LIMIT 10

## Note, this could have been sum(amount)

### 6. What is the class of item 7313?

SELECT DISTINCT item, class_desc FROM BnL.Products WHERE item = 7313;

#You are doing great
 
---- PART TWO

### 1. How many customers have spent more than €731.1 in total on items labelled as 'Non sustainable'?

SELECT COUNT(*) FROM (
SELECT customer_id
FROM BnL.Transactions 
LEFT JOIN BnL.Products USING (item)
WHERE sustainability_id_desc = 'Non sustainable'
GROUP BY customer_id
HAVING SUM(amount) > 73130) t;

## Note: It should have been 731.3 in the question

#41

### 2. What is the proportion of items that are labelled as 'Recycled' that are never sold?

SELECT IF(t.customer_id IS NULL, "not sold", "sold"), COUNT(*)
FROM BnL.Products p LEFT JOIN BnL.Transactions t ON p.item = t.item AND sustainability_id_desc = 'Recycled'
GROUP BY 1;

#none are sold: the proportion is 1

### 3. What is the proportion of sales on Saturdays?

SELECT 
SUM(IF(WEEKDAY(purchase_date) = 5, 1, 0))/COUNT(*)
FROM BnL.Transactions 

#0.1582

### note, this could have been sum(amount)

SELECT "Total", COUNT(*) FROM BnL.Transactions 
UNION 
SELECT "Saturday", COUNT(*) FROM BnL.Transactions 
WHERE WEEKDAY(purchase_date) = 5;

#Total 620273
#Saturday 98100