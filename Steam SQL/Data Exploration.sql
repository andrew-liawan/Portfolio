
SELECT *
FROM steam_data
ORDER BY AppID

-- Total Game Count in Database = 93,185

SELECT COUNT (AppID)
FROM steam_data

-- Total Developer Count in Database = 54,229

SELECT COUNT (DISTINCT Developers)
FROM steam_data
WHERE Developers IS NOT NULL

-- Game Count by Developers

SELECT Developers, COUNT (AppID)
FROM steam_data
WHERE Developers IS NOT NULL
GROUP BY Developers
ORDER BY COUNT(AppID) DESC

-- Game Count by Genres

WITH tmp(AppID, Name, Genre_idv, Genres) AS
(
    SELECT
        AppID,
        Name,
        LEFT(Genres, CHARINDEX(',', Genres + ',') - 1),
        STUFF(Genres, 1, CHARINDEX(',', Genres + ','), '')
    FROM steam_data
	WHERE Genres IS NOT NULL
    UNION all

    SELECT
        AppID,
        Name,
        LEFT(Genres, CHARINDEX(',', Genres + ',') - 1),
        STUFF(Genres, 1, CHARINDEX(',', Genres + ','), '')
    FROM tmp
    WHERE
        Genres > ''
)

SELECT Genre_idv, COUNT(AppID)
FROM tmp
GROUP BY Genre_idv
ORDER BY COUNT(AppID) DESC

-- Game Count by Categories

WITH tmp(AppID, Name, Categories_idv, Categories) AS
(
    SELECT
        AppID,
        Name,
        LEFT(Categories, CHARINDEX(',', Categories + ',') - 1),
        STUFF(Categories, 1, CHARINDEX(',', Categories + ','), '')
    FROM steam_data
	WHERE Categories IS NOT NULL
    UNION all

    SELECT
        AppID,
        Name,
        LEFT(Categories, CHARINDEX(',', Categories + ',') - 1),
        STUFF(Categories, 1, CHARINDEX(',', Categories + ','), '')
    FROM tmp
    WHERE
        Categories > ''
)

SELECT Categories_idv, COUNT(AppID)
FROM tmp
GROUP BY Categories_idv
ORDER BY COUNT(AppID) DESC

-- Game Count by Available Language

WITH tmp(AppID, Name, languages_idv, languages) AS
(
    SELECT
        AppID,
        Name,
        LEFT(languages, CHARINDEX(',', languages + ',') - 1),
        STUFF(languages, 1, CHARINDEX(',', languages + ','), '')
    FROM steam_data
	WHERE languages IS NOT NULL
    UNION all

    SELECT
        AppID,
        Name,
        LEFT(languages, CHARINDEX(',', languages + ',') - 1),
        STUFF(languages, 1, CHARINDEX(',', languages + ','), '')
    FROM tmp
    WHERE
        languages > ''
)

SELECT languages_idv, COUNT(AppID)
FROM tmp
GROUP BY languages_idv
ORDER BY COUNT(AppID) DESC

-- Top 50 Games Based On Peak CCU (Concurrent Users)

SELECT TOP 50 Name, owners, ccu
FROM steam_data
ORDER BY ccu DESC

-- Top 50 Games Released on Steam in the 2020s Based On Peak CCU (Concurrent Users)

SELECT TOP 50 Name, Release_date, owners, ccu
FROM steam_data
WHERE Release_date >= '2020-01-01'
ORDER BY ccu DESC

-- Top 50 Games Based on Total Owners & Peak CCU (Concurrent Users)

SELECT TOP 50
	Name,
	Release_date,
	type,
	CAST(REPLACE(PARSENAME(REPLACE(owners,' .. ','.'),2),',','') AS int) AS owners_min, 
	CAST(REPLACE(PARSENAME(REPLACE(owners,' .. ','.'),1),',','') AS int) AS owners_max,
	ccu,
	CAST(price AS float)/100 AS price_USD
FROM steam_data
WHERE Release_date IS NOT NULL
ORDER BY owners_max DESC, ccu DESC

-- Top 50 Action Games Based on Total Owners & Peak CCU (Concurrent Users)

SELECT TOP 50
	Name,
	Release_date,
	Genres,
	CAST(REPLACE(PARSENAME(REPLACE(owners,' .. ','.'),2),',','') AS int) AS owners_min, 
	CAST(REPLACE(PARSENAME(REPLACE(owners,' .. ','.'),1),',','') AS int) AS owners_max,
	ccu,
	CAST(price AS float)/100 AS price_USD
FROM steam_data
WHERE 
	(Release_date IS NOT NULL) AND
	(Genres like '%action%')
ORDER BY owners_max DESC, ccu DESC

-- Top 10 Multiplayer Racing Games Based on Total Owners & Peak CCU (Concurrent Users)

SELECT Top 10
	Name,
	Release_date,
	Genres,
	Categories,
	CAST(REPLACE(PARSENAME(REPLACE(owners,' .. ','.'),2),',','') AS int) AS owners_min, 
	CAST(REPLACE(PARSENAME(REPLACE(owners,' .. ','.'),1),',','') AS int) AS owners_max,
	ccu,
	CAST(price AS float)/100 AS price_USD
FROM steam_data
WHERE 
	(Release_date IS NOT NULL) AND
	(Categories like '%Multi-player%') AND
	(Genres like '%Racing%')
ORDER BY owners_max DESC, ccu DESC

-- Average Price of Each Genre

WITH tmp(AppID, price, Genre_idv, Genres) AS
(
    SELECT
        AppID,
        price,
        LEFT(Genres, CHARINDEX(',', Genres + ',') - 1),
        STUFF(Genres, 1, CHARINDEX(',', Genres + ','), '')
    FROM steam_data
	WHERE Genres IS NOT NULL
    UNION all

    SELECT
        AppID,
        price,
        LEFT(Genres, CHARINDEX(',', Genres + ',') - 1),
        STUFF(Genres, 1, CHARINDEX(',', Genres + ','), '')
    FROM tmp
    WHERE
        Genres > ''
)

SELECT Genre_idv, CAST(AVG(price) AS float)/100 AS price_USD
FROM tmp
WHERE price IS NOT NULL
GROUP BY Genre_idv
ORDER BY price_USD DESC

-- Average Number of Owners of Each Genre

WITH tmp(AppID, owners, Genre_idv, Genres) AS
(
    SELECT
        AppID,
        owners,
        LEFT(Genres, CHARINDEX(',', Genres + ',') - 1),
        STUFF(Genres, 1, CHARINDEX(',', Genres + ','), '')
    FROM steam_data
	WHERE (Genres IS NOT NULL) AND (owners IS NOT NULL)
    UNION all

    SELECT
        AppID,
        owners,
        LEFT(Genres, CHARINDEX(',', Genres + ',') - 1),
        STUFF(Genres, 1, CHARINDEX(',', Genres + ','), '')
    FROM tmp
    WHERE
        Genres > ''
)
SELECT 
	Genre_idv,
	AVG(CAST(REPLACE(PARSENAME(REPLACE(owners,' .. ','.'),1),',','') AS bigint)) AS owners_avg
FROM tmp
GROUP BY Genre_idv
HAVING COUNT(AppID)>100
ORDER BY owners_avg DESC

-- Top 100 Most Positively Reviewed Games

SELECT TOP 100 Name, positive, negative, ROUND(CAST(positive AS float)/CAST(negative AS float),2) AS ratio
FROM steam_data
WHERE (positive IS NOT NULL) AND (negative != 0)
ORDER BY ratio DESC

-- Most Negatively Reviewed Games (Games with More Negative Reviews Than Positive)

SELECT TOP 100 Name, positive, negative, ROUND(CAST(positive AS float)/CAST(negative AS float),2) AS ratio
FROM steam_data
WHERE (positive IS NOT NULL) AND (negative != 0) AND (negative>positive)
ORDER BY ratio ASC

-- Counter-Strike Games

SELECT 
	Name, 
	Release_date,
	owners, 
	ccu, 
	CAST(price AS float)/100 AS price_USD, 
	positive,
	negative,
	ROUND(CAST(positive AS float)/CAST(negative AS float),2) AS ratio
FROM steam_data
WHERE (Name like '%Counter-Strike%') AND (ccu IS NOT NULL)
ORDER BY Release_date