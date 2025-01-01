
-- Cleaning Data Tables (Removing unneccessary/overlapping columns)

SELECT AppID,Name,Release_date,Developers,Publishers,Categories,Genres,DLC_count,Windows,Mac,Linux,Metacritic_score,Metacritic_url,About_the_game,Header_image,Website,Support_url,Support_email,Notes,Achievements,Recommendations
INTO steam_games_clean
FROM portfolio.dbo.steam_games

SELECT steam_appid, required_age, controller_support, type
INTO steam_app_clean
FROM portfolio.dbo.steam_app

SELECT appid,owners,price,initialprice,discount,languages,ccu,positive,negative,tags
INTO steam_spy_clean
FROM portfolio.dbo.steam_spy

-- Renaming 'appid' columns to avoid merging errors due to same column names

EXEC sp_rename 'steam_spy_clean.appid','app_id','COLUMN';

-- Joining the 3 tables into one consolidated table (Joining based on the unique App IDs)

SELECT *
INTO steam_data
FROM steam_games_clean
LEFT JOIN steam_app_clean
ON steam_games_clean.AppID = steam_app_clean.steam_appid
LEFT JOIN steam_spy_clean
ON steam_games_clean.AppID = steam_spy_clean.app_id

-- Dropping extra App ID columns from the joining

ALTER TABLE steam_data
DROP COLUMN steam_appid,app_id

-- Final consolidated data table to be used

SELECT * FROM steam_data