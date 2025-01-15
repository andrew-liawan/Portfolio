
-- Create consolidated data table from the 5 separate data tables

SELECT * INTO rollingsales_ny FROM rollingsales_manhattan

UNION

SELECT * FROM rollingsales_brooklyn

UNION

SELECT * FROM rollingsales_bronx

UNION

SELECT * FROM rollingsales_queens

UNION

SELECT * FROM rollingsales_statenisland

-- Add column of abbreviated borough name into borough_ref

ALTER TABLE borough_ref
ADD BOROUGH_ABV nvarchar(50)
UPDATE borough_ref
SET BOROUGH_ABV = 'MN'
WHERE NO = 1
UPDATE borough_ref
SET BOROUGH_ABV = 'BX'
WHERE NO = 2
UPDATE borough_ref
SET BOROUGH_ABV = 'BK'
WHERE NO = 3
UPDATE borough_ref
SET BOROUGH_ABV = 'QN'
WHERE NO = 4
UPDATE borough_ref
SET BOROUGH_ABV = 'SI'
WHERE NO = 5

-- Merge rollingsales_ny with borough_ref

EXEC sp_rename 'borough_ref.BOROUGH','BOROUGH_NAME','COLUMN';
EXEC sp_rename 'pluto_24v4.borough','borough_nm','COLUMN';

SELECT * INTO rollingsales_ny_wref
FROM rollingsales_ny
LEFT JOIN borough_ref ON rollingsales_ny.BOROUGH = borough_ref.NO

ALTER TABLE rollingsales_ny_wref
DROP COLUMN BOROUGH

ALTER TABLE rollingsales_ny_wref
ADD merge_ref AS (CONCAT(BOROUGH_ABV,'-',BLOCK,'-',LOT))

-- Merge rollingsales_ny with pluto_24v4 = Consolidated Data Table (rollingsales_consolidated)

EXEC sp_rename 'pluto_24v4.block','block_pl','COLUMN';
EXEC sp_rename 'pluto_24v4.lot','lot_pl','COLUMN';

ALTER TABLE pluto_24v4
ADD merge_ref_pluto AS (CONCAT(borough_nm,'-',block_pl,'-',lot_pl))

EXEC sp_rename 'pluto_24v4.address','address_pl','COLUMN';

SELECT * INTO rollingsales_consolidated
FROM rollingsales_ny_wref
LEFT JOIN pluto_24v4 ON rollingsales_ny_wref.merge_ref = pluto_24v4.merge_ref_pluto

-- Remove columns to avoid comma errors in csv export

ALTER TABLE rollingsales_consolidated
DROP COLUMN ownername
ALTER TABLE rollingsales_consolidated
DROP COLUMN ADDRESS
ALTER TABLE rollingsales_consolidated
DROP COLUMN APARTMENT_NUMBER

-- Export Consolidated Data Table (rollingsales_consolidated) into csv for further processing/EDA in Python