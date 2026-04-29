**Fine-scale Analysis of Bowhead Whale Songs**

---
MATLAB and R code for 'Fine-scale quantitative analysis of bowhead whale (Balaena mysticetus) song shows varying stability of song types'. MATLAB code written by Luke Rendell and R code written by Seth Harris.

---
**GetStartAndEndFrequency.m** extracts the start and end frequency of each song unit. Input needs to be Raven selection table (txt format) with the columns BeginTime_s_, .Time5__s_, EndTime_s_, Time95__s_, LowFreq_Hz_, and HighFreq_Hz_.

---
**Bowhead_Whale_Song_Analysis.R** includes the random forest analysis, principal component analysis, Levenshtein Similarity Indexes (LSIs) and Dice's Similarity Indexes (DSIs) ran during this study. In order to run LSIs and DSIs you will need three versions of the same file (11 files, stored in **SongString** folder): 
**1.** a csv with all song data in one column with no header row, for the compare_songs function (lsi_rendition_2011.csv, lsi_rendition_2014.csv, lsi_rendition_2020.csv, lsi_rendition_2020tv.csv, lsi_rendition_all.csv).
**2.** a csv with all song data spread across multiple columns with no header row, for the dice function (lsi_rendition_2011_multicolumn.csv, lsi_rendition_2014_multicolumn.csv, lsi_rendition_2020_multicolumn.csv, lsi_rendition_all_multicolumn.csv).
**3.** a csv with all song data spread across multiple columns with a header row, for the plotting functions (lsi_rendition_2011_multicolumn_wh.csv, lsi_rendition_2014_multicolumn_wh.csv, lsi_rendition_2020_multicolumn_wh.csv, lsi_rendition_2020tv_multicolumn_wh.csv). 

For DSIs to work you will also need the dice function installed. See Materials and Methods for link to dice function.

---
