**Fine-scale Analysis of Bowhead Whale Songs**

MATLAB and R code for 'Fine-scale quantitative analysis of bowhead whale (Balaena mysticetus) song shows varying stability of song types'. MATLAB code written by Luke Rendell and R code written by Seth Harris.

**GetStartAndEndFrequency.m** extracts the start and end frequency of each song unit. Input needs to be Raven selection table (txt format) with the columns BeginTime_s_, .Time5__s_, EndTime_s_, Time95__s_, LowFreq_Hz_, and HighFreq_Hz_.

**Bowhead_Whale_Song_Analysis.R** includes the random forest analysis, principal component analysis, Levenshtein Similarity Indexes (LSIs) and Dice's Similarity Indexes (DSIs) ran during this study. In order to run LSIs and DSIs you will need three versions of the same file: 1. a csv with all song data in one column with no header row, for the compare_songs function; 2. a csv with all song data spread across multiple columns with no header row, for the dice function; and 3. a csv with all song data spread across multiple columns with a header row, for the plotting functions. See csv files in SongStrings for this data and the three formatting variants. For DSIs to work you will also need the dice function. See Materials and Methods for link to dice function.
