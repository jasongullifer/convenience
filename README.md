convenience
============

This package contains convenience functions I use regularly for processing data related to cognitive psychology and the language sciences. The most notable function is sem(). It will take as inputs raw data, an id variable (e.g., subject or item), and a set of columns on which to split the data (e.g., conditions). It then returns grand mean data over condition, with SEM computed using the number of unique IDs (e.g., subjects or items) as the sample size. Note, there's  an argment to return the interim dataset (i.e., subject-level or item-level) as well as the grand mean dataset.

Most other SEM functions that I have encountered implicitly assume the N as the number of rows in the dataset. This if fine if you are already working with subject aggregate data, but is likely not what you want with raw, trial-level data. 
