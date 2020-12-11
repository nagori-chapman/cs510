# CS510 Krupa Nagori Midterm Project
Repository contains any files related to CS510 Computing for Scientists Course Midterm Project

The main folder contains the Readme, license, and R project script. 
The dataset folder contains the csv file utilized in the r script. 

The project is separated into 3 main folders: 
1. src -> contains original r script
2. dataset -> contains the dataset used for analysis
3. doc -> contains final RMarkdown file, feedback and changes, and license

The script is sectioned in a few portions:
1. Functions - these allow for the analysis to be replicated again in the future for other demographic data
2. Initial Analysis - This is the base graph comparing Age to Healthcare costs. 
3. Cross Validation - This is to test if the base model is valid
4. QQ Plot - This utilized the residuals from the cross validation step for additional checks.
