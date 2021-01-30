## System Usability Scale

Measuring usability is not a simple task, however, some scales help in quantifying usability. And usability testing from real users to evaluate some system, software, or product can show important details about the interface weakness being tested.

The System Usability Scale (SUS) can help in this important task. This scale has 10 questions, and for each of them, there are options from 1 to 5, where 1 means completely disagree and 5 means completely agree.

The final score ranges from zero to 100 points. And to perform this calculation, the value given by the respondent in the odd questions is subtracted in one unit (the given value - 1); for the even-numbered questions, it will be 5 minus the value given by the respondent (5 - the value given). Finally, sum the values of the 10 calculated questions and multiply by 2.5.

The CSV file contains the first three questions to understand the respondent's profile and then the 10 questions on the SUS scale.

### How it works
We have a file in R language named `data-analysis-usability-v01.R` that load the CSV file named `usability-db-en.csv`, then it calculates a histogram for the 3 first questions, creates a boxplot to understand the distribution of the 10 questions, calculates the SUS score of all interface evaluators, and processes the Kendall coefficient of concordance among evaluators.

### Author
- João Paulo Folador
- Adriano de Oliveira Andrade (:star: advisor)

### Reference
Bangor, A., Kortum, P., & Miller, J. (2009). Determining what individual SUS scores mean: Adding an adjective rating scale. Journal of usability studies, 4(3), 114-123.
Available in: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.177.1240&rep=rep1&type=pdf

