# FROM HITTING THE RIGHT NOTES TO HITTING THE RIGHT PAY: 
## Analyzing the Impact of Different Remuneration Models on the Equality of Payout

This repository is meant to store the code that is written for my Marketing Analytics master thesis. In order to run the code, raw data is needed. If you would like to receive this raw data, or read my complete thesis, please send me an email: j.n.g.habets@tilburguniversity.edu.

### Contents
In this thesis, the effects of different remuneration models on the equality of payout is analyzed. The code that is written downloads raw data, transforms this data and subsequently runs three remuneration models on its contents. Consequently, analyses are performed on the revenue outcomes. Descriptives are created in separate files as well. 

### Executive information
To be able to run these files, two coding programs need to be installed:
- Rstudio
- Jupyter Notebook

In case packages do not load, they have to be installed prior to the execution of the code. 

First, the code in the data_extraction should be ran. Next, the code in the data_prep folder should ran. Last, the code in theanalysis folder should be ran. 
The code should be ran according to the numeric ordering in front of the coding files. So, first 1., second 2., third, 3., etc. This system has been applied in order to create an environment as clear as possible 

It is suggested to locally create two folders, named gen and data. In data, the raw data is stored and in gen, you create two subfolders: output and temp. Temp is used for temporary csv files created in the code. Output can be used to store output, such as the graphs created.
