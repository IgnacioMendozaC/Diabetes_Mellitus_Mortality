# Effect of age, period, and birth cohort on diabetes mellitus mortality rate in Colombia, 1983-2022. An analytical cross-sectional study. 

This repository allows you to reproduce the results of the research "Effect of Age, Period, and Birth Cohort on Diabetes Mellitus Mortality Rates in Colombia, 1983-2022: An Analytical Cross-Sectional Study".

## Description of files

**Files included in the project**

1. [Modelo cod](Final.R)
   
2. Databases for general analysis 

   2.1 [Main data base](bd_long_dm.xlsx)

   This database includes period groups, age groups, cohort groups, diabetes death counts, mid-period population, and the crude diabetes mortality rate per 100,000 inhabitants. It also includes      the age group, period, and cohort categories required for some graphs.

   2.2 [Lexis table database frequency (case count)](TABLA_LEXIS_FRECUENCIA_DM.xlsx)

   This database includes the count of deaths attributed to diabetes mellitus in Lexis table format according to age groups (rows), period groups (columns), and birth cohort groups (diagonals).

   2.3 [Lexis table database for general population](TABLA_LEXIS_POBLACION_GENERAL.xlsx)

   This database includes population counts based on projections and retroprojections from the National Administrative Department of Statistics of Colombia, in Lexis table format according to       age groups (rows), period groups (columns), and birth cohort groups (diagonals).

   2.4 [Lexis table database for diabetes mortality rate](TABLA_LEXIS_TASA_DM.xlsx)

   This database includes the crude mortality rate from diabetes mellitus per 100,000 inhabitants, in Lexis table format according to age groups (rows), period groups (columns) and birth cohort      groups (diagonals).

3. Databases for analysis by male sex

   3.1 [Main data base](bd_long_dm_h.xlsx)

   This database includes period groups, age groups, cohort groups, male diabetes death counts, male mid-period population, and the crude male diabetes mortality rate per 100,000 inhabitants. It 
   also includes the age group, period, and cohort categories required for some graphs.
 
   3.2 [Lexis table database frequency (case count)](TABLA_LEXIS_FRECUENCIA_DM_H.xlsx)

   This database includes the count of deaths attributed to diabetes mellitus for males in Lexis table format according to age groups (rows), period groups (columns) and birth 
   cohort groups (diagonals).

   3.3 [Lexis table database for general population](TABLA_LEXIS_POBLACION_DM_H.xlsx)

   This database includes the male count based on projections and retroprojections from the National Administrative Department of Statistics of Colombia, in Lexis table format according to age      groups (rows), period groups (columns) and birth cohort groups (diagonals).

   3.4[Lexis table database for diabetes mortality rate](TABLA_LEXIS_TASA_DM_H.xlsx)

   This database includes the mortality rate of diabetes mellitus for males per 100,000 inhabitants, in Lexis table format according to age groups (rows), period groups (columns) and birth          cohort groups (diagonals).

4. Databases for analysis by female sex

   4.1 [Main data base](bd_long_dm_m.xlsx)

   This database includes period groups, age groups, cohort groups, female diabetes death counts, female mid-period population, and the crude female diabetes mortality rate per 100,000       
   inhabitants. It also includes the age group, period, and cohort categories required for some graphs.

   4.2 [Lexis table database frequency (case count)](TABLA_LEXIS_FRECUENCIA_DM_M.xlsx)

   This database includes the count of deaths attributed to diabetes mellitus for females in Lexis table format according to age groups (rows), period groups (columns) and birth 
   cohort groups (diagonals).

   4.3 [Lexis table database for general population](TABLA_LEXIS_POBLACION_DM_M.xlsx)

   This database includes the female count based on projections and retroprojections from the National Administrative Department of Statistics of Colombia, in Lexis table format according to age    groups (rows), period groups (columns) and birth cohort groups (diagonals).

   4.4 [Lexis table database for diabetes mortality rate](TABLA_LEXIS_TASA_DM_M.xlsx)

   This database includes the mortality rate of diabetes mellitus for female per 100,000 inhabitants, in Lexis table format according to age groups (rows), period groups (columns) and birth          cohort groups (diagonals).



## Runnig the code

**Note:** This project was made in R in its Rstudio interface.

To reproduce the model, follow these steps:

1. Download each of the files included in the [project](https://github.com/IgnacioMendozaC/Diabetes_Mellitus_Mortality).

2. Create a project in Rstudio and put the files you downloaded earlier there.

3. From Rstudio opens the file [Final.R](Final.R)

4. Install each of the packages. If you have any problem with the package **APCG1**, you must install it manually.

   4.1 Download the package ‘APCI’ version 1.0.8.
   [‘APCI’](https://www.math.uh.edu/~fuw/APCG1_1.0.tar.gz)

   4.2 Put the file in the same folder where the Rstudio project is.
   **Note:** do not unzip the file

   4.3 `install.packages("APCG1_1.0.tar.gz", repos = NULL, type = "source")`

5. Continue installing the packages and run the rest of the code.

## Contact

Juan Pablo Pérez Bedoya
* :email: juan.perez42@udea.edu.co

Oscar Ignacio Mendoza Cardozo
* 📧 oscar.mendoza1@udea.edu.co

Carlos Andres Perez 
* 📧 caaperezag@unal.edu.co

## Acknowledgments

Dr. Paula Andrea Díaz Valencia

**National Faculty of Public Health, Epidemiology Group, University of Antioquia, Medellín, Colombia**

Dr. Noël Christopher Barengo

**Herbert Wertheim College of Medicine and Robert Stempel College of Public Health & Social Work, Florida International University, Miami, FL, United States**
