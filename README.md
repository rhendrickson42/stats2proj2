### stats2proj2

# Osteoporosis in Women  

### Data: (glow500.csv) - data.frame with 500 rows and 15 variables:  

```
Variable Name Type   #Unique  

     SUB_ID  integer  500 - Identification Code (1 - n)  
    SITE_ID  integer    6 - Study Site (1 - 6)  
     PHY_ID  integer  127 - Physician ID code (128 unique codes)  
 PRIORFRAC*   factor    2 - History of Prior Fracture (1: No, 2: Yes)   
        AGE  integer   36 - Age at Enrollment (Years)  
     WEIGHT  numeric  128 - Weight at enrollment (Kilograms)  
     HEIGHT  integer   34 - Height at enrollment (Centimeters)  
        BMI  numeric  409 - Body Mass Index (Kg/m^2)  
   PREMENO*   factor    2 - Menopause before age 45 (1: No, 2: Yes)  
   MOMFRAC*   factor    2 - Mother had hip fracture (1: No, 2: Yes)  
 ARMASSIST*   factor    2 - Arms are needed to stand from a chair (1: No, 2: Yes)  
     SMOKE*   factor    2 - Former or current smoker (1: No, 2: Yes)  
  RATERISK*   factor    3 - Self-reported risk of fracture (1: Less than others of the same age, 2: Same as others of  
  FRACSCORE  integer   12 - Fracture Risk Score (Composite Risk Score)  
  FRACTURE*   factor    2 - Any fracture in first year (1: No, 2: Yes)  

```

## Project Files  

 - [Data](Data) - project data directory  
 
 Datasets:  
 
  - [dataset](data/glow500.csv) <- Glow Dataset  
  - [Balanced dataset](data/glow500_smoted.csv) <- SMOTEd Glow Dataset  
  
 Analysis:  
 
  - [SMOTE dataset](presentation/glow_smartEDA_SMOTE.md)   
  - [test](presentation/test_balanced_data.md)   
  
 