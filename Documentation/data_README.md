## Synthetic datasets
To save the dataset click on the link below (right click to open in new tab) and then click `File` -> `Save Page As...`.

### [emot_reg_emot_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/emot_reg_emot_simulated.csv)
  * Emotional regulation in childhood and emotional symptoms scores, measured by the Strengths & Difficulties Questionnaires (SDQ),  across 5 time points between childhood and adolescence (ages 3 to 14).
  * This dataset contains ~13000 people, who have been simulated using the R package [synthpop](https://cran.r-project.org/web/packages/synthpop/index.html) from an original dataset [Millennium Cohort Study (MCS)](https://cls.ucl.ac.uk/cls-studies/millennium-cohort-study/) participants. This synthetic dataset does not contain any real participants.
  * There are 3 emotion dysregulation variables one could use to split trajectories by: 
     *  `er_t1_bin` , `er_t2_bin`, `er_bin` 

#### Variables as columns:
  * **Subject**
     * ID of simulated participant. 
     * Type: Character 
  * **sdq_t1, sdq_t2, sdq_t3, sdq_t4, sdq_t5**
     * Strengths & Difficulties Questionnaires Emotional Subscale (SDQ-E) scores from 0 to 10 at 5 time points. 
     * Type: Numeric
  * **age_t1, age_t2, age_t3, age_t4, age_t5**
     * Age at corresponding time points for the SDQ variable (years)
     * Type: Cateogorical
  * **Female**
    * Female is coded as 1. Male is coded as 0. 
    * Type: Categorical
  * **er_total_t1**
    * Emotional regulation scores from the emotional dysregulation component of the Child Social Behaviour Questionnaire at age 3 years.
    * Type: Numeric
    * Range: 0 to 10
  * **er_total_t2**
     * Emotional regulation scores from the emotional dysregulation component of the Child Social Behaviour Questionnaire at age 5 years.
    * Type: Numeric
    * Range: 0 to 10
  * **er_t1_bin**
    * When er_total_t1 is between 0 to 6 er_t1_bin is 0
    * When er_total_t1 is between 7 to 10 er_t1_bin is 1
  * **er_t2_bin**
    * When er_total_t2 is between 0 to 6 er_t2_bin is 0
    * When er_total_t2 is between 7 to 10 er_t2_bin is 1
  * **er_bin**
    * 0 = No emotional regulation (ER) problems at er_t1_bin & No ER problems at er_t2_bin
    * 1 = Yes ER problems at er_t1_bin & No ER problems at er_t2_bin
    * 2 = No ER problems at er_t1_bin & Yes ER problems at er_t2_bin 
    * 3 = Yes ER problems at er_t1_bin & Yes ER problems at er_t2_bin
