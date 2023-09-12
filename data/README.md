## Synthetic datasets
To save the dataset click on the link below (right click to open in new tab) and then click `File` -> `Save Page As...`.

All datsets below have been simulated from their original data resources using the R package [synthpop](https://cran.r-project.org/web/packages/synthpop/index.html). These synthetic datasets do not contain any real participants.

### [emot_reg_emot_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/emot_reg_emot_simulated.csv)
  * Emotional regulation in childhood and emotional symptoms scores, measured by the Strengths & Difficulties Questionnaires (SDQ),  across 5 time points between childhood and adolescence (ages 3 to 14).
  * This dataset contains ~13000 people, based upon data from the [Millennium Cohort Study (MCS)](https://cls.ucl.ac.uk/cls-studies/millennium-cohort-study/) participants.
  * You could use this example to look at the impact of poorer emotion regulation at baseline (t1) on later SDQ trajectories using the `er_t1_bin` variable or you could look to see how changes in emotion regulation from baseline to the second wave (t2) have a long term impact on SDQ trajetcories using `er_bin`. You could even adjust this analysis by accounting for sex in the analysis using the variable `female`.
  * There are 3 categorical emotion dysregulation variables one could use to split trajectories by: 
     *  `er_t1_bin` , `er_t2_bin`, `er_bin`
  * There are 2 continuous emotion dysregulation variables one could use to split trajectories by: 
     *  `er_total_t1` , `er_total_t2`

#### Variables as columns:
  * **Subject**
     * ID of simulated participant. 
     * Type: Character 
  * **sdq_t1, sdq_t2, sdq_t3, sdq_t4, sdq_t5**
     * Strengths & Difficulties Questionnaires Emotional Subscale (SDQ-E) scores from 0 to 10 at 5 time points. 
     * Type: Numeric
  * **age_t1, age_t2, age_t3, age_t4, age_t5**
     * Age at corresponding time points for the SDQ variable (years)
     * Type: Numeric
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
    * 1 = No ER problems at er_t1_bin & Yes ER problems at er_t2_bin
    * 2 = Yes ER problems at er_t1_bin & No ER problems at er_t2_bin 
    * 3 = Yes ER problems at er_t1_bin & Yes ER problems at er_t2_bin
   
### [ad_use_dep_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/ad_use_dep_simulated.csv)
  * Long term antidepressant adherence and subsequent measures of depressive symptoms, measured by the Short Mood and Feelings Questionnaire (SMFQ),  across 7 time points between in early adulthood (ages 23 to 30).
  * This dataset contains ~800 people, based upon data from the [Avon Longitudinal Study of Parents and Children (ALSPAC)](https://https://www.bristol.ac.uk/alspac/) participants.
  * You could use this dataset to look at long term depression trajetcories split by adherence to antidepressants. 
  * There is 1 antidepressant variable one could use to split trajectories by: 
     *  `meds`

#### Variables as columns:
  * **Subject**
     * ID of simulated participant. 
     * Type: Character 
  * **mfq_t1, mfq_t2, mfq_t3, mfq_t4, mfq_t5, mfq_t6, mfq_t7**
     * Short Mood and Feelings Questionnaire (SMFQ) scores from 0 to 26 at 7 time points. 
     * Type: Numeric
  * **age_t1, age_t2, age_t3, age_t4, age_t5, age_t6, age_t7**
     * Age at corresponding time points for the SDQ variable (years)
     * Type: Numeric
  * **meds**
    * Adherence to AD medication is coded as 1. Non-adherence to AD medication is coded as 0. 
    * Type: Categorical

### [fp_anx_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/fp_anx_simulated.csv)
  * Pre-pandemic financial problems and subsequent measures of anxiety symptoms across COVID-19, measured by the Generalised Anxiety Disorder 7 scale (GAD-7),  across 5 time points in early adulthood (ages 28 to 30).
  * This dataset contains ~4100 people, based upon data from the [Avon Longitudinal Study of Parents and Children (ALSPAC)](https://https://www.bristol.ac.uk/alspac/) participants.
  * You could use this dataset to explore the impact of pre-pandemic financial problems on later anxiety trajectories across COVID-19. You could use the fin_probs variable `fin_probs` to see how trajectories differ, and even see how these trajectories might look after adjusting for by pre-pandemic levels of anxiety `anx_pp`. 
  * There is 1 finacial problems variable one could use to split trajectories by: 
     *  `fin_probs`
  * Alternatively, there is 1 pre-pandemic anxiety variable one could use to split trajectories by: 
     *  `anx_pp`

#### Variables as columns:
  * **Subject**
     * ID of simulated participant. 
     * Type: Character 
  * **gad7_t2, gad7_t3, gad7_t4, gad7_t5, gad7_t6**
     * Generalised Anxiety Disorder 7 scale (GAD-7) scores from 0 to 21 at 5 time points. 
     * Type: Numeric
  * **age_t2, age_t3, age_t4, age_t5, age_t6**
     * Age at corresponding time points for the SDQ variable (years)
     * Type: Numeric
  * **fin_probs**
    * Pre-pandemic finacial problems is coded as 1. No pre-pandemic finacial problems is coded as 0. 
    * Type: Categorical
  * **anx_pp**
    * Pre-pandemic GAD-7 scores 
    * Type: Numeric
   
### [sleep_probs_mh_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/sleep_probs_mh_simulated.csv)
  * Sleep problems in childhood and subsequent measures of depressive symptoms, measured by the Short Mood and Feelings Questionnaire (SMFQ), and the Strengths & Difficulties Questionnaires Emotional Subscale (SDQ-E) across 4 time points in childhood and adolescence (ages 9 to 16).
  * This dataset contains ~8900 people, based upon data from the [Avon Longitudinal Study of Parents and Children (ALSPAC)](https://https://www.bristol.ac.uk/alspac/) participants.
  * You could use this dataset to examine the impact of childhood sleep problems on later mental health trajectories using either the MFQ or SDQ data as outcomes. For example you could see how MFQ trajectories differ by `sleep_bin` and even adjust for baseline SDQ scores `emot_t1` and vice versa.
  * There is 1 sleep problems variable one could use to split trajectories by: 
     *  `sleep_bin`
  * Alternatively, there are two mental health variables one could use to split trajectories by: 
     *  `mfq_t1` if looking at SDQ trajetcories
     *  `emot` if looking at SMFQ trajetcories

#### Variables as columns:
  * **Subject**
     * ID of simulated participant. 
     * Type: Character 
  * **mfq_t1, mfq_t2, mfq_t3, mfq_t4**
     * Short Mood and Feelings Questionnaire (SMFQ) scores from 0 to 26 at 4 time points. 
     * Type: Numeric
  * **emot_t1, emot_t2, emot_t3, emot_t4**
     * Strengths & Difficulties Questionnaires Emotional Subscale (SDQ-E) scores from 0 to 10 at 4 time points. 
     * Type: Numeric
  * **age_t1, age_t2, age_t3, age_t4**
     * Age at corresponding time points for the SMFQ/SDQ variable (years)
     * Type: Numeric
  * **sleep_bin**
    * Sleep problems in childhood are coded as 1. No sleep problems in childhood are coded as 0. 
    * Type: Categorical
   
### [sleep_probs_mh_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/sleep_probs_mh_simulated.csv)
  * Sleep problems in childhood and subsequent measures of depressive symptoms, measured by the Short Mood and Feelings Questionnaire (SMFQ), and the Strengths & Difficulties Questionnaires Emotional Subscale (SDQ-E) across 4 time points in childhood and adolescence (ages 9 to 16).
  * This dataset contains ~8900 people, based upon data from the [Avon Longitudinal Study of Parents and Children (ALSPAC)](https://https://www.bristol.ac.uk/alspac/) participants.
  * You could use this dataset to examine the impact of childhood sleep problems on later mental health trajectories using either the MFQ or SDQ data as outcomes. For example you could see how MFQ trajectories differ by `sleep_bin` and even adjust for baseline SDQ scores `emot_t1` and vice versa.
  * There is 1 sleep problems variable one could use to split trajectories by: 
     *  `sleep_bin`
  * Alternatively, there are two mental health variables one could use to split trajectories by: 
     *  `mfq_t1` if looking at SDQ trajetcories
     *  `emot` if looking at SMFQ trajetcories

#### Variables as columns:
  * **Subject**
     * ID of simulated participant. 
     * Type: Character 
  * **mfq_t1, mfq_t2, mfq_t3, mfq_t4**
     * Short Mood and Feelings Questionnaire (SMFQ) scores from 0 to 26 at 4 time points. 
     * Type: Numeric
  * **emot_t1, emot_t2, emot_t3, emot_t4**
     * Strengths & Difficulties Questionnaires Emotional Subscale (SDQ-E) scores from 0 to 10 at 4 time points. 
     * Type: Numeric
  * **age_t1, age_t2, age_t3, age_t4**
     * Age at corresponding time points for the SMFQ/SDQ variable (years)
     * Type: Numeric
  * **sleep_bin**
    * Sleep problems in childhood are coded as 1. No sleep problems in childhood are coded as 0. 
    * Type: Categorical
    
### [sleep_probs_mh_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/sleep_probs_mh_simulated.csv)
  * Sleep problems in childhood and subsequent measures of depressive symptoms, measured by the Short Mood and Feelings Questionnaire (SMFQ), and the Strengths & Difficulties Questionnaires Emotional Subscale (SDQ-E) across 4 time points between childhood and adolescence (ages 9 to 16).
  * This dataset contains ~8900 people, based upon data from the [Avon Longitudinal Study of Parents and Children (ALSPAC)](https://https://www.bristol.ac.uk/alspac/) participants.
  * You could use this dataset to examine the impact of childhood sleep problems on later mental health trajectories using either the MFQ or SDQ data as outcomes. For example you could see how MFQ trajectories differ by `sleep_bin` and even adjust for baseline SDQ scores `emot_t1` and vice versa.
  * There is 1 sleep problems variable one could use to split trajectories by: 
     *  `sleep_bin`
  * Alternatively, there are two mental health variables one could use to split trajectories by: 
     *  `mfq_t1` if looking at SDQ trajetcories
     *  `emot` if looking at SMFQ trajetcories

#### Variables as columns:
  * **Subject**
     * ID of simulated participant. 
     * Type: Character 
  * **mfq_t1, mfq_t2, mfq_t3, mfq_t4**
     * Short Mood and Feelings Questionnaire (SMFQ) scores from 0 to 26 at 4 time points. 
     * Type: Numeric
  * **emot_t1, emot_t2, emot_t3, emot_t4**
     * Strengths & Difficulties Questionnaires Emotional Subscale (SDQ-E) scores from 0 to 10 at 4 time points. 
     * Type: Numeric
  * **age_t1, age_t2, age_t3, age_t4**
     * Age at corresponding time points for the SMFQ/SDQ variable (years)
     * Type: Numeric
  * **sleep_bin**
    * Sleep problems in childhood are coded as 1. No sleep problems in childhood are coded as 0. 
    * Type: Categorical

### [height_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/height_simulated.csv)
  * Sex and genetic differences in height trajetcories, measured across 8 time points across childhood and adolescence (ages 7 to 18).
  * This dataset contains ~15000 people, based upon data from the [Avon Longitudinal Study of Parents and Children (ALSPAC)](https://https://www.bristol.ac.uk/alspac/) participants.
  * You could use this dataset to examine sex differences in height trajetcories using the `female` for sex. Alternatively, you could explore how higher or lower polygenic risk score for height `hei_k_5e08_std` are associated with different height trajectories, adjusting for sex `female`.
  * There is 1 sex variable one could use to split trajectories by: 
     *  `female`
  * Alternatively, there is one geneic risk variable one could use to split trajectories by: 
     *  `hei_k_5e08_std`

#### Variables as columns:
  * **Subject**
     * ID of simulated participant. 
     * Type: Character 
  * **height_t1, height_t2, height_t3, height_t4, height_t5, height_t6, height_t7, height_t8**
     * Height at each occasions (measured in cm)
     * Type: Numeric
  * **age_t1, age_t2, age_t3, age_t4, age_t5, age_t6, age_t7, age_t8**
     * Age at corresponding time points for the height variable (years)
     * Type: Numeric
  * **Female**
    * female is coded as 1. Male is coded as 0. 
    * Type: Categorical
  * **hei_k_5e08_std**
    * Polylgenic risk score for height where higher scores = greater genetic liability to height and lower scores = lower genetic liability for height. 
    * Type: Numeric
