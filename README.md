# ADIA-Study-3

## Analysis 1: Discrimination for any reason (n=6281)
In the folder "Discrimination for any reason", you will find the data, programs, and output files for the analysis using the expanded sample who reported discrimination for any reason. 
<b>Please note that all files of this analysis are saved in the main folder "Discrimination for any reason" that has three subfolders: </b>

#### <i>Discrimination for any reason/analyses</i> 
In this folder, you will find R programs for each outcome. The programs were executed in the numbered order. The tests for contrast between subgroups (4.subgroup_analysis.R) and constrast between Study 3 sample and remaining sample (5.contrast in vs out study.R) were conducted for the outcomes that had significant nodes in the regression trees. These contrast tests were sometimes run separately for classical and causal trees if significant ACE and OCS were found in both trees. <b>Please note: each time 0.preprocess_ALL is run it randomly selects new training and testing samples. Therefore, to replicate analysis, you must use data_for_regTree_08132023.rds file as an input data for 1.rtree.R programs. This data can be found in Discrimination for any reason/Data folder. </b>
<ul>
  <li>0.preprocess_ALL will prepare the analytic dataset by defining training and testing datasets.</li>
  <li>1.rtree.R will run the regression tree search. Further information on the types of implemented regression trees can be found <a href="https://github.com/ICF-Analytics/ADIA_S13/files/9456975/Regression.tree.analysis.implementation.pdf">here</a>.</li>
  <li>2.inference.R will run the inference testing to confirm results.</li>
  <li>3.additional tests.R will run the group contrasts.</li>
  <li>4.subgroup_analysis.R will run the tests to compare the effect between demogrphic subgroups. The sugroup tests are broken into three R scripts:
     <li style="margin-left:6em"> 4.0 subgroup_analysis.R tests the contrast between racial minority (non-White or Hispanic) and non-Hispanic White subgroups.</li>
     <li style="margin-left:6em"> 4.0.b subgroup_analysis.R tests the contrast between Black and non-Hispanic White subgroups.</li>
     <li style="margin-left:6em"> 4.0.h subgroup_analysis.R tests the contrast between Hispanic andnon-Hispanic White subgroups.</li>
  </li>
  <li>5.contrast in vs out study.R will run the tests to compare the strength of effects between the Study 3 sample and the remaining sample.</li>
</ul> 
Subfolder names for each outcome are as follow: 
<ul>
  <li>accinju: Accidents or Injury </li>
  <li>anxiety: Anxiety Symptom Severity </li>
  <li>depress: Depressive Symptom Severity </li>
  <li>drinkdy: Alcohol Consumption </li>
  <li>evrvicr: Violent Crime Victimization </li>
  <li>preshlth: General Health </li>
</ul> 

#### <i>Discrimination for any reason/output</i> 
You will find the analysis outputs (regression trees, statistical test results, etc.) for each outcome. 

#### <i>Discrimination for any reason/data</i>
You will find the datasets that can be used as input data for the R programs. 

<br>

## Analysis 2: Discrimination for race, ethnicity, and sexual orientation (n=841)
In the three main folders below, you will find the data, programs, and output files for the analysis using the original sample who reported discrimination for specific reasons (race, ethnicity, and sexual orientation).

#### <i>analyses</i> 
In the folder, you will find R programs for each outcome. The programs should be run in the numbered order: 
<ul>
  <li>0.preprocess_ALL will prepare the analytic dataset by defining training and testing datasets.</li>
  <li>1.rtree.R will run the regression tree search. Further information on the types of implemented regression trees can be found <a href="https://github.com/ICF-Analytics/ADIA_S13/files/9456975/Regression.tree.analysis.implementation.pdf">here</a>.</li>
  <li>2.inference.R will run the inference testing to confirm results.</li>
  <li>3.additional tests.R will run the group contrasts.</li>
</ul> 

#### <i>output</i> 
You will find the analysis outputs (regression trees, statistical test results, etc.) for each outcome. 

#### <i>data</i>
You will find the datasets that can be used as input data for the R programs. 
