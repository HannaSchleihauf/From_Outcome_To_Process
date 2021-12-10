# From_Outcome_To_Process

## Study Overview
Someone is rational in their thinking to the extent that they follow a rational procedure when determining what to believe. So whether someone is rational cannot be determined so much by whether they hold true or false beliefs (outcome-based rationality), but by how they arrived at these beliefs (procedure-based rationality). In this study, we want to answer the question to what extent 4-5-year-old children, 6-7-year-old children, 8-9-year-old children, and adults from China and the United States consider the procedure and the outcome in evaluating the rationality of an agent?

In a picture book story, participants will be introduced to two characters whose pet ran away. They are trying to find the pet by using either rational (e.g. looking for the pet's traces) or irrational (e.g. using a spinning wheel) procedures that lead them to either the right (pointing at the location where the pet is hiding) or the wrong conclusion (pointing at the location where the pet is not hiding). 

More precisely, the participants will see three conditions:

In an outcome condition, both characters are using an irrational procedure to find out where their pet is hiding, but one chooses the correct location, the other the wrong location.

In a process condition, one of the characters is using a rational and the other is using an irrational procedure, while both choose wrong locations. 

In a process-vs.-outcome condition, one character is using an irrational procedure and point to the right location, the other character is using a rational procedure and point to the wrong location.

## Data Set and Meta Data

Data: PRR_data.csv

Meta Data: 

Descriptive and Predictor Variables:

  culture: china, usa
  id: idenificator 
  sub.id: identificator that comobies id and culture
  test.date: date of testing session
  gender: male, female
  counterbalancing.order: indicates with which condition participants started (2 counterbalancing orders)
  trial.nr: trial number from 1 to 6
  condition: process.only, outcome.only or process-vs.outcome condition 
  story: indicates which story was presented, there were two stories per condition
  
  age: exact: age with decimals
  age.group: age categories of 4-year-olds, 5-year-olds, 6-year-olds, 7-year-olds, 8-year-olds, 9-year-olds, adults
  age.group.sum: age categories of 4-5-year-olds, 6-7-year-olds, 8-9-year-olds, adults
  
  comments: comments if something unusal happened during testing
  
Response variables: 

(0-1-coding):
In the outcome condition we coded a response with 0, when participants chose the agent with the correct outcome, and with 1, when participants chose the agent    with the wrong outcome. In the process condition, we coded a response with 1, when participants chose the agent with the rational process, and with 0, when participants chose the agent with the irrational process. In the process-vs-outcome condition, we coded a response with 1 when participants chose the agent with the rational process/wrong outcome, and with 0 when participants chose the agent with the irrational process/correct outcome. 

(correct-incorrect coding):
Coded with correct (agent with right outcome in the outcome condition, 
                        agent with valid process in the process condition, agent with valid process in the process-vs.-outcome condition).
Coded with incorrect (agent with wrong outcome in the outcome condition, 
                        agent with invalid process in the process condition, agent with invalid process in the process-vs.-outcome condition).

  better.job: Response variable to the question "Who do you think was doing a better job?", coded with 0 and 1
  help.seeking: Response variable to the question "Who do you want to ask for help?", coded with 0 and 1
     
  better.job.correct:  Response variable to the question "Who do you think was doing a better job?", coded with correct and incorrect           
  help.seeking.correct: Response variable to the question "Who do you want to ask for help?", coded with correct and incorrect
     
  better.job.reli: reliability coding 
  help.seeking.reli: reliability coding 
  
  better.job.correct.reli: reliability coding 
  help.seeking.correct.reli: reliability coding 
  
  better.job.why: Response to the answer why they chose a specif agent, participants’ responses to the why questions were categorized by “referring to outcome”, “referring to process”, “I don’t know”, and “other”.  
  help.seeking.why: Response to the answer why they chose a specif agent, participants’ responses to the why questions were categorized by “referring to outcome”, “referring to process”, “I don’t know”, and “other”.  
  
## Power Simulation: 

Script: power.simulation.with.two.way.int.Rmd
Outpuy of Simulation: power.simulation.with.two.way.int.RData
Mock data created in power simulation: power.simulation.mock.data.txt
  
## Analyses and Files
In this study we had two dependent variables "performance evaluation" (Who is doing a better job?) and "partner choice" (Who do you want to ask for help?).
Both variables are analyzed seperatly. 

In the main analyses we only analyzed the data of the process-vs.-outcome condition sincce there was almost no variance (complete seperation issue) in the process condition and the outcome condition. In the supplementray analyzed the data of all conditions. To be able to do so, we addressed the complete separation problem by running 1000 models for each analysis, in which we replaced one data point at a time in each of the cells in which the original data was completely separated. More precisely, from the cells in which we had originally only 0s (indicating that children made an outcome-oriented decision), we randomly sampled one data point which was then replaced by a 1 (indicating children made a process-oriented decision), and vice versa. 

### Main Analyses Files

Scripts: 
main.analysis.better.job.outcome.vs.process.R
main.analysis.help.seeking.outcome.vs.process.R

### Supplementary Analyses Files

Scripts: 
supplementary.analyisis.help.seeking.1000.R
supplementary.analysis.better.job.1000.R

Output of Analyses: 
supplementary.analysis.better.job.1000.RData
supplementary.analysis.help.seeking.1000.RData

## Functions

All functions in this folder are written and provided by Roger Mundry.
