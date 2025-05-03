--create a join table
SELECT * FROM Absenteeism_at_work a
LEFT JOIN Compensation b
ON a.ID = b.ID
LEFT JOIN Reasons r ON
a.Reason_for_absence = r.Number;

--find the healthiest employees for $1000 bonus
SELECT * FROM Absenteeism_at_work
WHERE Social_drinker = 0 AND Social_smoker = 0
AND Body_mass_index < 25 AND
Absemteeism_time_in_hours < (SELECT AVG(Absemteeism_time_in_hours) FROM Absenteeism_at_work)

--compensation rate increase for non-smokers - $983221 total budget
SELECT ROUND(CAST(983221 AS FLOAT) / COUNT(*), 2) AS wage_increase
FROM Absenteeism_at_work
WHERE Social_smoker = 0;

--optimize the query for visualization
SELECT a.ID, CONCAT(UPPER(LEFT(r.Reason, 1)), LOWER(SUBSTRING(Reason, 2, LEN(Reason)))), Month_of_absence, Body_mass_index,
CASE WHEN Body_mass_index < 18.5 THEN 'Underweight'
     WHEN Body_mass_index BETWEEN 18.5 AND 24.9 THEN 'Healthy'
	 WHEN Body_mass_index BETWEEN 25 AND 30 THEN 'Overweight'
	 WHEN Body_mass_index >30 THEN 'Obese' 
	 ELSE 'Unknown' 
	 END AS BMI_Category,
CASE WHEN Month_of_absence IN(12, 1, 2) THEN 'Winter'
	 WHEN Month_of_absence IN(3, 4, 5) THEN 'Spring'
	 WHEN Month_of_absence IN(6, 7, 8) THEN  'Summer'
	 WHEN Month_of_absence IN(9, 10, 11) THEN 'Fall'
	 ELSE 'Unknown'
	 END AS Season_Names,
Month_of_absence,
Day_of_the_week,
Transportation_expense,
Education, 
Son,
Social_drinker,
Social_smoker,
Pet,
Disciplinary_failure,
Age,
Work_load_Average_day,
Absenteeism_time_in_hours
FROM Absenteeism_at_work a 
LEFT JOIN Compensation b ON a.ID = b.ID 
LEFT JOIN Reasons r ON a.Reason_for_absence = r.Number




