
# HR Analytics: Absenteeism Analysis and Visualization

## Objective

Analyze absenteeism patterns using employee health and HR data, perform SQL-based data transformation and analysis, and visualize key insights using Power BI.

---

## Business Problem

The HR department has tasked the Data Analytics team with the following:

- **Healthy Bonus Program**: Identify "Healthy" individuals with low absenteeism for a bonus split from a **$1,000 budget**.
- **Insurance Budget Allocation**: Distribute **$983,221** among **non-smokers** as proportional wage increases. Based on a count of 686 non-smokers, each receives approximately **$1,433.34**.
- **Dashboard Requirement**: Build a dashboard (based on an approved wireframe) to summarize absenteeism trends and these compensation programs.

---

## Tools Used

- Microsoft SQL Server Management Studio (SSMS)
- Power BI Desktop
- CSV data files
- GitHub (version control & documentation)
---

## Project Steps

### 1. Data Import

- Created `HR_Absenteeism` database in SQL Server.
- Imported CSV files using the SSMS Import Wizard (no manual SQL used for import).
- Tables included: Absenteeism_at_work, Compensation, Reasons.

### 2. Data Cleaning & Transformation

- Validated and adjusted data types.
- Removed duplicates, handled NULLs.
- Applied categorical mapping for clarity (e.g., Reason for Absence, capitalized reason names).

### 3. SQL Querying

Wrote queries to:

- Join employee and absenteeism data.
- Aggregate absenteeism by health category, month, weekday, etc.
- Identify Healthy/Low-Absentee individuals.
- Allocate insurance compensation to non-smokers.

#### Example SQL:

```sql
-- Calculate per-person wage increase for non-smokers
SELECT 
  CAST(ROUND(983221.00 / COUNT(*), 2) AS DECIMAL(10,2)) AS Per_Person_Wage_Increase
FROM Absenteeism_at_work
WHERE Social_smoker = 0;
```

```sql
-- Join absenteeism, compensation, and reasons
SELECT * FROM Absenteeism_at_work a
LEFT JOIN compensation b ON a.ID = b.ID
LEFT JOIN Reasons r ON a.Reason_for_absence = r.Number;

-- Identify healthy employees for $1,000 bonus
SELECT * FROM Absenteeism_at_work
WHERE Social_drinker = 0 AND Social_smoker = 0
  AND Body_mass_index < 25
  AND Absemteeism_time_in_hours < (
    SELECT AVG(Absemteeism_time_in_hours) FROM Absenteeism_at_work
);

-- Count of non-smokers (for wage increase distribution)
SELECT COUNT(*) FROM Absenteeism_at_work
WHERE Social_smoker = 0;
```

> Full query available in [/hr-analytics-absenteeism/absenteeism_analysis.sql](/hr-analytics-absenteeism/absenteeism_analysis.sql)

### 4. Connecting SQL to Power BI

- Connected to SQL Server via **Get Data > SQL Server** in Power BI.
- Loaded only cleaned and aggregated data.

### 5. Dashboard Creation

Built Power BI visuals to display:

- **Line Charts**: Average absenteeism hours by month of absence and count of absenteeism hours by day of the week.
- 
- **Pie Charts**: BMI Category, Health, Smoking, Pet Ownership, Education Level.
- **Cards**:
  - **Total Employees**: 740
  - **Total Absenteeism Hours**: 5124
  - **Average Absenteeism Hours**: 6.92
- **Custom KPIs**: Bonus Eligibility & Insurance Compensation.
- **Narrative Visual**: Automatically generated summaries were used to provide textual insights directly from the data model.
- **Slicer**: A filter by season (Fall, Spring, Summer, Winter) was added to allow HR to analyze absenteeism trends across different times of the year, with an option to "Select All" or view individual seasons.

All visuals are interactive — selecting any category, season, or chart element dynamically updates the rest of the dashboard to reflect relevant data.

**Dashboard Highlights:**

- Overweight had the highest count of BMI category (272), followed by Healthy (264) and Obese (204).
- Overweight accounted for 36.76% of the BMI distribution.
- Highest absenteeism hours were on Monday and Tuesday.
- Absenteeism peaked in April and August, dropped in May and September.
- Most frequent reasons: Musculoskeletal (842), External injuries (729), Medical consultations (624), and Dental consultations (335).

---

## Key Insights

- 37.07% of employees were classified as Healthy.
- April, August, and February saw peak absenteeism.
- Most common absence reasons: Medical & Dental consultations.
- Friday and Tuesday had the highest absentee hours.
- Lifestyle factors (e.g., smoking) impacted absentee trends.

---

## Conclusion

This project showcases an end-to-end data analytics workflow, from data modeling in SQL to actionable insights via Power BI. It enables HR to identify wellness opportunities and allocate budgets more strategically.
