# 📊 Simple ELT Pipeline with Snowflake, Python & Streamlit

A beginner-friendly ELT pipeline that demonstrates how to ingest CSV data, load it into Snowflake, perform SQL-based transformations, and visualize the results through a Streamlit dashboard.

---

## 🧰 Tech Stack

- **Snowflake** – Cloud data warehouse
- **Python** – Data loading and scripting (`pandas`, `snowflake.snowpark`)
- **Streamlit** – Lightweight web dashboard

---

## 🚀 Workflow Overview

### 1. Extract

Source of dataset : https://www.kaggle.com/datasets/mohammadtalib786/retail-sales-dataset
The dataset is then read from a local CSV file using pandas:
```python
df = pd.read_csv("retail_sales_dataset.csv")
```

---

### 2. Load

A Snowflake session is established using Snowpark and the data is written to a Snowflake table:

```python
conn = {
    'user': 'your_user',
    'password': 'your_password',
    'account': 'your_account',
    'warehouse': 'my_wh',
    'database': 'retail',
    'schema': 'PUBLIC'
}

session = Session.builder.configs(conn).create()

session.write_pandas(df, table_name="raw_retail_sales", auto_create_table=True, overwrite=True)
```

> **Note:** Warehouse and database were created directly within the Snowflake UI.

---

### 3. Transform

Dataset did not have null or missing values.
A transformation query is written using Snowflake SQL to:
- Format date field
- Rename and clean columns
- Prepare a CTE table for analysis

```sql
WITH retail_sales AS(SELECT TO_DATE("Date", 'YYYY-MM-DD') AS "sale_date",
"Customer ID" AS "customer_id", 
"Gender" AS "gender",
"Age" AS "age",
"Product Category" AS "product_category",
"Quantity" AS "quantity",
"Price per Unit" AS "price_per_unit",
"Total Amount" AS "total_amount"
FROM "raw_retail_sales")

SELECT * FROM retail_sales
```

The transformed data is then fetched back into Python:

```python
retail = session.sql(query).to_pandas()
```

---

### 4. Visualize

Using `Streamlit`, a dashboard (`app.py`) is built to present the transformed data with KPIs, graphs, and charts:

```bash
streamlit run app.py
```

---

## 📁 Project Structure

```
├── app.py                   
├── snowpro.ipynb                 
└── README.md               
```

---


## ✅ Requirements

- Python 3.9+
- Snowflake account
- Streamlit
- pandas
- snowflake-snowpark-python

---

## 📌 Notes

- The project uses Snowflake's `write_pandas` to automatically create the table.
- It's ideal for demo purposes, small team POCs, or getting started with cloud-based ELT.

---

