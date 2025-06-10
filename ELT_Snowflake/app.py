import streamlit as st
import pandas as pd
from snowflake.snowpark import Session
import matplotlib.pyplot as plt
import seaborn as sns


st.set_page_config(page_title= "Retail Sales Dashboard")
plt.style.use('seaborn-v0_8-whitegrid')
sns.set_theme(style="whitegrid")


@st.cache_resource
def snowflake_session():
    conn = {
    'user' : 'joy3',
    'password' : 'Datapass3698@@',
    'account' : 'WGULHNB-XJC50608',
    'warehouse' : 'my_wh',
    'database' : 'retail',
    'schema' : 'PUBLIC'
    }

    session = Session.builder.configs(conn).create()
    return session


def load_data(session):
    query = """
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
    """
    retail = session.sql(query).to_pandas()
    return retail

df = load_data(snowflake_session())



st.markdown("""
    <style>
    html, body, [data-testid="stAppViewContainer"] {
        font-family: 'Open Sans', sans-serif !important; 
        color: #333333; 
        background-color: #f8f9fa;
    }

    .stApp {
        background-color: #f0f2f6;
    }
    h1, h2, h3, h4 {
        text-align: center;
    }

    .block-container {
        text-align: center;
    }
    
    .block-container p {
        text-align: left;
    }
    
    [data-testid = stMetricLabel]{
    display:flex;
    justify-content:center;
    text-align:center;
    }
    
    </style>
""", unsafe_allow_html=True)

st.title("📊 Retail Sales Performance")

# --- KPI Section ---
st.markdown('### 📌 Key Metrics')
kpi1, kpi2, kpi3 = st.columns(3)
kpi1.metric(label ="# 💰 Total Revenue", value = f"${df['total_amount'].sum():,.0f}")
kpi2.metric(label ="# 📦 Average Transaction Value", value = f"${df['total_amount'].mean()}")
kpi3.metric(label ="# 👥 Unique Customers", value = df['customer_id'].nunique())

# --- Time Series ---
st.markdown("### ⏱️ Sales Over Time")
time_series = df.groupby('sale_date')['total_amount'].sum().reset_index()
st.line_chart(time_series.rename(columns={'sale_date':'index'}).set_index('index'))

# --- Product Category Pie Chart ---
st.markdown("### 🛍️ Product Category Sales Distribution")
fig1, ax1 = plt.subplots()
category_data = df.groupby('product_category')['total_amount'].sum().sort_values(ascending=False)
ax1.pie(category_data, labels = category_data.index, autopct = '%1.1f%%',startangle=90)
ax1.set_aspect('equal')
st.pyplot(fig1)

# --- Gender Bar Chart ---
st.markdown("### 🧑‍🤝‍🧑 Sales by Gender")
gender_data = df.groupby('gender')['total_amount'].sum().reset_index()
fig2, ax2 = plt.subplots(facecolor='#f9f9f9')
sns.set_style("whitegrid")
sns.barplot(x='gender', y='total_amount', data=gender_data, ax=ax2, palette='pastel')
ax2.set_ylabel("Total Amount")
ax2.set_facecolor('#f9f9f9')
fig2.patch.set_facecolor('#f9f9f9')
st.pyplot(fig2)

# --- Age Distribution ---
st.markdown("### 🎂 Age-based Sales Distribution")
age_bins = pd.cut(df['age'], bins = [0, 20, 30, 40, 50, 60, 100],
                  labels = ['<20', '20-30', '30-40', '40-50', '50-60', '60+'])
df['age_group'] = age_bins
age_data = df.groupby('age_group')['total_amount'].sum().reset_index()
fig3, ax3 = plt.subplots()
sns.barplot(x='age_group', y='total_amount', data=age_data, ax=ax3, palette='pastel')
ax3.set_ylabel("Total Amount")
st.pyplot(fig3)

# --- Footer ---
st.markdown("---")
st.caption("Retail Analytics Dashboard | Streamlit + Snowflake")
