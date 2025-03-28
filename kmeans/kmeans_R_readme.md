
# Customer Segmentation Using K-Means Clustering (R)

## 💼 Why K-Means for Business

K-Means clustering helps businesses uncover patterns in customer behavior and group similar customers together. This helps businesses focus their efforts smarter — like creating offers people actually care about, spending less on guesswork, and building stronger customer relationships.

## 📌 Objective
Use K-Means to find patterns in customer data so businesses can better understand their customers and improve marketing.

---

## 📊 Dataset Description

**Source:** Mall Customer Dataset  
**Size:** 200 rows × 5 columns

### Key Variables:
- **CustomerID**: Unique identifier (dropped during processing)
- **Gender**: Gender of the customer (excluded from clustering)
- **Age**: Age of the customer
- **Annual Income**: Income of the customer (in thousands)
- **Spending Score**: Score assigned based on purchasing behavior (1–100)

---

## 🧹 Data Preprocessing

- ✅ No missing or duplicate values
- ✏️ Renamed columns for readability and consistent case
- ❌ Dropped `CustomerID` as it wasn’t meaningful for clustering
- 🧮 Gender was excluded based on EDA showing minimal variation
- 🚫 Capped outliers in `Annual Income` using the IQR method
- 📏 Standardized `Age`, `Annual Income`, and `Spending Score` to ensure fair distance measurement during clustering

---

## 📈 Exploratory Data Analysis (EDA)

- **Age**: The age distribution is multimodal, meaning there are several age groups. It’s slightly right-skewed, with fewer older customers above 60.
- **Annual Income**: Most customers earn between $30K and $90K. Very few earn more than $100K, and the income distribution is nearly uniform with slight peaks.
- **Spending Score**: The values are spread across the full range, showing lots of variation and potential for identifying different spending behaviors.

---

## 🔍 Clustering with K-Means

- 📦 Used the Elbow Method (plot of WSS for k = 1 to 10) to determine optimal clusters (**k = 4**)
- 🌀 K-Means was applied to standardized data
- 📊 Visualized clusters using scatter plots (e.g., Income vs Spending Score)
- 🧠 Found distinct clusters with clear behavioral patterns:
  - High income + high spending
  - High income + low spending
  - Low income + high spending
  - Moderate income and spending group

---

## 🧠 What the Clusters Mean

Here are the four customer groups and what businesses could offer them:
  1. **Emerging High Spenders**: Young, rising earners with high spending potential. 
     - 🔄 Offer: Trendy tech gadgets, fashion-forward collections, and early access to new product drops to feed their desire for exclusivity and novelty.
  2. **Young, Low Earners, Moderate Spenders**: Price-sensitive but responsive segment. 
     - 🔄 Offer: Student-friendly bundles, flash sales, gamified reward points, and affordable lifestyle accessories that appeal to younger crowds.
  3. **Mature, Low-Income, Less Spenders**: Conservative and low engagement group. 
     - 🔄 Offer: Health and savings-focused programs, essential goods packages, and senior-friendly discounts with simple messaging.
  4. **Average Aged, High Earners, Cautious Spenders**: High purchasing power but selective behavior.
     - 🔄 Offer: Premium service upgrades, bundled subscriptions (wellness, fitness, finance), and offers built around quality and reliability.

---

## ✅ Conclusion

- K-Means effectively segmented mall customers into 4 key groups using behavioral and income data
- Business can now allocate marketing resources based on data-driven customer groups

---

## 📝 Notes

- I removed extreme values in Annual Income before scaling the data.
- I used Elbow Method to pick 4 clusters.
- I created 3D plots to help visualize the customer groups.

## 🛠️ Tools Used

- R
- `ggplot2` for visualization
- `dplyr` for data manipulation
- Base R functions for K-Means and standardization

---
