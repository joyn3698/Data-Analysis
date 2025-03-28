
# Customer Segmentation Using K-Means Clustering (R)

## 📌 Objective
Segment customers into distinct groups based on demographic and purchasing behavior to aid in personalized marketing strategies.

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
- 🧮 Gender was excluded due to minimal variation in key features
- 🚫 Capped 2 income outliers using the IQR method
- 📏 Standardized `Age`, `Annual Income`, and `Spending Score` due to varying units (required for K-means)

---

## 📈 Exploratory Data Analysis (EDA)

- **Age**: Histogram showed a multimodal distribution with slight right skew (fewer customers >60)
- **Annual Income**: Uniform distribution with mild peaks
- **Spending Score**: Wide variation, suggesting meaningful cluster potential

---

## 🔍 Clustering with K-Means

- 📦 Used the Elbow Method to determine optimal clusters (k = 5)
- 🌀 K-Means was applied to standardized data
- 📊 Visualized clusters using scatter plots (e.g., Income vs Spending Score)
- 🧠 Found distinct clusters with clear behavioral patterns:
     - High income + high spending
     - High income + low spending
     - Low income + high spending
     - Low to average income + low spending (includes young and older low earners)

---

## 🧠 Insights

- Clustering revealed targetable customer profiles for marketing:
  - VIPs: High income, high spending (loyalty programs)
  - Budget-conscious: High income, low spending (push offers)
  - High-potential: Low income, high spending (retain and grow)
- Helped inform strategies for targeted campaigns, loyalty benefits, and promotional focus

---

## ✅ Conclusion

- K-Means effectively segmented mall customers using behavioral and income data
- Business can now allocate marketing resources based on data-driven customer groups
- Future work: Apply DBSCAN or hierarchical clustering for comparison

---

## 🛠️ Tools Used

- R
- `ggplot2` for visualization
- `dplyr` for data manipulation
- Base R functions for K-Means and standardization

---
