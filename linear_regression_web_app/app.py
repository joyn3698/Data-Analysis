import pickle
import streamlit as st

st.title("Predicting Yearly Amount Spent with Linear Regression")
st.markdown("""
<style>
.stApp {
    background-color: #f8f9fa;
    color: #212529;
    font-family: 'Segoe UI', sans-serif;
    padding: 2rem;
}

h1 {
    color: #0d6efd;
    text-align: center;
}

.block-container {
    padding-top: 2rem;
}


.stNumberInput input {
    font-size: 1.5rem;
    background-color: #ffffff;
    color: #212529;
    border: 1px solid #ced4da;
    border-radius: 0.375rem;
}

button[kind="primary"] {
    background-color: #0d6efd;
    color: white;
    border-radius: 0.375rem;
}
</style>
""", unsafe_allow_html=True)



left_col, right_col = st.columns([1, 2])

with left_col :
    st.markdown("""
    <div style='border-right: 2px solid #dee2e6; padding-right: 20px; height: 100%;'>
        <h4>🤖 Evaluation</h4>
        <ul>
            <li>R² = 0.9809</li>
            <li>Adjusted R² = 0.9803</li>
            <li>MAE = 8.43</li>
            <li>MSE = 103.92</li>
            <li>RMSE = 10.19</li>
            <li>Residuals are nearly normal</li>
        </ul>
    </div>
    """, unsafe_allow_html=True)
    





with right_col:
    model = pickle.load(open("/Data-Analysis/linear_regression_web_app/model.pkl", "rb"))
    x1 = st.number_input("Enter the Avg session length with the stylist(30-60 minutes)", min_value=30, max_value=60, value=None)
    x2 = st.number_input("Enter the time spent on the app(minutes)", min_value=0, max_value=100, value=None)
    x3 = st.number_input("Enter the time spent on the website(minutes)", min_value=0, max_value=100, value=None)
    x4 = st.number_input("Length of membership in months(0-12)", min_value=0, max_value=12, value=None)

    if st.button("OK"):
        user_input = [[x1, x2, x3, x4]]
        pred = model.predict(user_input)
        st.success(f"Predicted yearly spending: ${int(round(pred[0], 0))}")





