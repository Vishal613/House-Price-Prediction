import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import pickle
import numpy as np
import math
from sklearn.preprocessing import LabelEncoder
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import Ridge, LinearRegression 
from sklearn.neighbors import KNeighborsRegressor
from sklearn import linear_model
from sklearn.model_selection import train_test_split
from sklearn.metrics import precision_score, recall_score, mean_squared_error, r2_score, mean_absolute_error

st.title("Price Prediction")
st.write('This UI predicts the **Housing Price**')
st.write('---')

# Sidebar

# Header of Specify Input Parameters
st.sidebar.header('Specify Input Parameters')

st.sidebar.markdown('Upload Input File')

#Uploading input file
fileupload = st.sidebar.file_uploader("CSV Input File", type=["csv"])
if fileupload is not None:
    input_df=pd.read_csv(fileupload)
else:
    def user_input_features():
        Date = st.sidebar.slider('Date', 0.00, 371.00)
        Bedrooms = st.sidebar.slider('Bedrooms', 0, 33)
        Bathrooms = st.sidebar.slider('Bathrooms', 0.00, 8.00, step=0.25)
        SQFT_Living = st.sidebar.slider('SQFT_living', 290, 13540)
        SQFT_Lot = st.sidebar.slider('SQFT_Lot', 520, 1651359)
        Floors = st.sidebar.slider('Floors', 1.0, 3.5, step=0.5)
        WaterFront = st.sidebar.slider('WaterFront', 0, 1)
        View = st.sidebar.slider('View', 0, 4)
        Condition = st.sidebar.slider('Condition', 1, 5)
        Grade = st.sidebar.slider('Grade',  1, 13)
        SQFT_Above = st.sidebar.slider('SQFT_Above', 290, 9410)
        SQFT_Basement = st.sidebar.slider('SQFT_Basement', 4820)
        Year_Built = st.sidebar.slider('Year_Built', 1900, 2015)
        Year_Renovated = st.sidebar.selectbox('Year_Renovated', (0, 2015))
        Zipcode = st.sidebar.slider('Zipcode', 0, 69)
        Latitude = st.sidebar.slider('Latitude', 47.155, 47.777, step=0.001)
        Longitude = st.sidebar.slider('Longitude', -122.519, -121.315, step=0.001)
        SQFT_Living15 = st.sidebar.slider('SQFT_Living15', 399.00, 6210.00)
        SQFT_Lot15 = st.sidebar.slider('SQFT_Lot15', 651.00, 871200.00)
        data = {'date': Date,
                'bedrooms': Bedrooms,
                'bathrooms': Bathrooms,
                'sqft_living': SQFT_Living,
                'sqft_lot': SQFT_Lot,
                'floors': Floors,
                'waterfront': WaterFront,
                'view': View,
                'condition': Condition,
                'grade': Grade,
                'sqft_above': SQFT_Above,
                'sqft_basement': SQFT_Basement,
                'year_built': Year_Built,
                'year_renovated': Year_Renovated,
                'zipcode': Zipcode,
                'latitude': Latitude,
                'longitude': Longitude,
                'sqft_living15': SQFT_Living15,
                'sqft_lot15': SQFT_Lot15,}
        features = pd.DataFrame(data, index=[0])
        return features
    input_df = user_input_features()


if fileupload is None:
    df=input_df.copy()
    #Encoding the Values
    le = LabelEncoder()
    df['date'] = le.fit_transform(df['date'])
    df['zipcode'] = le.fit_transform(df['zipcode'])
    df=df[:1]
elif fileupload is not None:
    df=pd.read_csv('kc_cleaned.csv')
    df=df[:1]
    df = df.drop(columns=['price'])
    st.write(df)
else:
    st.write('Upload Failed')

# Main Panel
# Print specified input parameters
st.header('Specified Input parameters')
st.write(df)
st.write('---')

load_model2 = pickle.load(open('file_name2.pkl','rb'))

print(df)
print(df.to_numpy())
st.header('Prediction of Price')
out=load_model2.predict(df)

#Print Output

if out[0]==1:
    st.write(f'Price Increased, Predited {out[0]}')
else:
    st.write(f'Price, Predited {out[0]}')

st.write('---')


# Creating functions to define models for Visualizations
def main():
    st.write('***Visualization of Housing Price Prediction***')
    st.sidebar.title('Parameters for Visualization')

    def load_data():
        data = pd.read_csv('kc_cleaned.csv')
        label = LabelEncoder()
        for col in data.columns:
            data[col] = label.fit_transform(data[col])
        return data

    def split(dataframe):
        x = df.drop(columns =['price'])
        y = df.price
        x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.3)
        return x_train, x_test, y_train, y_test
    
    df = load_data()
    class_names = ['Price Increased', 'Price Same']

    x_train, x_test, y_train, y_test = split(df)
    
    st.sidebar.subheader('Select ML Model')
    Regression_Model = st.sidebar.selectbox('Machine Learning Model', ('Linear Regression', 'Lasso Regression', 'KNN Regression', 'Ridge Regression'))
    
    if Regression_Model == 'Linear Regression':
        if st.sidebar.button("Display", key='display'):
            st.subheader("Linear Regression")
            lr_model = LinearRegression()
            lr_model.fit(x_train, y_train)
            accuracy = lr_model.score(x_test, y_test)
            y_pred_lr = lr_model.predict(x_test)
            st.write("Accuracy ", accuracy.round(2))
            plt.plot(y_pred_lr,label="preds")
            st.write("MSE: ", mean_squared_error(y_test, y_pred_lr))
            st.write('R2 Score:', r2_score(y_test, y_pred_lr))
            rmse = math.sqrt(mean_squared_error(y_test, y_pred_lr))
            st.write("RMSE Score:", rmse)
            st.write('Mean Absolute Error:', mean_absolute_error(y_test, y_pred_lr))
    
    if Regression_Model == 'Lasso Regression':
        if st.sidebar.button("Display", key='display'):
            st.subheader("Lasso Regression")
            lar_model = linear_model.Lasso(alpha=0.1)
            lar_model.fit(x_train, y_train)
            accuracy = lar_model.score(x_test, y_test)
            y_pred_lar = lar_model.predict(x_test)
            st.write("Accuracy ", accuracy.round(2))
            plt.plot(y_pred_lar,label="preds")
            st.write("MSE: ", mean_squared_error(y_test, y_pred_lar))
            st.write('R2 Score:', r2_score(y_test, y_pred_lar))
            rmse = math.sqrt(mean_squared_error(y_test, y_pred_lar))
            st.write("RMSE Score:", rmse)
            st.write('Mean Absolute Error:', mean_absolute_error(y_test, y_pred_lar))
    

    if Regression_Model == 'KNN Regression':
        if st.sidebar.button("Display", key='display'):
            st.subheader("KNN Regression")
            knn_model = KNeighborsRegressor(n_neighbors=100)
            knn_model.fit(x_train, y_train)
            accuracy = knn_model.score(x_test, y_test)
            y_pred_knn = knn_model.predict(x_test)
            st.write("Accuracy ", accuracy.round(2))
            plt.plot(y_pred_knn,label="preds")
            st.write("MSE: ", mean_squared_error(y_test, y_pred_knn))
            st.write('R2 Score:', r2_score(y_test, y_pred_knn))
            rmse = math.sqrt(mean_squared_error(y_test, y_pred_knn))
            st.write("RMSE Score:", rmse)
            st.write('Mean Absolute Error:', mean_absolute_error(y_test, y_pred_knn))
    
    if Regression_Model == 'Ridge Regression':
        if st.sidebar.button("Display", key='display'):
            st.subheader("Ridge Regression")
            rr_model = Ridge()
            rr_model.fit(x_train, y_train)
            accuracy = rr_model.score(x_test, y_test)
            y_pred_rr = rr_model.predict(x_test)
            st.write("Accuracy ", accuracy.round(2))
            plt.plot(y_pred_rr,label="preds")
            st.write("MSE: ", mean_squared_error(y_test, y_pred_rr))
            st.write('R2 Score:', r2_score(y_test, y_pred_rr))
            rmse = math.sqrt(mean_squared_error(y_test, y_pred_rr))
            st.write("RMSE Score:", rmse)
            st.write('Mean Absolute Error:', mean_absolute_error(y_test, y_pred_rr))


    if st.sidebar.checkbox("Show raw data", False):
        st.subheader("Price Prediction Dataset (Regressionsn)")
        st.write(df)


if __name__ == '__main__':
    main()
