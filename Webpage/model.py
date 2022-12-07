import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import train_test_split
import pickle
from sklearn import linear_model

def load_data():
    data = pd.read_csv('kc_cleaned.csv')
    label = LabelEncoder()
    for col in data.columns:
        data[col] = label.fit_transform(data[col])
    return data

df=load_data()

def split(df):
    x = df.drop(columns =['price'])
    y = df.price
    x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.3)
    return x_train, x_test, y_train, y_test

x_train, x_test, y_train, y_test=split(df)

model = linear_model.Lasso(alpha=0.1)
model.fit(x_train, y_train)
accuracy = model.score(x_test, y_test)
y_pred_lr = model.predict(x_test)
print("Accuracy ", accuracy.round(2))

file_name2 = 'req_model2.sav'
pickle.dump(model,open('file_name2.pkl','wb'))