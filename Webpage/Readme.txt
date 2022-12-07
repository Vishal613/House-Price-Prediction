Direction to run and navigate through the webpage -

#Open Terminal 
#conda create -n py39 python =3.9.0 (use python 3.9 to avoid any issues) 
#conda activate py39
#Once activated, !pip install required packages. Required packages present in Readme.txt file.
#After packages installed, >streamlit run app.py
#The webpage is opened automatically in browser
#On the sidebar of the webpage, input desired parameters by selecting the options provided. Some parameters are given in the form of sliders.
#On the main page, the input parameters specified are shown and the prediction of stroke is shown.
#If prediction is 1, the patient is risk of stroke. If prediction is 0, the patient is not at a risk of stroke.
#On the bottom of the webpage, select the type of ML Model to view visualization for that model.
#For example, select ‘Logistic Regression’ and click on display to display the output metrics along with vizualization graphs.
#Click on ‘Show Raw Data’ to view the data taken for that model. The raw data refreshers randomly everytime the model viewed is changed.


Packages to be installed -

!pip install numpy
!pip install streamlit
!pip install pickle
!pip install sklearn
!pip install pandas
!pip install math
!pip install matplotlib


Note : 
#If 'app.py' file unable to run, run the 'SDM Project.ipynb' first to obtain 'kc_cleaned.csv file'. 
Now, run 'train.py' file once and then run 'app.py' in the command prompt as
>stream run app.py

#Make sure all files are in the same folder shown in the IDE used.