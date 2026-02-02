
import numpy as np
from sklearn.linear_model import LinearRegression

years = np.array([2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023]).reshape(-1, 1)
annual_mean = np.array([66030, 73190, 79920, 78580, 85040, 87020, 89040, 91380, 95380])

model = LinearRegression()
model.fit(years, annual_mean)

slope = model.coef_[0]
intercept = model.intercept_

def forcast_instance(startSalary, startYear, targetYear):
    yearsAhead = startSalary+slope*(targetYear-startYear)
    return yearsAhead

def forcast_collection(sYear, sal):
    forcastDict={}
    Target=sYear
    for _ in range (1,40):
        Target+=1
        na=forcast_instance(sal, sYear, Target)
        forcastDict[Target]=na
    
    return forcastDict

myForcast = forcast_collection(2025, 70000)
print(myForcast)