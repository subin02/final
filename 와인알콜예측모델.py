# -*- coding: utf-8 -*-
"""사례연구 과제

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1VWnf2F7qclBjnc_ld7PJbWSYYS3P6zdQ

#데이터분석 보고서 작성 1.문제정의, 2. 데이터수집 3.EDA 4.전처리 5.분석방법설명 6.분석과정 7.결과(시각화) 8.인사이트

# 데이터수집
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
wine = pd.read_csv('/content/drive/MyDrive/빅데이터사례연구/winequality-red (1).csv')
wine.head(10)

wine.info()

wine['quality'].astype('float64')

"""# 탐색적 데이터분석"""

#요약통계량
wine.describe()

"""상자그림"""

#상자그림
plt.figure(figsize=(10,6))

sns.boxplot(wine[['fixed acidity']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['volatile acidity']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['citric acid']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['residual sugar']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['chlorides']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['free sulfur dioxide']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['total sulfur dioxide']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['density']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['pH']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['sulphates']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['alcohol']])
plt.show()

plt.figure(figsize=(10,6))

sns.boxplot(wine[['quality']])
plt.show()

#히스토그램
wine.hist(figsize=(70,20), grid=False, layout=(5,15), bins=30)

num_features = wine.select_dtypes(include=[np.number]).columns
sns.pairplot(wine[num_features])

"""# 데이터전처리"""

#결측치 확인
wine.isnull().sum()

#필요없는 변수 삭제
#quaity 삭제
wine.drop('quality', axis=1, inplace=True)
wine.head(10)

#free sulfur dioxide 삭제
wine.drop('free sulfur dioxide', axis=1, inplace=True)
wine.head(10)

#volatile acidity 삭제
wine.drop('volatile acidity', axis=1, inplace=True)
wine.head(10)

"""이상치 제거 -3번 실시

str을 쓰는 이유: 변수가 len을 통해 숫자로 들어가기 때문에 문자열로 변환시켜주는 작업이 필요함!
"""



#Q3 + 1.5*IQR
for a in range(0,len(wine.columns)):
  wine.drop(wine[wine[str(wine.columns[a])]>(wine[str(wine.columns[a])].quantile(0.75) +
                                    (1.5*(wine[str(wine.columns[a])].quantile(0.75)
                                    -wine[str(wine.columns[a])].quantile(0.25))))].index,
                                    axis = 0, inplace=True)

#Q1 - 1.5*IQR
for b in range(0,len(wine.columns)):
  wine.drop(wine[wine[str(wine.columns[b])]<(wine[str(wine.columns[b])].quantile(0.25)-
                                         (1.5*(wine[str(wine.columns[b])].quantile(0.75)
                                         -wine[str(wine.columns[b])].quantile(0.25))))].index,
                                          axis = 0, inplace=True)

"""# 상관관계"""

plt.figure(figsize=(20,10))
correlation_wine = wine.corr()
sns.heatmap(correlation_wine, annot=True)

"""상관관계가 높은 변수들의 산점도"""

plt.scatter(x='fixed acidity', y='pH', data = wine)

plt.scatter(x='fixed acidity', y='citric acid', data = wine)

plt.scatter(x='alcohol', y= 'density', data = wine)

#MAE
from sklearn.metrics import mean_absolute_error
mean_absolute_error(y_test, y_predict)

from sklearn.linear_model import LogisticRegression

wine.columns

"""# 회귀분석"""

wine.info()

from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression

model = LinearRegression()

x = wine[['fixed acidity', 'citric acid', 'residual sugar', 'chlorides',
       'total sulfur dioxide', 'density', 'pH', 'sulphates']]
y = wine[ 'alcohol']

x_train, x_test, y_train, y_test = train_test_split(x, y, train_size=0.8, random_state=800)
model.fit(x_train,y_train)

plt.figure(figsize=(15,7))
plt.plot(model.predict(x_test[:50]), label='predict')
plt.plot(y_test[:50].values.reshape(-1,1), label = 'real')
plt.legend()

plt.figure(figsize=(15,7))
y_predict = model.predict(x_test)

plt.scatter(y_test, y_predict, alpha=0.4)
sns.regplot(x = y_test, y= y_predict )
plt.grid()
plt.show()

print(model.score(x_train, y_train))

print(model.score(x_test, y_test))

#변수중요도
print(model.coef_)

import statsmodels.api as sm

x_train = sm.add_constant(x_train)
model = sm.OLS(y_train, x_train).fit()
print(model.summary())

"""# 예측"""

result = model.predict([[7.3, 0.3, 2, 0.076, 34, 0.996, 3.5, 0.63]])
print(result)

#MAE
from sklearn.metrics import mean_absolute_error
mean_absolute_error(y_test, y_predict)

#MSE
from sklearn.metrics import mean_squared_error
mean_squared_error(y_test, y_predict)

#RMSE
mse = mean_squared_error(y_test, y_predict)
np.sqrt(mse)

#MAPE
from sklearn.metrics import mean_absolute_percentage_error
mean_absolute_percentage_error(y_test, y_predict)

from xgboost import XGBClassifier

model = XGBClassifier(max_depth = 8,
                      n_estimators = 200,
                      nthread = 5,
                      min_child_weight = 20,
                      use_label_encoder = False,
                      random_state = 2022)
model.fit(x_train, y_train)

"""# 전진선택법"""

#전진선택법
import statsmodels.api as sm
variables = wine.columns[:-2].tolist() ## 설명 변수 리스트

y = wine['alcohol'] ## 반응 변수
selected_variables = [] ## 선택된 변수들
sl_enter = 0.05

sv_per_step = [] ## 각 스텝별로 선택된 변수들
adjusted_r_squared = [] ## 각 스텝별 수정된 결정계수
steps = [] ## 스텝
step = 0
while len(variables) > 0:
    remainder = list(set(variables) - set(selected_variables))
    pval = pd.Series(index=remainder) ## 변수의 p-value
    ## 기존에 포함된 변수와 새로운 변수 하나씩 돌아가면서
    ## 선형 모형을 적합한다.
    for col in remainder:
        X = wine[selected_variables+[col]]
        X = sm.add_constant(X)
        model = sm.OLS(y,X).fit()
        pval[col] = model.pvalues[col]

    min_pval = pval.min()
    if min_pval < sl_enter: ## 최소 p-value 값이 기준 값보다 작으면 포함
        selected_variables.append(pval.idxmin())

        step += 1
        steps.append(step)
        adj_r_squared = sm.OLS(y,sm.add_constant(wine[selected_variables])).fit().rsquared_adj
        adjusted_r_squared.append(adj_r_squared)
        sv_per_step.append(selected_variables.copy())
    else:
        break

selected_variables

from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
model2 = LinearRegression()
plt.figure(figsize=(20,10))

x = wine[['density','fixed acidity','pH', 'residual sugar', 'sulphates',
          'citric acid', 'total sulfur dioxide', 'chlorides','volatile acidity']]
y = wine[['alcohol']]

x_train, x_test, y_train, y_test = train_test_split(x,y, train_size=0.7, test_size = 0.3)
model2.fit(x,y)

plt.plot(model2.predict(x_test[:50]), label='predict')
plt.plot(y_test[:50].values.reshape(-1,1), label = 'real')
plt.legend()

plt.figure(figsize=(15,7))
y_predict = model2.predict(x_test)

plt.scatter(y_test, y_predict, alpha=0.4)
plt.show()

print(model2.score(x_train, y_train))

import statsmodels.api as sm

x_train = sm.add_constant(x_train)
model2 = sm.OLS(y_train, x_train).fit()
print(model2.summary())

"""#후진선택법"""

#후진선택법
variables = wine.columns[:-2].tolist() ## 설명 변수 리스트

y = wine['alcohol'] ## 반응 변수
selected_variables1 = variables ## 초기에는 모든 변수가 선택된 상태
sl_remove = 0.05

sv_per_step = [] ## 각 스텝별로 선택된 변수들
adjusted_r_squared = [] ## 각 스텝별 수정된 결정계수
steps = [] ## 스텝
step = 0
while len(selected_variables1) > 0:
    X = sm.add_constant(wine[selected_variables1])
    p_vals = sm.OLS(y,X).fit().pvalues[1:] ## 절편항의 p-value는 뺀다
    max_pval = p_vals.max() ## 최대 p-value
    if max_pval >= sl_remove: ## 최대 p-value값이 기준값보다 크거나 같으면 제외
        remove_variable = p_vals.idxmax()
        selected_variables1.remove(remove_variable)

        step += 1
        steps.append(step)
        adj_r_squared = sm.OLS(y,sm.add_constant(wine[selected_variables1])).fit().rsquared_adj
        adjusted_r_squared.append(adj_r_squared)
        sv_per_step.append(selected_variables1.copy())
    else:
        break

selected_variables1

from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
model1 = LinearRegression()
plt.figure(figsize=(20,10))

x = wine[['fixed acidity', 'volatile acidity', 'citric acid', 'residual sugar',
          'chlorides', 'total sulfur dioxide', 'density', 'pH', 'sulphates']]
y = wine[['alcohol']]

x_train, x_test, y_train, y_test = train_test_split(x,y, train_size=0.7, test_size = 0.3)
model1.fit(x,y)

plt.plot(model1.predict(x_test[:50]), label='predict')
plt.plot(y_test[:50].values.reshape(-1,1), label = 'real')
plt.legend()

plt.figure(figsize=(15,7))
y_predict = model1.predict(x_test)

plt.scatter(y_test, y_predict, alpha=0.4)
plt.show()

print(model1.score(x_train, y_train))

#결정계수
import statsmodels.api as sm

model1 = sm.OLS(y_train, x_train).fit()
print(model1.summary())

"""# 단계별선택법"""

## 전진 단계별 선택법
variables = wine.columns[:-1].tolist() ## 설명 변수 리스트

y = wine['alcohol'] ## 반응 변수
selected_variables2 = [] ## 선택된 변수들
sl_enter = 0.05 # 기준 유의확률
sl_remove = 0.05

sv_per_step = [] ## 각 스텝별로 선택된 변수들
adjusted_r_squared = [] ## 각 스텝별 수정된 결정계수
steps = [] ## 스텝
step = 0
while len(variables) > 0:
    remainder = list(set(variables) - set(selected_variables2))
    pval = pd.Series(index=remainder) ## 변수의 p-value
    ## 기존에 포함된 변수와 새로운 변수 하나씩 돌아가면서
    ## 선형 모형을 적합한다.
    for col in remainder:
        X = wine[selected_variables2+[col]]
        X = sm.add_constant(X)
        model = sm.OLS(y,X).fit()
        pval[col] = model.pvalues[col]

    min_pval = pval.min()
    if min_pval < sl_enter: ## 최소 p-value 값이 기준 값보다 작으면 포함
        selected_variables2.append(pval.idxmin())
        ## 선택된 변수들에대해서
        ## 어떤 변수를 제거할지 고른다.
        while len(selected_variables2) > 0:
            selected_X = wine[selected_variables2]
            selected_X = sm.add_constant(selected_X)
            selected_pval = sm.OLS(y,selected_X).fit().pvalues[1:] ## 절편항의 p-value는 뺀다
            max_pval = selected_pval.max()
            if max_pval >= sl_remove: ## 최대 p-value값이 기준값보다 크거나 같으면 제외
                remove_variable = selected_pval.idxmax()
                selected_variables2.remove(remove_variable)
            else:
                break

        step += 1
        steps.append(step)
        adj_r_squared = sm.OLS(y,sm.add_constant(wine[selected_variables2])).fit().rsquared_adj
        adjusted_r_squared.append(adj_r_squared)
        sv_per_step.append(selected_variables2.copy())
    else:
        break

selected_variables2

from sklearn.linear_model import LinearRegression

from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
model2 = LinearRegression()
plt.figure(figsize=(20,10))

x = wine[['density',
 'fixed acidity',
 'residual sugar',
 'pH',
 'sulphates',
 'total sulfur dioxide',
 'citric acid']]
y = wine[['alcohol']]

x_train, x_test, y_train, y_test = train_test_split(x,y, train_size=0.7, test_size = 0.3, random_state=200)
modelfit = model2.fit(x,y)

plt.plot(model2.predict(x_test[:50]), label='predict')
plt.plot(y_test[:50].values.reshape(-1,1), label = 'real')
plt.legend()

plt.figure(figsize=(15,7))
y_predict = model2.predict(x_test)

plt.scatter(y_test, y_predict, alpha=0.4)
plt.show()

print(model2.score(x_train, y_train))

#결정계수
import statsmodels.api as sm

x_train = sm.add_constant(x_train)
model2 = sm.OLS(y_train, x_train).fit()
print(model2.summary())
