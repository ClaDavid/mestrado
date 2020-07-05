from sklearn.cluster import KMeans 
from sklearn import metrics 
from scipy.spatial.distance import cdist 
import numpy as np 
import matplotlib.pyplot as plt  
from kneed import KneeLocator
plt.rcParams.update({'font.size': 24})

#Creating the data 
x1 = np.array([2, 1, 1, 3, 1, 6, 6, 7, 5, 4, 7, 8, 9, 7]) 
x2 = np.array([5, 5, 4, 6, 5, 8, 6, 7, 7, 7, 1, 2, 2, 2]) 
X = np.array(list(zip(x1, x2))).reshape(len(x1), 2) 

distortions = [] 
inertias = [] 
mapping1 = {} 
mapping2 = {} 
K = range(1,12)

for k in K: 
    #Building and fitting the model 
    kmeanModel = KMeans(n_clusters=k).fit(X) 
    kmeanModel.fit(X)     
      
    distortions.append(sum(np.min(cdist(X, kmeanModel.cluster_centers_, 
                      'euclidean'),axis=1)) / X.shape[0]) 
    inertias.append(kmeanModel.inertia_) 
  
    mapping1[k] = sum(np.min(cdist(X, kmeanModel.cluster_centers_, 
                 'euclidean'),axis=1)) / X.shape[0] 
    mapping2[k] = kmeanModel.inertia_ 

kn = KneeLocator(K, distortions, curve='convex', direction='decreasing')

plt.figure(figsize=(16,8))
plt.plot(K, distortions, 'bx-')
plt.xlabel('Número de Grupos')
plt.ylabel('Valores de Inércia')
# plt.title('The Elbow Method showing the optimal k')
plt.vlines(kn.knee, plt.ylim()[0], plt.ylim()[1], linestyles='dashed')
plt.style.use('ggplot')
plt.show()
