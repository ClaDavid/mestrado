import numpy as np
import logging
import pandas as pd

import sys
sys.path.append('..')


from fuzzy_clustering_modificado import FCMMOD
#from fuzzycmeans.visualization import draw_model_2d


def example(weight=None):
    #X = np.array([[1, 1], [1, 2], [2, 2], [9, 10], [10, 10], [10, 9], [9, 9], [20,20]])
    X = pd.DataFrame(np.random.randint(0,100,size=(8, 2)))
    if weight is None:
        weight = np.repeat(1, X.shape[0], axis=0)
    fcm = FCMMOD(n_clusters=3)
    #fcm.set_logger(tostdout=True, level=logging.DEBUG)
    #fcm.fit(X, [0, 0, 0, 1, 1, 1, 1, 2])
    fcm.fit(X, weight = weight)
    # fcm.fit(X)
    #testing_data = np.array([[0, 1.9], [5, 3], [4, 4], [8, 9], [9.5, 6.5], [5, 5], [15,15], [12,12], [14,14], [19,10]])
    #predicted_membership = fcm.predict(testing_data)
    #print("\n\ntesting data")
    #print(testing_data)
    #print("predicted membership")
    #print(predicted_membership)
    #print("predicted membership")
    #print("\n\n")
    print("u")
    #print(fcm.u)    
    membership_fuzzy = pd.DataFrame(data=fcm.u[0:,0:],
                       index=[i for i in range(fcm.u.shape[0])],
                       columns=['' + str(i) for i in range(fcm.u.shape[1])])
    print(membership_fuzzy)
    print("hard clustering")
    print(membership_fuzzy.idxmax(axis=1))
    print("centers")
    print(fcm.cluster_centers_)
    #draw_model_2d(fcm, data=testing_data, membership=predicted_membership)

example()