import pandas as pd
import matplotlib.pyplot as plt
from kneed import KneeLocator


if __name__ == '__main__':
    listaToMatrix = pd.read_csv("listaToMatrix.csv")
    # column_name = listaToMatrix.Var1.unique()
    # index_name = listaToMatrix.L1.unique()
    # tav = pd.DataFrame(data=0,index=index_name,columns= column_name)

    tav = listaToMatrix.pivot_table(columns='Var1', index='L1', values='value').reset_index()
    # tav = tav.dropna(thresh=47, axis=1)
    list_tav = []
    documents_common = []

    for i in range(0, 1000):
        tav = tav.dropna(thresh=i, axis=1)
        documents_common.append(i)
        list_tav.append(tav.shape[1])


    kn = KneeLocator(documents_common, list_tav, curve='convex', direction='decreasing')
    print(kn.knee)

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.bar(documents_common, list_tav)
    plt.vlines(kn.knee, plt.ylim()[0], plt.ylim()[1], linestyles='dashed')
    plt.style.use('ggplot')
    plt.show()


    # tav = tav.fillna(0)

    # print(tav)

    # tav.to_csv("reuters_tav.csv", index=False)
