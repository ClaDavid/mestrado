import pandas as pd

if __name__ == '__main__':
    listaToMatrix = pd.read_csv("listToMatrix<dataset>.csv")
    
    tav = listaToMatrix.pivot_table(columns='Var1', index='L1', values='value').reset_index()
    tav = tav.dropna(thresh=0, axis=1)


    tav = tav.fillna(0)

    print(tav)

    tav.to_csv("<dataset_tav>reuters_trans_tav_full_final.csv", index=False)
