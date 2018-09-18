__author__ = 'Iot-CoN-User'
# 777777777777777777777777777
# to add output results
import pandas as pd

if __name__ == "__main__":
    all_d = []
    for ind in range(4,21):# change it into 21
        k = ind
        for index in range(1, k+1):
            try:
                data = pd.read_csv('eachSensors/Merged_data/k_'+str(k)+'/k_'+str(k)+'_cluster_'+str(index)+ '_output.csv', sep=",")
                d = pd.DataFrame(data)
                print(d)
                print(len(all_d))
                if len(all_d)!= 0:
                    all_d = pd.concat([all_d, d], axis=1)
                else:
                    all_d = d
            except:
                pass
    R_hat2 = pd.DataFrame(all_d)
    R_hat2.to_csv('eachSensors/Merged_data/Result5.csv', index=False)


