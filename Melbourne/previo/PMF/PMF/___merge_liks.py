__author__ = 'Iot-CoN-User'

# 7.1
# to add output results
import pandas as pd

if __name__ == "__main__":
    all_d = []
    for ind in range(3,21):# change it into 21
        k = ind
        for index in range(1, k+1):
            try:
                data = pd.read_csv('data_other/June_13/'+str(k)+'_lik_'+str(index)+'.csv', sep=",")
                d = pd.DataFrame(data)
                # d.append(k)
                # d.append(index)
                # print(d)
                # print(len(all_d))
                if len(all_d) != 0:
                    all_d = pd.concat([all_d, d], axis=1)
                else:
                    all_d = d
            except:
                pass
    R_hat2 = pd.DataFrame(all_d)
    R_hat2.to_csv('data_other/june_13/all_merged-2.csv', index=False)
print("Done!")

