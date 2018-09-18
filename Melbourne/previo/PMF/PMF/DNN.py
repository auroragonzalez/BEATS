__author__ = 'Iot-CoN-User'
# Neural Network test
# 10, 10,10, 10,10, 10,10, 10,10, 10,10, 10

import numpy as np
import pandas as pd
import DeepNN as dnn
import time

def change_into_class_mode(data_file):
    data_list = []
    class_list = []
    for each_row_tuple_index in range(0, len(data_file)):
        for each_col_index in range(0, len(data_file[each_row_tuple_index])):
            if ~np.isnan(data_file[each_row_tuple_index][each_col_index]):
                data_list.append((each_row_tuple_index,each_col_index))
                class_list.append(data_file[each_row_tuple_index][each_col_index])
    return data_list, class_list


def split_train_test(data_temp, percent_test):
    """Split the data into train/test sets.
    :param int percent_test: Percentage of data to use for testing. Default 10."""

    unique_data = data_temp[~np.isnan(data_temp).any(axis=1)] # delete all empty rows

    n, m = unique_data.shape             # # U, # V
    N = n * m                     # # cells in matrix
    test_size = int(round((N * percent_test)/100,0))   # use 10% of data as test set
    train_size = N - test_size    # and remainder for training

    # print( train_size, test_size)
    # Prepare train/test
    train = unique_data.copy()
    test = np.ones(unique_data.shape) * np.nan

    # Draw random data
    to_sample = np.where(~np.isnan(train))
    idx_pairs = zip(to_sample[0], to_sample[1]) # id pairs
    indices = np.arange(len(idx_pairs)) #index
    sample2 = np.random.choice(indices, replace=False, size=test_size)


    # train set >> test set.
    for idx in sample2:
        idx_pair = idx_pairs[idx]
        test[idx_pair] = train[idx_pair]  # transfer to test set
        train[idx_pair] = np.nan          # remove from train set

    assert(np.isnan(train).sum() == test_size)
    assert(np.isnan(test).sum() == train_size)
    return train, test


def select_distinct(data_array):
    a = np.ascontiguousarray(data_array)
    unique_a = np.unique(a.view([('', a.dtype)]* a.shape[1]))
    return unique_a.view(a.dtype).reshape((unique_a.shape[0], a.shape[1]))


def rmse_class(test_data_l, predicted_l):
    diff = np.array(test_data_l).astype(float)- np.array(predicted_l).astype(float)
    N = len(test_data_l)
    mse = abs(diff) #Rm
    mse2 = np.array(mse).astype(float) / np.array(test_data_l).astype(float)
    mse3 = mse2 * 100
    avg_percent = mse3.sum() / N
    mse4 = mse ** 2 #Rm
    mse5 = mse4.sum()/N #Rm
    return np.sqrt(mse5), np.max(mse3), np.min(mse3), avg_percent   #RMSE, Max, Min (0), Avg


def data_into_class(data):
    result_data = []
    for index_each in range(0, len(data)):
        if (data[index_each]<=0.05):
            result_data.append("0.0")
        elif (data[index_each]<=0.15):
            result_data.append("0.1")
        elif (data[index_each]<=0.25):
            result_data.append("0.2")
        elif (data[index_each]<=0.35):
            result_data.append("0.3")
        elif (data[index_each]<=0.45):
            result_data.append("0.4")
        elif (data[index_each]<=0.55):
            result_data.append("0.5")
        elif (data[index_each]<=0.65):
            result_data.append("0.6")
        elif (data[index_each]<=0.75):
            result_data.append("0.7")
        elif (data[index_each]<=0.85):
            result_data.append("0.8")
        elif (data[index_each]<=0.95):
            result_data.append("0.9")
        else:
            result_data.append("1.0")
    return result_data


# Starting Point for the code
if __name__ == "__main__":
    rmse_all = []
    for ind in range(3,21):# change it into 21
        k = ind
        for index in range(1, k+1):
            data = pd.read_csv('data_other/Merged_data/k_'+str(k)+'/k_'+str(k)+'_cluster_'+str(index)+ '_norm_without_empty_column.csv', sep=",")
            print "K : ", k, " Index: ", index, " Data amount: ", data.shape

            if len(data) > 100:
                new_d = data[1:]
                d = new_d.astype(float)
                unique = select_distinct(d)

                training, testing = split_train_test(unique, 10)
                training_SVM_NN, training_SVM_NN_class = change_into_class_mode(training)
                testing_SVM_NN, testing_SVM_NN_class = change_into_class_mode(testing)
                training_SVM_NN_class_organized = data_into_class(training_SVM_NN_class)

                start_time=time.time()
                NN_classification = dnn.MLPClassifier()
                NN_classification.fit(np.array(training_SVM_NN),np.array(training_SVM_NN_class_organized))
                elapsed_time_train=time.time()-start_time
                start_time2 = time.time()

                result_from_NN = NN_classification.predict( np.array(testing_SVM_NN))
                elapsed_time_estimate=time.time()-start_time2
                out_RMSE, out_max, out_min, out_avg = rmse_class(np.array(testing_SVM_NN_class),result_from_NN)
                rmse_all.append([k, index, out_RMSE, out_max, out_min, out_avg])
                print("RMSE= ", out_RMSE)
                print "Done with index = ", index

    R_hat_NN = pd.DataFrame(np.array(rmse_all).T)
    R_hat_NN.to_csv('data_other/Merged_data/All_Result_DNN.csv', index=False)
    print "All done!!!"
