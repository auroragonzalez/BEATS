
import numpy as np
import pandas as pd
import PMF_Thesis as Pmf
import time

# To change all the data into tuples (col, row, value)
def change_into_tuple(data_file):
    data_list = []
    for each_row_tuple_index in range(0, len(data_file)):
        for each_col_index in range(0, len(data_file[each_row_tuple_index])):
            if ~np.isnan(data_file[each_row_tuple_index][each_col_index]):
                data_list.append([each_row_tuple_index,each_col_index,data_file[each_row_tuple_index][each_col_index]])
    return data_list


# to select only distinct values
def select_distinct(data_array):
    a = np.ascontiguousarray(data_array)
    unique_a = np.unique(a.view([('', a.dtype)] * a.shape[1]))
    return unique_a.view(a.dtype).reshape((unique_a.shape[0], a.shape[1]))

# Evaluation function.
# Calculate root mean squared error. Ignoring missing values in the test data.
def rmse(test_data, predicted):
    I = ~np.isnan(test_data)   # indicator for missing values
    N = I.sum()                # number of non-missing values
    diff = test_data - predicted
    sqerror1 = abs(diff)   #RMSE
    sqerror2 = diff / (test_data)
    avarage = sqerror2[I].sum() / N
    sqerror = sqerror1 ** 2  #RMSE
    mse = sqerror[I].sum() / N     #RMSE
    return np.sqrt(mse), np.max(sqerror2[I]), np.min(sqerror2[I]), avarage  # RMSE, max, min, avg


def build_matrix_from_tuple(columns_data, rows_data, matrix_R):
    matrix = np.ones((columns_data, rows_data)) * np.nan
    for item_id in xrange(rows_data):
        data = matrix_R[matrix_R[:, 1] == item_id]
        if data.shape[0] > 0:
            matrix[(data[:, 0]).astype(int), int(item_id)] = data[:, 2]
    return matrix


def accuracy_percent_pmf(test_data_l, predicted_l):
    I = ~np.isnan(test_data_l)   # indicator for missing values
    N = I.sum()                # number of non-missing values
    substract_d = np.array(test_data_l).astype(float) - np.array(predicted_l).astype(float)
    sub_only_target1 = substract_d[I]
    # print len(sub_only_target1), N
    total_number = len(sub_only_target1)
    sub_only_target2 = sub_only_target1[sub_only_target1<=0.1]
    total_number1 = len(sub_only_target2)
    range_exact_percent = float((total_number1 *100)/total_number)
    sub_only_target3 = sub_only_target1[sub_only_target1<=0.2]
    total_number2 = len(sub_only_target3)
    range_five_percent = float((total_number2 * 100)/total_number)
    sub_only_target4 = sub_only_target1[sub_only_target1<=0.3]
    total_number3 = len(sub_only_target4)
    range_thirty_percent = float((total_number3 * 100)/total_number)
    return range_exact_percent, range_five_percent, range_thirty_percent


if __name__ == "__main__":

    all_rmse = []
    all_rmse_acc = []
    time_list = []
    auroRes = []
    for ind in range(3,9):#range(3,21): # change it into 21
        k = ind
        for index in range(1, k+1):
            data = pd.read_csv('eachSensors/Merged_data/k_'+str(k)+'/k_'+str(k)+'_cluster_'+str(index)+ '_norm_without_empty_column.csv', sep=",")
            print "K : ", k, " Index: ", index, " Data amount: ", data.shape

            if len(data) > 100:

                d = data[1:5001].astype(float)
                unique_data = select_distinct(d)
                col_num, row_num = unique_data.shape
                all_data_tuple = change_into_tuple(unique_data)
                np.random.shuffle(all_data_tuple)    # shuffle_data

                # set feature numbers
                num_feature = int(row_num/2)

                # set max_iterations
                max_iter = 10000

                # split data to training & testing
                train_pct = 0.9
                train_size = int(train_pct * len(all_data_tuple))
                train = np.array(all_data_tuple[:train_size])
                validation = np.array(all_data_tuple[train_size:])

                all_data_array = np.array(all_data_tuple)

                # models
                start_time=time.time()
                rec = Pmf.Pmf.BayesianMatrixFactorization(col_num, row_num, num_feature, all_data_array, train,
                                                          validation, max_matrix_R=1, min_matrix_R=0, show_log=False)

                elapsed_time_train=time.time()-start_time
                start_time2 = time.time()
                rec.estimate(max_iter)
                elapsed_time_estimate=time.time()-start_time2
                # validation
                R_hat = np.dot(rec.matrix_u_features, rec.matrix_v_features.T)
                # print R_hat.shape
                rmse_data, max_error, min_error, ave_error = rmse(build_matrix_from_tuple(col_num, row_num,validation), R_hat)
                all_rmse.append([k, index, rec.validation_errors[-1], rmse_data, max_error, min_error, ave_error])

                exact_acc, five_acc, thirty_acc = accuracy_percent_pmf(build_matrix_from_tuple(col_num, row_num, validation), R_hat)
                all_rmse_acc.append([k, index, exact_acc, five_acc, thirty_acc])

                time_list.append([k, index, elapsed_time_train,len(train), elapsed_time_estimate, len(validation)])
                realM = build_matrix_from_tuple(col_num, row_num, validation)
                predictedM = R_hat
                predicted = predictedM[~np.isnan(realM)]
                real= realM[~np.isnan(realM)]
                MAPE = sum((abs(real-predicted))/len(real))*100
                RMSE = np.sqrt(np.mean((real - predicted)**2));
                CVRMSE = RMSE/np.mean(real)*100
                auroRes.append([MAPE,RMSE,CVRMSE])

            print "Done with index = ", index

    if len(all_rmse) > 0:
        lst_rmse = pd.DataFrame(np.array(all_rmse).T)
        lst_rmse.to_csv('data_other/June_13/All_Result_PMF_True_RMSE.csv', index=False)

    if len(all_rmse_acc) > 0:
        lst_acc = pd.DataFrame(np.array(all_rmse_acc).T)
        lst_acc.to_csv('data_other/June_13/All_Result_PMF_True_Acc.csv', index=False)

    if len(time_list) > 0:
        lst_time = pd.DataFrame(np.array(time_list).T)
        lst_time.to_csv('data_other/June_13/All_Result_PMF_True_elapsed_time.csv', index=False)

    print "All done!!!"

    auroRes