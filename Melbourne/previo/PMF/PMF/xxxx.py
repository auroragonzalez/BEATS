
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


# To change all the data into tuples (col, row, value)
def change_into_tupleAu(data_file):
    data_list = []
    for each_row_tuple_index in range(0, len(data_file)):
        for each_col_index in range(0, len(data_file[each_row_tuple_index])):
            if data_file[each_row_tuple_index][each_col_index]!=0:
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




all_rmse = []
all_rmse_acc = []
time_list = []

k = 4
for index in range(1, k+1):
    data = pd.read_csv('eachSensors/Merged_data/k_' + str(k) + '/k_' + str(k) + '_cluster_' + str(index) + '_norm_without_empty_column.csv', sep=",")
    if len(data) > 100:
        print "K : ", k, " Index: ", index, " Data amount: ", data.shape
        d = data[1:5001].astype(float)
        unique_data = select_distinct(d)
        col_num, row_num = unique_data.shape
        all_data_tuple = change_into_tuple(unique_data)   # it gives you position (in the matrix) and value of the position
        np.random.shuffle(all_data_tuple)  # shuffle_data
        num_feature = int(row_num / 2)
        max_iter = 10000  # set max_iterations
        train_pct = 0.9                 # split data to training & testing
        train_size = int(train_pct * len(all_data_tuple))
        train = np.array(all_data_tuple[:train_size])
        validation = np.array(all_data_tuple[train_size:])
        all_data_array = np.array(all_data_tuple)
        rec = Pmf.Pmf.BayesianMatrixFactorization(col_num, row_num, num_feature, all_data_array, train,
                                                  validation, max_matrix_R=1, min_matrix_R=0, show_log=False)
        rec.estimate(max_iter)
        R_hat = np.dot(rec.matrix_u_features, rec.matrix_v_features.T)
        rmse_data, max_error, min_error, ave_error = rmse(build_matrix_from_tuple(col_num, row_num, validation),
                                                        R_hat)
        all_rmse.append([k, index, rec.validation_errors[-1], rmse_data, max_error, min_error, ave_error])
        exact_acc, five_acc, thirty_acc = accuracy_percent_pmf(build_matrix_from_tuple(col_num, row_num, validation), R_hat)
        all_rmse_acc.append([k, index, exact_acc, five_acc, thirty_acc])
       # time_list.append([k, index, elapsed_time_train, len(train), elapsed_time_estimate, len(validation)])
        print "Done with index = ", index


sameNAs = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv", sep=",", header=None)
#ameNAs = sameNAs.transpose()

print("NEW PART")


MAPE = []
RMSE = []
CVRMSE = []

k = 8
for index in range(1, k+1):
#index = 2
    data = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansRes/k"+ str(k) +'/gr_'+ str(index)+'norm01.csv', sep=",")
    motes =pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansRes/k"+ str(k) +'/motes_gr_'+ str(index)+'.csv', sep=",", header=None)
    flattened = [val for sublist in motes.iloc[:,0:].values.tolist() for val in sublist]
    sameNAs.iloc[flattened]
    unique_dataNA = select_distinct(sameNAs.iloc[flattened])
    dataNA = change_into_tupleAu(unique_dataNA.transpose())   #delete the zeroes...
    #data = pd.read_csv("/home/aurora/Escritorio/BMECode/kmeansRes/k"+ str(k) +'/gr_'+ str(index)+'.csv', sep=",")
    #data = pd.read_csv('eachSensors/Merged_data/k_' + str(k) + '/k_' + str(k) + '_cluster_' + str(index) + '_norm_without_empty_column.csv', sep=",")
    print "K : ", k, " Index: ", index, " Data amount: ", data.shape
    d = data[1:1201].astype(float)
    #d = data[1:10].iloc[:,0:5].astype(float)
    unique_data = select_distinct(d)
    col_num, row_num = unique_data.shape
    all_data_tuple = change_into_tuple(unique_data)   # it gives you position (in the matrix) and value of the position
    NAv = [item[:2] for item in dataNA]
    all = [item[:2] for item in all_data_tuple]
    a = all
    b = NAv
    validationPositions = [i for i, item in enumerate(a) if item in b]
    x = range(0,len(all))
    y = validationPositions
    trainPositions = filter(lambda x: x not in y, x)
    #np.random.shuffle(all_data_tuple)  # shuffle_data
    num_feature = int(row_num / 2)
    #max_iter = 10000  # set max_iterations
    #train_pct = 0.9                 # split data to training & testing
    train_size = int(train_pct * len(all_data_tuple))
    train = [all_data_tuple[i] for i in trainPositions]
    validation = [all_data_tuple[i] for i in validationPositions]
    train = np.array(all_data_tuple[:train_size])  # they have previously shuffled the data so now they take the first ´train_size´ elements as train
    #validation = np.array(all_data_tuple[train_size:])
    #train =  np.array(train)
    #validation = np.array(validation)
    all_data_array = np.array(all_data_tuple)
    rec = Pmf.Pmf.BayesianMatrixFactorization(col_num, row_num, num_feature, all_data_array, train,
                                              validation, max_matrix_R=1, min_matrix_R=0, show_log=False)
    rec.estimate(max_iter)
    R_hat = np.dot(rec.matrix_u_features, rec.matrix_v_features.T)
    rmse_data, max_error, min_error, ave_error = rmse(build_matrix_from_tuple(col_num, row_num, validation),
                                                    R_hat)
    all_rmse.append([k, index, rec.validation_errors[-1], rmse_data, max_error, min_error, ave_error])
    exact_acc, five_acc, thirty_acc = accuracy_percent_pmf(build_matrix_from_tuple(col_num, row_num, validation), R_hat)
    all_rmse_acc.append([k, index, exact_acc, five_acc, thirty_acc])
    # time_list.append([k, index, elapsed_time_train, len(train), elapsed_time_estimate, len(validation)])
    print "Done with index = ", index
    exact_acc
    build_matrix_from_tuple(col_num, row_num, validation)
    R_hat
    R_hat
    realM = build_matrix_from_tuple(col_num, row_num, validation)
    predictedM = R_hat
    predicted = predictedM[~np.isnan(realM)]
    real= realM[~np.isnan(realM)]
    MAPE_now = sum((abs(real-predicted))/len(real))*100
    RMSE_now = np.sqrt(np.mean((real - predicted)**2));
    CVRMSE_now = RMSE/np.mean(real)*100
    MAPE.append(MAPE_now)
    RMSE.append(RMSE_now)
    CVRMSE.append(CVRMSE_now)





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









k = 8
for index in range(1, k+1):
#index = 2
    data = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansRes/k"+ str(k) +'/gr_'+ str(index)+'norm01.csv', sep=",")
    motes =pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansRes/k"+ str(k) +'/motes_gr_'+ str(index)+'.csv', sep=",", header=None)
    flattened = [val for sublist in motes.iloc[:,0:].values.tolist() for val in sublist]
    sameNAs.iloc[flattened]
    unique_dataNA = select_distinct(sameNAs.iloc[flattened])
    dataNA = change_into_tupleAu(unique_dataNA.transpose())   #delete the zeroes...
    #data = pd.read_csv("/home/aurora/Escritorio/BMECode/kmeansRes/k"+ str(k) +'/gr_'+ str(index)+'.csv', sep=",")
    #data = pd.read_csv('eachSensors/Merged_data/k_' + str(k) + '/k_' + str(k) + '_cluster_' + str(index) + '_norm_without_empty_column.csv', sep=",")
    print "K : ", k, " Index: ", index, " Data amount: ", data.shape
    d = data[1:1201].astype(float)
    #d = data[1:10].iloc[:,0:5].astype(float)
    unique_data = select_distinct(d)
    col_num, row_num = unique_data.shape
    all_data_tuple = change_into_tuple(unique_data)   # it gives you position (in the matrix) and value of the position
    NAv = [item[:2] for item in dataNA]
    all = [item[:2] for item in all_data_tuple]
    a = all
    b = NAv
    validationPositions = [i for i, item in enumerate(a) if item in b]
    x = range(0,len(all))
    y = validationPositions
    trainPositions = filter(lambda x: x not in y, x)
    #np.random.shuffle(all_data_tuple)  # shuffle_data
    num_feature = int(row_num / 2)
    #max_iter = 10000  # set max_iterations
    #train_pct = 0.9                 # split data to training & testing
    train_size = int(train_pct * len(all_data_tuple))
    train = [all_data_tuple[i] for i in trainPositions]
    validation = [all_data_tuple[i] for i in validationPositions]
    train = np.array(all_data_tuple[:train_size])  # they have previously shuffled the data so now they take the first ´train_size´ elements as train
    #validation = np.array(all_data_tuple[train_size:])
    #train =  np.array(train)
    #validation = np.array(validation)
    all_data_array = np.array(all_data_tuple)
    rec = Pmf.Pmf.BayesianMatrixFactorization(col_num, row_num, num_feature, all_data_array, train,
                                              validation, max_matrix_R=1, min_matrix_R=0, show_log=False)
    rec.estimate(max_iter)
    R_hat = np.dot(rec.matrix_u_features, rec.matrix_v_features.T)
    rmse_data, max_error, min_error, ave_error = rmse(build_matrix_from_tuple(col_num, row_num, validation),
                                                    R_hat)
    all_rmse.append([k, index, rec.validation_errors[-1], rmse_data, max_error, min_error, ave_error])
    exact_acc, five_acc, thirty_acc = accuracy_percent_pmf(build_matrix_from_tuple(col_num, row_num, validation), R_hat)
    all_rmse_acc.append([k, index, exact_acc, five_acc, thirty_acc])
    # time_list.append([k, index, elapsed_time_train, len(train), elapsed_time_estimate, len(validation)])
    print "Done with index = ", index
    exact_acc
    build_matrix_from_tuple(col_num, row_num, validation)
    R_hat
    R_hat
    realM = build_matrix_from_tuple(col_num, row_num, validation)
    predictedM = R_hat
    predicted = predictedM[~np.isnan(realM)]
    real= realM[~np.isnan(realM)]
    MAPE_now = sum((abs(real-predicted))/len(real))*100
    RMSE_now = np.sqrt(np.mean((real - predicted)**2));
    CVRMSE_now = RMSE/np.mean(real)*100
    MAPE.append(MAPE_now)
    RMSE.append(RMSE_now)
    CVRMSE.append(CVRMSE_now)
















0.35462,0.31184,0.3407,0.29503,0.28181,0.28936,0.32716,0.2685,0.33464,0.36614,0.30703
0.34692,0.30707,0.33196,0.28574,0.27158,0.28047,0.31262,0.25609,0.32222,0.35259,0.29712
0.34017,0.30007,0.32369,0.27704,0.26214,0.27057,0.30278,0.24688,0.31189,0.3433,0.28838
0.33584,0.29713,0.31583,0.26968,0.25596,0.26241,0.29536,0.23957,0.30492,0.33619,0.28197
0.33174,0.28948,0.30703,0.26658,0.25352,0.25886,0.28975,0.23372,0.29855,0.33061,0.27621
0.32628,0.28184,0.30189,0.25993,0.24671,0.25033,0.28339,0.22819,0.28997,0.32459,0.27014
0.32303,0.27749,0.29667,0.25793,0.24824,0.24816,0.28075,0.22567,0.28409,0.3204,0.26624
0.31943,0.27149,0.29352,0.25388,0.24639,0.24814,0.2772,0.22396,0.28283,0.3172,0.26252
0.315,0.27081,0.2906,0.25073,0.24352,0.24384,0.27372,0.22069,0.27664,0.31028,0.25716