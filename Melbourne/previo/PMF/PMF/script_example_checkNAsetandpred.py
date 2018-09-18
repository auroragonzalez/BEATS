
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



all_rmse = []
all_rmse_acc = []
time_list = []
MAPE = []
RMSE = []
CVRMSE = []


k=3
index = 1
data = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k"+ str(k) +'/gr_'+ str(index)+'norm01.csv', sep=",", header=None)


print "K : ", k, " Index: ", index, " Data amount: ", data.shape
d = data[0:5001].astype(float)
#unique_data = select_distinct(d)
unique_data = np.array(d)
col_num, row_num = unique_data.shape
all_data_tuple = change_into_tuple(unique_data)


### luego
def change_into_tupleAu(data_file):
    data_list = []
    for each_row_tuple_index in range(0, len(data_file)):
        for each_col_index in range(0, len(data_file[each_row_tuple_index])):
            if data_file[each_row_tuple_index][each_col_index]!=0:
                data_list.append([each_row_tuple_index,each_col_index,data_file[each_row_tuple_index][each_col_index]])
    return data_list

sameNAs = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv", sep=",", header=None)
motes =pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k"+ str(k) +'/motes_gr_'+ str(index)+'.csv', sep=",", header=None)
flattened = [val for sublist in motes.iloc[:,0:].values.tolist() for val in sublist]
sameNAs.iloc[list(map(lambda x: x - 1, flattened))] # the -1 is because there is no sensor 0!but there is sensor 53!
unique_dataNA = sameNAs.iloc[list(map(lambda x: x - 1, flattened))].values
dataNA = change_into_tupleAu(unique_dataNA.transpose())   #delete the zeroes

NAv = [item[:2] for item in dataNA]
all = [item[:2] for item in all_data_tuple]
a = all
b = NAv
validationPositions = [i for i, item in enumerate(a) if item in b]
x = range(0,len(all))
y = validationPositions
trainPositions = filter(lambda x: x not in y, x)
num_feature = int(row_num / 2)
max_iter = 10000  # set max_iterations
train2 = np.array([all_data_tuple[i] for i in trainPositions])
validation2 = np.array([all_data_tuple[i] for i in validationPositions])
validation3 = sorted(validation2, key=lambda x: int(x[1])) # order by sensor
validation4 =[l.tolist() for l in validation3]

validationCL1 = [x[2] for x in validation4]
NAPerSensor1 = (unique_dataNA!=0).sum(1)
motes1 =motes



train = train2
validation = validation2

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
build_matrix_from_tuple(col_num, row_num,validation)
R_hat

from itertools import compress
fil = [item[1]==0 for item in dataNA]
justfil = list(compress(dataNA, fil))




realM = build_matrix_from_tuple(col_num, row_num, validation)
predictedM = R_hat
predicted = predictedM[~np.isnan(realM)]
real= realM[~np.isnan(realM)]


trrealM = np.transpose(realM)
trreal = trrealM[~np.isnan(trrealM)]

trpredM = np.transpose(predictedM)
trpred = trpredM[~np.isnan(trrealM)]

sensRep = np.array(motes[0][np.sort([item[1] for item in dataNA])])


pd.DataFrame(np.transpose([sensRep, trreal, trpred]).to_csv('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/metrics/realMCL1.csv', index=False)
pd.DataFrame(real).to_csv('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/metrics/realCL1.csv', index=False)




## segundo cluster


index = 2
data = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k"+ str(k) +'/gr_'+ str(index)+'norm01.csv', sep=",", header=None)


print "K : ", k, " Index: ", index, " Data amount: ", data.shape
d = data[0:5001].astype(float)
#unique_data = select_distinct(d)
unique_data = np.array(d)
col_num, row_num = unique_data.shape
all_data_tuple = change_into_tuple(unique_data)


### luego
def change_into_tupleAu(data_file):
    data_list = []
    for each_row_tuple_index in range(0, len(data_file)):
        for each_col_index in range(0, len(data_file[each_row_tuple_index])):
            if data_file[each_row_tuple_index][each_col_index]!=0:
                data_list.append([each_row_tuple_index,each_col_index,data_file[each_row_tuple_index][each_col_index]])
    return data_list

sameNAs = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv", sep=",", header=None)
motes =pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k"+ str(k) +'/motes_gr_'+ str(index)+'.csv', sep=",", header=None)
flattened = [val for sublist in motes.iloc[:,0:].values.tolist() for val in sublist]
sameNAs.iloc[list(map(lambda x: x - 1, flattened))] # the -1 is because there is no sensor 0!but there is sensor 53!
unique_dataNA = sameNAs.iloc[list(map(lambda x: x - 1, flattened))].values
dataNA = change_into_tupleAu(unique_dataNA.transpose())   #delete the zeroes

NAv = [item[:2] for item in dataNA]
all = [item[:2] for item in all_data_tuple]
a = all
b = NAv
validationPositions = [i for i, item in enumerate(a) if item in b]
x = range(0,len(all))
y = validationPositions
trainPositions = filter(lambda x: x not in y, x)
num_feature = int(row_num / 2)
max_iter = 10000  # set max_iterations
train2 = np.array([all_data_tuple[i] for i in trainPositions])
validation2 = np.array([all_data_tuple[i] for i in validationPositions])
validation3 = sorted(validation2, key=lambda x: int(x[1])) # order by sensor
validation4 =[l.tolist() for l in validation3]

validationCL2 = [x[2] for x in validation4]

NAPerSensor2 = (unique_dataNA!=0).sum(1)
motes2 =motes
## tercer cluster


index = 3
data = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k"+ str(k) +'/gr_'+ str(index)+'norm01.csv', sep=",", header=None)


print "K : ", k, " Index: ", index, " Data amount: ", data.shape
d = data[0:5001].astype(float)
#unique_data = select_distinct(d)
unique_data = np.array(d)
col_num, row_num = unique_data.shape
all_data_tuple = change_into_tuple(unique_data)


### luego
def change_into_tupleAu(data_file):
    data_list = []
    for each_row_tuple_index in range(0, len(data_file)):
        for each_col_index in range(0, len(data_file[each_row_tuple_index])):
            if data_file[each_row_tuple_index][each_col_index]!=0:
                data_list.append([each_row_tuple_index,each_col_index,data_file[each_row_tuple_index][each_col_index]])
    return data_list

sameNAs = pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv", sep=",", header=None)
motes =pd.read_csv("/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k"+ str(k) +'/motes_gr_'+ str(index)+'.csv', sep=",", header=None)
flattened = [val for sublist in motes.iloc[:,0:].values.tolist() for val in sublist]
sameNAs.iloc[list(map(lambda x: x - 1, flattened))] # the -1 is because there is no sensor 0!but there is sensor 53!
unique_dataNA = sameNAs.iloc[list(map(lambda x: x - 1, flattened))].values
dataNA = change_into_tupleAu(unique_dataNA.transpose())   #delete the zeroes

NAv = [item[:2] for item in dataNA]
all = [item[:2] for item in all_data_tuple]
a = all
b = NAv
validationPositions = [i for i, item in enumerate(a) if item in b]
x = range(0,len(all))
y = validationPositions
trainPositions = filter(lambda x: x not in y, x)
num_feature = int(row_num / 2)
max_iter = 10000  # set max_iterations
train2 = np.array([all_data_tuple[i] for i in trainPositions])
validation2 = np.array([all_data_tuple[i] for i in validationPositions])
validation3 = sorted(validation2, key=lambda x: int(x[1])) # order by sensor
validation4 =[l.tolist() for l in validation3]

validationCL3 = [x[2] for x in validation4]


NAPerSensor3 = (unique_dataNA!=0).sum(1)
motes3 =motes


validation = validationCL1 + validationCL2 + validationCL3  # no has ordenado las validaciones por lo tanto hija mia esto no tiene ni pies ni cabeza

NAPerSensor = list(NAPerSensor1) + list(NAPerSensor2) + list(NAPerSensor3)
motes = [x[0] for x in motes1.values.tolist()] + [x[0] for x in motes2.values.tolist()] + [x[0] for x in motes3.values.tolist()]

thesens = [item for item, count in zip(motes, NAPerSensor) for i in range(count)]

pd.DataFrame(validation, thesens).to_csv('/home/aurora/Git_repos/posgradoActual/Melbourne/chequeoValidacion.csv', index=True)

from itertools import compress

fil = [item[1]==0 for item in dataNA]

justzeros = list(compress(dataNA, fil))

NAv = [item[:2] for item in justzeros]
all = [item[:2] for item in all_data_tuple]
a = all
b = NAv
validationPositions = [i for i, item in enumerate(a) if item in b]
x = range(0,len(all))
y = validationPositions
trainPositions = filter(lambda x: x not in y, x)
num_feature = int(row_num / 2)
max_iter = 10000  # set max_iterations
train2 = np.array([all_data_tuple[i] for i in trainPositions])
validation2 = np.array([all_data_tuple[i] for i in validationPositions])

validationCL3 = [x[2] for x in validation2]



realM = build_matrix_from_tuple(col_num, row_num, validation)
predictedM = R_hat
predicted = predictedM[~np.isnan(realM)]
real= realM[~np.isnan(realM)]





NAv = [item[:2] for item in dataNA]
all = [item[:2] for item in all_data_tuple]
a = all
b = NAv
validationPositions = [i for i, item in enumerate(a) if item in b]
x = range(0,len(all))
y = validationPositions
trainPositions = filter(lambda x: x not in y, x)
num_feature = int(row_num / 2)
max_iter = 10000  # set max_iterations
train2 = np.array([all_data_tuple[i] for i in trainPositions])
validation2 = np.array([all_data_tuple[i] for i in validationPositions])


fil = [item[1]==0 for item in validation2]
justzeros = list(compress(validation2, fil))
[item[2] for item in justzeros]