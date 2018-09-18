
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

k = 8
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


all_rmse = []
all_rmse_acc = []
time_list = []

k = 8
index=1
data = pd.read_csv('eachSensors/Merged_data/k_' + str(k) + '/k_' + str(k) + '_cluster_' + str(index) + '_norm_without_empty_column.csv', sep=",")
print "K : ", k, " Index: ", index, " Data amount: ", data.shape
d = data[1:5001].astype(float)
unique_data = select_distinct(d)
col_num, row_num = unique_data.shape
all_data_tuple = change_into_tuple(unique_data)  # it gives you position (in the matrix) and value of the position
np.random.shuffle(all_data_tuple)  # shuffle_data
num_feature = int(row_num / 2)
max_iter = 10000  # set max_iterations
train_pct = 0.9  # split data to training & testing
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
test_data = build_matrix_from_tuple(col_num, row_num, validation)
predicted = R_hat


I = ~np.isnan(test_data)   # indicator for missing values
N = I.sum()                # number of non-missing values
diff = test_data - predicted
sqerror1 = abs(diff)   #RMSE
sqerror2 = diff / (test_data)
avarage = sqerror2[I].sum() / N
sqerror = sqerror1 ** 2  #RMSE
mse = sqerror[I].sum() / N
xx = sqerror2[I][sqerror2[I] >=0.0000008]
np.sqrt(mse)
np.max(xx)
np.min(xx)
avarage

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

### last thing to check before emailing






import numpy as np
import numpy.random as rand
import numpy.random as npr
from numpy.linalg import inv, cholesky
from scipy.stats import chi2
from abc import ABCMeta



rec = Pmf.Pmf.BayesianMatrixFactorization(col_num, row_num, num_feature, all_data_array, train,
                                          validation, max_matrix_R=1, min_matrix_R=0, show_log=False)
num_matrix_u, num_matrix_v, num_feature, main_data, train, validation


num_matrix_u = col_num
num_matrix_v = row_num
num_feature = num_feature
main_data = all_data_array
train = train
validation = validation
max_matrix_R=1
min_matrix_R=0
show_log=False


WI_matrix_u = np.eye(num_feature, dtype='float16')
beta_matrix_u = float(2.0)
df_matrix_u = int(num_feature)
mu_matrix_u = np.zeros((num_feature, 1), dtype='float16')

# Inv-Whishart (matrix_v features)
WI_matrix_v = np.eye(num_feature, dtype='float16')
beta_matrix_v = float(2.0)
df_matrix_v = int(num_feature)
mu_matrix_v = np.zeros((num_feature, 1), dtype='float16')

# Latent Variables
mu_matrix_u = np.zeros((num_feature, 1), dtype='float16')
mu_matrix_v = np.zeros((num_feature, 1), dtype='float16')

alpha_matrix_u = np.eye(num_feature, dtype='float16')
alpha_matrix_v = np.eye(num_feature, dtype='float16')

matrix_u_features = 0.3 * np.random.rand(num_matrix_u, num_feature)
matrix_v_features = 0.3 * np.random.rand(num_matrix_v, num_feature)

last_rmse = None

# the algorithm will converge, but really slow
# use MF's initialize latent parameter will be better
for iteration in xrange(iterations):
    # update matrix_v & matrix_u parameter
    self._update_matrix_v_params()
    self._update_matrix_u_params()

    # update matrix_v & matrix_u_features
    self._udpate_matrix_v_features()
    self._update_matrix_u_features()

    # compute RMSE
    # train errors for fitting
    train_preds = self.predict(self.train)
    train_rmse = self.RMSE(train_preds, np.float16(self.train[:, 2]))

    # validation errors
    validation_preds = self.predict(self.validation)
    validation_rmse = self.RMSE(validation_preds, np.float16(self.validation[:, 2]))

    self.train_errors.append(train_rmse)
    self.validation_errors.append(validation_rmse)
    if self.show_log:
        print "\t Iterations: %3d, total Testing RMSE: %.6f " % (iteration + 1, validation_rmse)

    # stop if converge
    if last_rmse:
        if abs(train_rmse - last_rmse) < tolerance:
            break

    last_rmse = train_rmse







def build_matrix(num_matrix_u, num_matrix_v, matrix_temp_R):
    matrix = np.ones((num_matrix_u, num_matrix_v)) * np.nan
    for item_id in xrange(num_matrix_v):
        data = matrix_temp_R[matrix_temp_R[:, 1] == item_id]
        if data.shape[0] > 0:
            matrix[(data[:, 0]).astype(int), int(item_id)] = data[:, 2]
    return matrix


matrix = build_matrix(num_matrix_u, num_matrix_v, main_data)


def estimate(self, iterations=1000, tolerance=1e-2):
    last_rmse = None

    # the algorithm will converge, but really slow
    # use MF's initialize latent parameter will be better
    for iteration in xrange(iterations):
        # update matrix_v & matrix_u parameter
        self._update_matrix_v_params()
        self._update_matrix_u_params()

        # update matrix_v & matrix_u_features
        self._udpate_matrix_v_features()
        self._update_matrix_u_features()

        # compute RMSE
        # train errors for fitting
        train_preds = self.predict(self.train)
        train_rmse = self.RMSE(train_preds, np.float16(self.train[:, 2]))

        # validation errors
        validation_preds = self.predict(self.validation)
        validation_rmse = self.RMSE(validation_preds, np.float16(self.validation[:, 2]))

        self.train_errors.append(train_rmse)
        self.validation_errors.append(validation_rmse)
        if self.show_log:
            print "\t Iterations: %3d, total Testing RMSE: %.6f " % (iteration + 1, validation_rmse)

        # stop if converge
        if last_rmse:
            if abs(train_rmse - last_rmse) < tolerance:
                break

        last_rmse = train_rmse


def RMSE(self, estimation, truth):
    num_sample = len(estimation)
    sse = np.sum(np.square(truth - estimation))  # sum square error
    return np.sqrt(np.divide(sse, num_sample - 1.0))




def wishartrand(self, nu, phi):
    dim = phi.shape[0]
    chol = cholesky(phi)
    # nu = nu+dim - 1
    # nu = nu + 1 - np.arange(1,dim+1)
    foo = np.zeros((dim, dim))

    for i in range(dim):
        for j in range(i + 1):
            if i == j:
                foo[i, j] = np.sqrt(chi2.rvs(nu - (i + 1) + 1))
            else:
                foo[i, j] = npr.normal(0, 1)
    return np.dot(chol, np.dot(foo, np.dot(foo.T, chol.T)))


def estimate(self, iterations=1000, tolerance=1e-2):
    last_rmse = None

    # the algorithm will converge, but really slow
    # use MF's initialize latent parameter will be better
    for iteration in xrange(iterations):
        # update matrix_v & matrix_u parameter
        self._update_matrix_v_params()
        self._update_matrix_u_params()

        # update matrix_v & matrix_u_features
        self._udpate_matrix_v_features()
        self._update_matrix_u_features()

        # compute RMSE
        # train errors for fitting
        train_preds = self.predict(self.train)
        train_rmse = self.RMSE(train_preds, np.float16(self.train[:, 2]))

        # validation errors
        validation_preds = self.predict(self.validation)
        validation_rmse = self.RMSE(validation_preds, np.float16(self.validation[:, 2]))

        self.train_errors.append(train_rmse)
        self.validation_errors.append(validation_rmse)
        if self.show_log:
            print "\t Iterations: %3d, total Testing RMSE: %.6f " % (iteration + 1, validation_rmse)

        # stop if converge
        if last_rmse:
            if abs(train_rmse - last_rmse) < tolerance:
                break

        last_rmse = train_rmse


def predict(self, data):
    u_features = self.matrix_u_features[(data[:, 0]).astype(int), :]
    i_features = self.matrix_v_features[(data[:, 1]).astype(int), :]
    preds = np.sum(u_features * i_features, 1) + self.mean_matrix_Rs

    if self.max_matrix_Rs:
        preds[preds > self.max_matrix_Rs] = self.max_matrix_Rs

    if self.min_matrix_Rs:
        preds[preds < self.min_matrix_Rs] = self.min_matrix_Rs

    return preds


def _update_matrix_v_params(self):
    N = self.num_matrix_v
    X_bar = np.mean(self.matrix_v_features, 0)
    X_bar = np.reshape(X_bar, (self.num_feature, 1))
    # print 'X_bar', X_bar.shape
    S_bar = np.cov(self.matrix_v_features.T)
    # print 'S_bar', S_bar.shape

    norm_X_bar = X_bar - self.mu_matrix_v
    # print 'norm_X_bar', norm_X_bar.shape

    WI_post = self.WI_matrix_v + N * S_bar + np.dot(norm_X_bar, norm_X_bar.T) * (N * self.beta_matrix_v) / (
    self.beta_matrix_v + N)
    # print 'WI_post', WI_post.shape

    # Not sure why we need this...
    WI_post = (WI_post + WI_post.T) / 2.0
    df_post = self.df_matrix_v + N

    # update alpha_matrix_v
    self.alpha_matrix_v = self.wishartrand(df_post, WI_post)

    # update mu_matrix_v
    mu_temp = (self.beta_matrix_v * self.mu_matrix_v + N * X_bar) / (self.beta_matrix_v + N)
    # print "mu_temp", mu_temp.shape
    lam = cholesky(inv(np.dot(self.beta_matrix_v + N, self.alpha_matrix_v)))
    # print 'lam', lam.shape
    self.mu_matrix_v = mu_temp + np.dot(lam, rand.randn(self.num_feature, 1))
    # print 'mu_matrix_v', self.mu_matrix_v.shape


def _update_matrix_u_params(self):
    # same as _update_matrix_u_params
    N = self.num_matrix_u
    X_bar = np.mean(self.matrix_u_features, 0).T
    X_bar = np.reshape(X_bar, (self.num_feature, 1))

    # print 'X_bar', X_bar.shape
    S_bar = np.cov(self.matrix_u_features.T)
    # print 'S_bar', S_bar.shape

    norm_X_bar = X_bar - self.mu_matrix_u
    # print 'norm_X_bar', norm_X_bar.shape

    WI_post = self.WI_matrix_u + N * S_bar + np.dot(norm_X_bar, norm_X_bar.T) * (N * self.beta_matrix_u) / (
    self.beta_matrix_u + N)
    # print 'WI_post', WI_post.shape

    # Not sure why we need this...
    WI_post = (WI_post + WI_post.T) / 2.0
    df_post = self.df_matrix_u + N

    # update alpha_matrix_u
    self.alpha_matrix_u = self.wishartrand(df_post, WI_post)

    # update mu_matrix_v
    mu_temp = (self.beta_matrix_u * self.mu_matrix_u + N * X_bar) / (self.beta_matrix_u + N)
    # print 'mu_temp', mu_temp.shape
    lam = cholesky(inv(np.dot(self.beta_matrix_u + N, self.alpha_matrix_u)))
    # print 'lam', lam.shape
    self.mu_matrix_u = mu_temp + np.dot(lam, rand.randn(self.num_feature, 1))
    # print 'mu_matrix_u', self.mu_matrix_u.shape


def _udpate_matrix_v_features(self):
    # Gibbs sampling for matrix_v features
    for matrix_v_id in xrange(self.num_matrix_v):
        vec = ~np.isnan(self.matrix[:, matrix_v_id])
        # print 'vec', vec.shape
        if vec.shape[0] == 0:
            continue

        # print(vec)
        features = self.matrix_u_features[vec, :]
        # print 'features', features.shape
        matrix_Rs = self.matrix[vec, matrix_v_id] - self.mean_matrix_Rs
        matrix_Rs_len = len(matrix_Rs)
        matrix_Rs = np.reshape(matrix_Rs, (matrix_Rs_len, 1))

        # print 'matrix_Rs', matrix_Rs.shape
        covar = inv(self.alpha_matrix_v + self.beta * np.dot(features.T, features))
        # print 'covar', covar.shape
        lam = cholesky(covar)

        temp = self.beta * np.dot(features.T, matrix_Rs) + np.dot(self.alpha_matrix_v, self.mu_matrix_v)
        # print 'temp', temp.shape
        mean = np.dot(covar, temp)
        # print 'mean', mean.shape
        temp_feature = mean + np.dot(lam, rand.randn(self.num_feature, 1))
        temp_feature = np.reshape(temp_feature, (self.num_feature,))
        self.matrix_u_features[int(matrix_v_id), :] = temp_feature


def _update_matrix_u_features(self):
    self.matrix = self.matrix.T
    # Gibbs sampling for matrix_u features
    for matrix_u_id in xrange(self.num_matrix_u):
        vec = ~np.isnan(self.matrix[:, matrix_u_id])
        # print 'vec', vec.shape
        if vec.shape[0] == 0:
            continue

        # print(vec)
        features = self.matrix_v_features[vec, :]
        # print 'features', features.shape
        matrix_Rs = self.matrix[vec, matrix_u_id] - self.mean_matrix_Rs
        matrix_Rs_len = len(matrix_Rs)
        matrix_Rs = np.reshape(matrix_Rs, (matrix_Rs_len, 1))

        # print 'matrix_Rs', matrix_Rs.shape
        covar = inv(
            self.alpha_matrix_u + self.beta * np.dot(features.T, features))
        lam = cholesky(covar)
        temp = self.beta * np.dot(features.T, matrix_Rs) + np.dot(self.alpha_matrix_u, self.mu_matrix_u)
        mean = np.dot(covar, temp)
        # print 'mean', mean.shape
        temp_feature = mean + np.dot(lam, rand.randn(self.num_feature, 1))
        temp_feature = np.reshape(temp_feature, (self.num_feature,))
        self.matrix_u_features[matrix_u_id, :] = temp_feature


class Base(object):

    __metaclass__ = ABCMeta

    def __init__(self):
        self.train_errors = []
        self.validation_errors = []

    def __reper__(self):
        return self.__class__.__name__


class BayesianMatrixFactorization(Base):

    def __init__(self, num_matrix_u, num_matrix_v, num_feature, main_data, train, validation, **params):
        super(BayesianMatrixFactorization, self).__init__()

        self.show_log = params.get('show_log')
        self.num_matrix_u = num_matrix_u
        self.num_matrix_v = num_matrix_v
        self.num_feature = num_feature
        self.train = np.asarray(train)
        self.validation = np.asarray(validation)
        self.mean_matrix_Rs = np.mean(self.train[:, 2])

        self.max_matrix_Rs = params.get('max_matrix_Rs')
        self.min_matrix_Rs = params.get('min_matrix_Rs')
        if self.max_matrix_Rs:
            self.max_matrix_Rs = float(self.max_matrix_Rs)
        if self.min_matrix_Rs:
            self.min_matrix_Rs = float(self.min_matrix_Rs)

        # Hyper Parameter
        self.beta = float(params.get('beta', 2.0))

        # Inv-Whishart (matrix_u features)
        self.WI_matrix_u = np.eye(num_feature, dtype='float16')
        self.beta_matrix_u = float(params.get('beta_matrix_u', 2.0))
        self.df_matrix_u = int(params.get('df_matrix_u', num_feature))
        self.mu_matrix_u = np.zeros((num_feature, 1), dtype='float16')

        # Inv-Whishart (matrix_v features)
        self.WI_matrix_v = np.eye(num_feature, dtype='float16')
        self.beta_matrix_v = float(params.get('beta_matrix_v', 2.0))
        self.df_matrix_v = int(params.get('df_matrix_v', num_feature))
        self.mu_matrix_v = np.zeros((num_feature, 1), dtype='float16')

        # Latent Variables
        self.mu_matrix_u = np.zeros((num_feature, 1), dtype='float16')
        self.mu_matrix_v = np.zeros((num_feature, 1), dtype='float16')

        self.alpha_matrix_u = np.eye(num_feature, dtype='float16')
        self.alpha_matrix_v = np.eye(num_feature, dtype='float16')

        self.matrix_u_features = 0.3 * np.random.rand(num_matrix_u, num_feature)
        self.matrix_v_features = 0.3 * np.random.rand(num_matrix_v, num_feature)

        self.matrix = self.build_matrix(num_matrix_u, num_matrix_v, main_data)

    def RMSE(self, estimation, truth):
        num_sample = len(estimation)
        sse = np.sum(np.square(truth - estimation))    # sum square error
        return np.sqrt(np.divide(sse, num_sample - 1.0))

    def build_matrix(self, num_matrix_u, num_matrix_v, matrix_temp_R):

        matrix = np.ones((num_matrix_u, num_matrix_v)) * np.nan
        for item_id in xrange(num_matrix_v):
            data = matrix_temp_R[matrix_temp_R[:, 1] == item_id]
            if data.shape[0] > 0:
                matrix[(data[:, 0]).astype(int), int(item_id)] = data[:, 2]

        return matrix

    def wishartrand(self, nu, phi):
        dim = phi.shape[0]
        chol = cholesky(phi)
        #nu = nu+dim - 1
        #nu = nu + 1 - np.arange(1,dim+1)
        foo = np.zeros((dim,dim))

        for i in range(dim):
            for j in range(i+1):
                if i == j:
                    foo[i,j] = np.sqrt(chi2.rvs(nu-(i+1)+1))
                else:
                    foo[i,j]  = npr.normal(0,1)
        return np.dot(chol, np.dot(foo, np.dot(foo.T, chol.T)))

    def estimate(self, iterations=1000, tolerance=1e-2):
        last_rmse = None

        # the algorithm will converge, but really slow
        # use MF's initialize latent parameter will be better
        for iteration in xrange(iterations):
            # update matrix_v & matrix_u parameter
            self._update_matrix_v_params()
            self._update_matrix_u_params()

            # update matrix_v & matrix_u_features
            self._udpate_matrix_v_features()
            self._update_matrix_u_features()

            # compute RMSE
            # train errors for fitting
            train_preds = self.predict(self.train)
            train_rmse = self.RMSE(train_preds, np.float16(self.train[:, 2]))

            # validation errors
            validation_preds = self.predict(self.validation)
            validation_rmse = self.RMSE(validation_preds, np.float16(self.validation[:, 2]))

            self.train_errors.append(train_rmse)
            self.validation_errors.append(validation_rmse)
            if self.show_log:
                print "\t Iterations: %3d, total Testing RMSE: %.6f " % (iteration + 1, validation_rmse)

            # stop if converge
            if last_rmse:
                if abs(train_rmse - last_rmse) < tolerance:
                    break

            last_rmse = train_rmse

    def predict(self, data):
        u_features = self.matrix_u_features[(data[:, 0]).astype(int), :]
        i_features = self.matrix_v_features[(data[:, 1]).astype(int), :]
        preds = np.sum(u_features * i_features, 1) + self.mean_matrix_Rs

        if self.max_matrix_Rs:
            preds[preds > self.max_matrix_Rs] = self.max_matrix_Rs

        if self.min_matrix_Rs:
            preds[preds < self.min_matrix_Rs] = self.min_matrix_Rs

        return preds

    def _update_matrix_v_params(self):
        N = self.num_matrix_v
        X_bar = np.mean(self.matrix_v_features, 0)
        X_bar = np.reshape(X_bar, (self.num_feature, 1))
        # print 'X_bar', X_bar.shape
        S_bar = np.cov(self.matrix_v_features.T)
        # print 'S_bar', S_bar.shape

        norm_X_bar = X_bar - self.mu_matrix_v
        # print 'norm_X_bar', norm_X_bar.shape

        WI_post = self.WI_matrix_v + N * S_bar + np.dot(norm_X_bar, norm_X_bar.T) * (N * self.beta_matrix_v) / (self.beta_matrix_v + N)
        # print 'WI_post', WI_post.shape

        # Not sure why we need this...
        WI_post = (WI_post + WI_post.T) / 2.0
        df_post = self.df_matrix_v + N

        # update alpha_matrix_v
        self.alpha_matrix_v = self.wishartrand(df_post, WI_post)

        # update mu_matrix_v
        mu_temp = (self.beta_matrix_v * self.mu_matrix_v + N * X_bar) / (self.beta_matrix_v + N)
        # print "mu_temp", mu_temp.shape
        lam = cholesky(inv(np.dot(self.beta_matrix_v + N, self.alpha_matrix_v)))
        # print 'lam', lam.shape
        self.mu_matrix_v = mu_temp + np.dot(lam, rand.randn(self.num_feature, 1))
        # print 'mu_matrix_v', self.mu_matrix_v.shape

    def _update_matrix_u_params(self):
        # same as _update_matrix_u_params
        N = self.num_matrix_u
        X_bar = np.mean(self.matrix_u_features, 0).T
        X_bar = np.reshape(X_bar, (self.num_feature, 1))

        # print 'X_bar', X_bar.shape
        S_bar = np.cov(self.matrix_u_features.T)
        # print 'S_bar', S_bar.shape

        norm_X_bar = X_bar - self.mu_matrix_u
        # print 'norm_X_bar', norm_X_bar.shape

        WI_post = self.WI_matrix_u + N * S_bar + np.dot(norm_X_bar, norm_X_bar.T) * (N * self.beta_matrix_u) / (self.beta_matrix_u + N)
        # print 'WI_post', WI_post.shape

        # Not sure why we need this...
        WI_post = (WI_post + WI_post.T) / 2.0
        df_post = self.df_matrix_u + N

        # update alpha_matrix_u
        self.alpha_matrix_u = self.wishartrand(df_post, WI_post)

        # update mu_matrix_v
        mu_temp = (self.beta_matrix_u * self.mu_matrix_u + N * X_bar) / (self.beta_matrix_u + N)
        # print 'mu_temp', mu_temp.shape
        lam = cholesky(inv(np.dot(self.beta_matrix_u + N, self.alpha_matrix_u)))
        # print 'lam', lam.shape
        self.mu_matrix_u = mu_temp + np.dot(lam, rand.randn(self.num_feature, 1))
        # print 'mu_matrix_u', self.mu_matrix_u.shape

    def _udpate_matrix_v_features(self):
        # Gibbs sampling for matrix_v features
        for matrix_v_id in xrange(self.num_matrix_v):
            vec = ~np.isnan(self.matrix[:, matrix_v_id])
            # print 'vec', vec.shape
            if vec.shape[0] == 0:
               continue

            # print(vec)
            features = self.matrix_u_features[vec, :]
            # print 'features', features.shape
            matrix_Rs = self.matrix[vec, matrix_v_id] - self.mean_matrix_Rs
            matrix_Rs_len = len(matrix_Rs)
            matrix_Rs = np.reshape(matrix_Rs, (matrix_Rs_len, 1))


            # print 'matrix_Rs', matrix_Rs.shape
            covar = inv(self.alpha_matrix_v + self.beta * np.dot(features.T, features))
            # print 'covar', covar.shape
            lam = cholesky(covar)

            temp = self.beta * np.dot(features.T, matrix_Rs)+ np.dot(self.alpha_matrix_v, self.mu_matrix_v)
            # print 'temp', temp.shape
            mean = np.dot(covar, temp)
            # print 'mean', mean.shape
            temp_feature = mean + np.dot(lam, rand.randn(self.num_feature, 1))
            temp_feature = np.reshape(temp_feature, (self.num_feature,))
            self.matrix_u_features[int(matrix_v_id), :] = temp_feature

    def _update_matrix_u_features(self):
        self.matrix = self.matrix.T
        # Gibbs sampling for matrix_u features
        for matrix_u_id in xrange(self.num_matrix_u):
            vec = ~np.isnan(self.matrix[:, matrix_u_id])
            # print 'vec', vec.shape
            if vec.shape[0] == 0:
               continue

            # print(vec)
            features = self.matrix_v_features[vec, :]
            # print 'features', features.shape
            matrix_Rs = self.matrix[vec, matrix_u_id] - self.mean_matrix_Rs
            matrix_Rs_len = len(matrix_Rs)
            matrix_Rs = np.reshape(matrix_Rs, (matrix_Rs_len, 1))

            # print 'matrix_Rs', matrix_Rs.shape
            covar = inv(
                self.alpha_matrix_u + self.beta * np.dot(features.T, features))
            lam = cholesky(covar)
            temp = self.beta * np.dot(features.T, matrix_Rs) + np.dot(self.alpha_matrix_u, self.mu_matrix_u)
            mean = np.dot(covar, temp)
            # print 'mean', mean.shape
            temp_feature = mean + np.dot(lam, rand.randn(self.num_feature, 1))
            temp_feature = np.reshape(temp_feature, (self.num_feature,))
            self.matrix_u_features[matrix_u_id, :] = temp_feature

        # transpose back
        self.matrix = self.matrix.T

