
import numpy as np
import numpy.random as rand
import numpy.random as npr
from numpy.linalg import inv, cholesky
from scipy.stats import chi2
from abc import ABCMeta


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
        print('lam', lam)
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


