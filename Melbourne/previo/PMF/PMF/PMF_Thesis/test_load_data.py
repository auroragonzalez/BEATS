import numpy as np

from .BayesianMatrixFactorization import BayesianMatrixFactorization


def build_ml_1m():
    # num_matrix_u = 6040
    # num_matrix_v = 3952
    try:
        with open("ratings.dat", "rb") as f:
            iter_lines = iter(f)
            print(iter_lines)
            matrix_Rs = []
            for line_num, line in enumerate(iter_lines):
                line = line.split('::')[:3] # format (matrix_u_id, matrix_v_id, matrix_Rs)
                line = [int(l) for l in line]
                matrix_Rs.append(line)

        matrix_Rs = np.array(matrix_Rs)
        matrix_Rs[:, (0, 1)] = matrix_Rs[:, (0, 1)] - 1
        return (np.max(matrix_Rs[:, 0])+1), (np.max(matrix_Rs[:, 1]) +1), matrix_Rs
    except:
        pass


if __name__ == "__main__":

    print "Started"
    # load MovieLens data
    num_matrix_u, num_matrix_v, matrix_Rs = build_ml_1m() # get data
    np.random.shuffle(matrix_Rs)    # shuffle_data

    # print matrix_Rs[1]
    # set feature numbers
    num_feature = 20

    # set max_iterations
    max_iter = 5

    # split data to training & testing
    train_pct = 0.9
    train_size = int(train_pct * len(matrix_Rs))
    train = matrix_Rs[:train_size]
    validation = matrix_Rs[train_size:]

    print matrix_Rs[10]
    # models
    rec = BayesianMatrixFactorization(num_matrix_u, num_matrix_v, num_feature, matrix_Rs, train, validation, max_matrix_R=5, min_matrix_R=0)

    rec.estimate(max_iter)
    print "RMSE for Testing data: %.6f \n" , rec.validation_errors[-1]

    #check the top data
    # result = np.dot(rec.matrix_u_features, rec.matrix_v_features.T)
    # print "result: "
    # print(result[10][:100])
    # print "actual: "
    # print(rec.matrix[10][:100])

