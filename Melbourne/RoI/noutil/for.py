mean_abs_err = 0.
n_batches=1
for i, j in mb_indices:
    print(i, j)
    # Reset derivative matrices each minibatch
    dU[:, :] = 0.
    dV[:, :] = 0.
    # Slice out row and column indices
    r_i = r[i:j]
    c_i = c[i:j]
    # Get data corresponding to the row and column indices
    X_i = X_s[r_i, c_i].toarray().ravel() - X_mean
    # Compute predictions
    pred = np.sum(U[r_i] * V[c_i], axis=1)
    print(r_i)
    print(c_i)
    # Compute how algorithm is doing
    mean_abs_err += np.sum(np.abs(pred - X_i)) / (n_batches * (j - i))
    # Loss has a tendency to be unstable, but is the "right thing"
    # to monitor instead of sum_abs_err
    # pred_loss = (pred - X_i) ** 2
    # Compute gradients
    grad_loss = 2 * (pred - X_i)
    grad_U = grad_loss * V[c_i] + reg * U[r_i]
    grad_V = grad_loss * U[r_i] + reg * V[c_i]
    dU[r_i] = grad_U
    dV[c_i] = grad_V
    print(dU)
   # Momentum storage
    print(U_inc)
    U_inc = mom * U_inc + lr * dU
    print(U_inc)
    V_inc = mom * V_inc + lr * dV
    U -= U_inc
    V -= V_inc
