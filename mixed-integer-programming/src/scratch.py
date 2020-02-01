from time import monotonic
from random import randrange, choices
from mip import Model, xsum, maximize, BINARY, CONTINUOUS, LinExpr
from numpy.random import exponential
import numpy as np


def knapsack():
    p = [10, 13, 18, 31, 7, 15]
    w = [11, 15, 20, 35, 10, 33]
    c, I = 47, range(len(w))

    m = Model("knapsack")
    x = [m.add_var(var_type=BINARY) for i in I]
    m.objective = maximize(xsum(p[i] * x[i] for i in I))
    m += xsum(w[i] * x[i] for i in I) <= c
    print(m.optimize())


def random_knapsack(n: int, interval):
    I = range(n)
    p = [randrange(*interval) for _ in I]
    w = [randrange(*interval) for _ in I]
    c = round(sum(interval) / 2 * n)

    m = Model("random-knapsack")
    x = [m.add_var(var_type=BINARY) for i in I]
    m.objective = maximize(xsum(p[i] * x[i] for i in I))
    m += xsum(w[i] * x[i] for i in I) <= c
    start = monotonic()
    print(m.optimize())
    print(f"DURATION: {(monotonic() - start) * 1000} ms")


class PiecewiseLinearFunction:
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def __len__(self):
        return len(self.a)


def maximize_sum(plfs, max_costs):
    # pylint: disable=too-many-locals
    m = Model("bid-landscapes")
    costs = LinExpr()
    objective = LinExpr()
    xs = []
    ws = []
    for (i, plf) in enumerate(plfs):
        k = len(plf)
        w = [m.add_var(var_type=CONTINUOUS) for _ in range(0, k)]
        x = [m.add_var(var_type=BINARY) for _ in range(0, k - 1)]
        xs.append(x)
        ws.append(w)

        m += xsum(w[i] for i in range(0, k)) == 1
        for i in range(0, k):
            m += w[i] >= 0

        m += w[0] <= x[0]
        for i in range(1, k - 1):
            m += w[i] <= x[i - 1] + x[i]
        m += w[k - 1] <= x[k - 2]
        m += xsum(x[k] for k in range(0, k - 1)) == 1

        for i in range(0, k):
            costs.add_term(w[i] * plf.a[i])
            objective.add_term(w[i] * plf.b[i])

    m += costs <= max_costs
    m.objective = maximize(objective)

    start = monotonic()
    print(m.optimize())
    print(f"DURATION: {(monotonic() - start) * 1000} ms")

    u = []
    v = []
    for (i, plf) in enumerate(choices(plfs, k=10)):
        u_i = sum(ws[i][j].x * plf.a[j] for j in range(0, k))
        v_i = sum(ws[i][j].x * plf.b[j] for j in range(0, k))
        print(f"{min(plf.a)} <= {u_i} <= {max(plf.a)}")
        u.append(u_i)
        v.append(v_i)


def random_plfs(k, n):
    result = []
    for _ in range(n):
        a = np.cumsum(exponential(5, k))
        b = np.cumsum(exponential(5, k))
        result.append(PiecewiseLinearFunction(a, b))
    return result


if __name__ == "__main__":
    k = 8
    n = 3000
    maximize_sum(random_plfs(k, n), round(5 * k * n / 2))
