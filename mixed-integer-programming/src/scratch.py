from collections import namedtuple
from time import monotonic
from random import randrange
from mip import Model, xsum, maximize, BINARY, CONTINUOUS, LinExpr
from numpy.random import exponential
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


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
    def __init__(self, a, b, name=None):
        self.a = a
        self.b = b
        self.name = name

    @property
    def lb(self):
        return min(self.a)

    @property
    def ub(self):
        return max(self.a)

    def __len__(self):
        return len(self.a)

    def __repr__(self):
        return "<{}({})>".format(self.__class__.__name__, self.name or "")


Line = namedtuple("Line", ["cost", "revenue", "plf"])


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

    optimum = []
    for (i, plf) in enumerate(plfs):
        k = len(plf)
        u_i = sum(ws[i][j].x * plf.a[j] for j in range(0, k))
        v_i = sum(ws[i][j].x * plf.b[j] for j in range(0, k))
        optimum.append(Line(cost=u_i, revenue=v_i, plf=plf))
    return optimum


def random_plfs(k, n):
    result = []
    for _ in range(n):
        a = np.cumsum(exponential(5, k))
        b = np.cumsum(exponential(5, k))
        result.append(PiecewiseLinearFunction(a, b))
    return result


def read_bid_landscapes(path):
    df = pd.read_csv(path, delimiter=",")
    for (keyword, group) in df.groupby("keyword"):
        plf = PiecewiseLinearFunction(group.cost.values, group.revenues.values, keyword)
        if len(plf) > 1:
            yield plf


def test():
    plfs = random_plfs(100, 30)
    budget = sum([plf.ub + plf.lb for plf in plfs]) / 2
    maximize_sum(plfs, budget)


def main():
    plfs = list(read_bid_landscapes("data/scenarios_very_large.csv"))
    print(f"Maximizing over {len(plfs)} keywords")
    lb = sum(plf.lb for plf in plfs)
    ub = sum(plf.ub for plf in plfs)
    xs = []
    ys = []
    zs = []
    ds = []
    for b in np.linspace(lb, 1.5 * ub, 20):
        start = monotonic()
        optimum = maximize_sum(plfs, b)
        duration = monotonic() - start
        total = {"cost": 0, "revenue": 0}
        print(len(optimum))
        for line in sorted(optimum, key=lambda line: line.revenue):
            plf = line.plf
            print(
                f"{plf.name}\t: {plf.lb:.2f} <= {line.cost:.2f} <= {plf.ub:.2f}: {line.revenue:.2f}"
            )
            total["cost"] += line.cost
            total["revenue"] += line.revenue

        print(f"Number of keywords: {len(plfs)}")
        print(f"Feasible spend: [{lb:.2f}, {ub:.2f}]")
        print(f"Total spend: {total['cost']:.2f}")
        print(f"Total revenue: {total['revenue']:.2f}")
        print(f"Duration: {duration:.2f}s")
        total["duration"] = duration
        xs.append(b)
        zs.append(total["cost"])
        ys.append(total["revenue"])
        ds.append(duration)

    fig, ax1 = plt.subplots()
    ax1.set_xlabel("budget")
    ax1.set_ylabel("revenue")
    ax1.plot(xs, ys, "bo", linestyle="solid")
    ax1.tick_params(axis="y", labelcolor="blue")

    ax2 = ax1.twinx()
    ax2.set_ylabel("spend")
    ax2.plot(xs, zs, "ro", linestyle="solid")
    ax2.tick_params(axis="y", labelcolor="red")

    fig.tight_layout()

    plt.show()


if __name__ == "__main__":
    test()
