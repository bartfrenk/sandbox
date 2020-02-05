import numpy as np


class DiscreteDistribution:
    def __init__(self, pdf):
        self.pdf = pdf

    def sample(self):
        index = np.searchsorted(np.cumsum(self.pdf[:, 0]) > np.random.rand(), True)
        return self.pdf[index, 1]


class RandomWalk:
    def __init__(self, step):
        self.step = step

    def sample(self, n, start=0):
        return np.cumsum(np.array([self.step.sample() for _ in range(n)])) + start


def polynomial(discrete):
    lo = int(min(np.min(discrete.pdf[:, 1]), 0))
    hi = int(np.max(discrete.pdf[:, 1]))
    coeffs = np.zeros(hi - lo + 1)
    for x in discrete.pdf:
        print(x)
        coeffs[int(x[1] - lo)] = x[0]
    coeffs[-lo] = -1
    return np.poly1d(coeffs)


def ruin_probability(random_walk, lo, hi):
    hi = np.max(random_walk.step.pdf[:, 1])
    lo = np.max(random_walk.step.pdf[:, 1])

    roots = set(polynomial(random_walk.discrete).r)

    # TODO: Set up a system of linear equations to compute the ruin
    # probabilities.
