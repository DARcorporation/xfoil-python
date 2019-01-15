import numpy as np


class Airfoil(object):

    def __init__(self, x, y):
        super().__init__()
        self.coords = np.ndarray((0, 2))
        self.x = x
        self.y = y

    @property
    def n_coords(self):
        return self.coords.shape[0]

    @property
    def x(self):
        return self.coords[:, 0]

    @x.setter
    def x(self, value):
        v = value.flatten()
        self.coords.resize((v.size, 2))
        self.coords[:, 0] = v[:]

    @property
    def y(self):
        return self.coords[:, 1]

    @y.setter
    def y(self, value):
        v = value.flatten()
        self.coords.resize((v.size, 2))
        self.coords[:, 1] = v[:]
