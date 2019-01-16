# -*- coding: utf-8 -*-
#   Copyright (c) 2019 D. de Vries
#
#   This file is part of XFoil.
#
#   XFoil is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   XFoil is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with XFoil.  If not, see <https://www.gnu.org/licenses/>.
import numpy as np
import os

from ctypes import c_int, c_float, byref, POINTER

here = os.path.abspath(os.path.dirname(__file__))
fptr = POINTER(c_float)


class XFoil(object):

    def __init__(self):
        super().__init__()
        self._lib = np.ctypeslib.load_library('libxfoil', here)
        self._lib.init()
        self._airfoil = None

    @property
    def airfoil(self):
        return self._airfoil

    @airfoil.setter
    def airfoil(self, airfoil):
        self._airfoil = airfoil
        self._lib.set_airfoil(
            np.asfortranarray(airfoil.x.flatten(), dtype=c_float).ctypes.data_as(fptr),
            np.asfortranarray(airfoil.y.flatten(), dtype=c_float).ctypes.data_as(fptr),
            byref(c_int(airfoil.n_coords))
        )

    @property
    def conditions(self):
        re = c_float()
        m = c_float()
        self._lib.get_conditions(byref(re), byref(m))
        return float(re), float(m)

    @conditions.setter
    def conditions(self, conditions):
        self._lib.set_conditions(byref(c_float(conditions[0])), byref(c_float(conditions[1])))

    @property
    def max_iter(self):
        return self._lib.get_max_iter()

    @max_iter.setter
    def max_iter(self, max_iter):
        self._lib.set_max_iter(byref(c_int(max_iter)))

    def reset_bls(self):
        self._lib.reset_bls()

    def a(self, a):
        cl = c_float()
        cd = c_float()
        cm = c_float()

        self._lib.alfa(byref(c_float(a)), byref(cl), byref(cd), byref(cm))

        return cl.value, cd.value, cm.value

    def cl(self, cl):
        a = c_float()
        cd = c_float()
        cm = c_float()

        self._lib.cl(byref(c_float(cl)), byref(a), byref(cd), byref(cm))

        return a.value, cd.value, cm.value

    def aseq(self, a_start, a_end, a_step):
        n = abs(int((a_end - a_start) / a_step))

        a = np.zeros(n, dtype=c_float)
        cl = np.zeros(n, dtype=c_float)
        cd = np.zeros(n, dtype=c_float)
        cm = np.zeros(n, dtype=c_float)

        self._lib.aseq(byref(c_float(a_start)), byref(c_float(a_end)), byref(c_int(n)),
                       a.ctypes.data_as(fptr), cl.ctypes.data_as(fptr), cd.ctypes.data_as(fptr), cm.ctypes.data_as(fptr))

        return a, cl, cd, cm

    def cseq(self, cl_start, cl_end, cl_step):
        n = abs(int((cl_end - cl_start) / cl_step))

        a = np.zeros(n, dtype=c_float)
        cl = np.zeros(n, dtype=c_float)
        cd = np.zeros(n, dtype=c_float)
        cm = np.zeros(n, dtype=c_float)

        self._lib.cseq(byref(c_float(cl_start)), byref(c_float(cl_end)), byref(c_int(n)),
                       a.ctypes.data_as(fptr), cl.ctypes.data_as(fptr), cd.ctypes.data_as(fptr), cm.ctypes.data_as(fptr))

        return a, cl, cd, cm



