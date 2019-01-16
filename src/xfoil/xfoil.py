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
    """Interface to the XFoil Fortran routines.

    Attributes
    ----------
    airfoil
    conditions
    max_iter
    """

    def __init__(self):
        super().__init__()
        self._lib = np.ctypeslib.load_library('libxfoil', here)
        self._lib.init()
        self._airfoil = None

    @property
    def airfoil(self):
        """Airfoil: Instance of the Airfoil class."""
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
        """tuple of two floats: Reynolds number and Mach number.

        If Re = 0, XFOIL is run in inviscid mode.
        """
        re = c_float()
        m = c_float()
        self._lib.get_conditions(byref(re), byref(m))
        return float(re), float(m)

    @conditions.setter
    def conditions(self, conditions):
        self._lib.set_conditions(byref(c_float(conditions[0])), byref(c_float(conditions[1])))

    @property
    def max_iter(self):
        """int: Maximum number of iterations."""
        return self._lib.get_max_iter()

    @max_iter.setter
    def max_iter(self, max_iter):
        self._lib.set_max_iter(byref(c_int(max_iter)))

    def reset_bls(self):
        """Reset the boundary layers to be reinitialized on the next analysis."""
        self._lib.reset_bls()

    def repanel(self, n_nodes=160, cv_par=1, cte_ratio=0.15, ctr_ratio=0.2, xt_ref=(1, 1), xb_ref=(1, 1)):
        """Re-panel airfoil.

        Parameters
        ----------
        n_nodes : int
            Number of panel nodes
        cv_par : float
            Panel bunching parameter
        cte_ratio : float
            TE/LE panel density ratio
        ctr_ratio : float
            Refined-area/LE panel density ratio
        xt_ref : tuple of two floats
            Top side refined area x/c limits
        xb_ref : tuple of two floats
            Bottom side refined area x/c limits
        """
        self._lib.repanel(byref(c_int(n_nodes)), byref(c_float(cv_par)),
                          byref(c_float(cte_ratio)), byref(c_float(ctr_ratio)),
                          byref(c_float(xt_ref[0])), byref(c_float(xt_ref[1])),
                          byref(c_float(xb_ref[0])), byref(c_float(xb_ref[1])))

    def filter(self, factor=0.2):
        """Filter surface speed distribution using modified Hanning filter.

        Parameters
        ----------
        factor : float
            Filter parameter. If set to 1, the standard, full Hanning filter is applied. Default is 0.2.
        """
        self._lib.filter(byref(c_float(factor)))

    def a(self, a):
        """Analyze airfoil at a fixed angle of attack.

        Parameters
        ----------
        a : float
            Angle of attack in degrees

        Returns
        -------
        cl, cd, cm : float
            Corresponding values of the lift, drag, and moment coefficients.
        """
        cl = c_float()
        cd = c_float()
        cm = c_float()

        self._lib.alfa(byref(c_float(a)), byref(cl), byref(cd), byref(cm))

        return cl.value, cd.value, cm.value

    def cl(self, cl):
        """"Analyze airfoil at a fixed lift coefficient.

        Parameters
        ----------
        cl : float
            Lift coefficient

        Returns
        -------
        a, cd, cm : float
            Corresponding angle of attack, drag coefficient, and moment coefficient.
        """
        a = c_float()
        cd = c_float()
        cm = c_float()

        self._lib.cl(byref(c_float(cl)), byref(a), byref(cd), byref(cm))

        return a.value, cd.value, cm.value

    def aseq(self, a_start, a_end, a_step):
        """Analyze airfoil at a sequence of angles of attack.

        The analysis is done for the angles of attack given by range(a_start, a_end, a_step).

        Parameters
        ----------
        a_start, a_end, a_step : float
            Start, end, and increment angles for the range.

        Returns
        -------
        a, cl, cd, cm : np.ndarray
            Lists of angles of attack and their corresponding lift, drag, and moment coefficients.
        """
        n = abs(int((a_end - a_start) / a_step))

        a = np.zeros(n, dtype=c_float)
        cl = np.zeros(n, dtype=c_float)
        cd = np.zeros(n, dtype=c_float)
        cm = np.zeros(n, dtype=c_float)

        self._lib.aseq(byref(c_float(a_start)), byref(c_float(a_end)), byref(c_int(n)),
                       a.ctypes.data_as(fptr), cl.ctypes.data_as(fptr), cd.ctypes.data_as(fptr), cm.ctypes.data_as(fptr))

        return a, cl, cd, cm

    def cseq(self, cl_start, cl_end, cl_step):
        """Analyze airfoil at a sequence of lift coefficients.

        The analysis is done for the lift coefficients given by range(cl_start, cl_end, cl_step).

        Parameters
        ----------
        cl_start, cl_end, cl_step : float
            Start, end, and increment lift coefficients for the range.

        Returns
        -------
        a, cl, cd, cm : np.ndarray
            Lists of angles of attack and their corresponding lift, drag, and moment coefficients.
        """
        n = abs(int((cl_end - cl_start) / cl_step))

        a = np.zeros(n, dtype=c_float)
        cl = np.zeros(n, dtype=c_float)
        cd = np.zeros(n, dtype=c_float)
        cm = np.zeros(n, dtype=c_float)

        self._lib.cseq(byref(c_float(cl_start)), byref(c_float(cl_end)), byref(c_int(n)),
                       a.ctypes.data_as(fptr), cl.ctypes.data_as(fptr), cd.ctypes.data_as(fptr), cm.ctypes.data_as(fptr))

        return a, cl, cd, cm



