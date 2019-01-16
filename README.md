
General
-------
This is a stripped down version of XFOIL. All graphical functionality has been removed.

Building and Installing the Python Module
-----------------------------------------
To successfully build and install the Python module a few prerequisites have to be present on your system. First of all,
a working installation of Python is required, of course. The module targets Python 3, and does NOT support Python 2. 
Furthermore, working compilers for C and Fortran have to be installed and on the PATH. On Windows, the build and
installation have ONLY been tested with MinGW, using gcc and gfortran. 

Then, installing XRotor should be as simple as running:

```bash
pip install xfoil
```

Or, from the root of the downloaded repository:

```bash
pip install .
```

On Windows, you may have to force the system to use MinGW. To do so, create a file named `distutils.cfg` in 
`PYTHONPATH\Lib\distutils` with the following contents:

```INI
[build]
compiler=mingw32
```

If you are not able to create this file for your Python environment, it is also possible to force the use of MinGW 
directly when invoking `pip` by calling:

```bash
pip install --global-option build_ext --global-option --compiler=mingw32 xfoil
```

Using the Module
----------------
All XFoil operations are performed using the `XFoil` class. So the first step when using this module is to create an
instance of this class:

```pycon
>>>  from xrotor import XFoil
>>>  xf = XFoil()
```

If this does not produce any errors, the installation should be functioning properly. 
The symmetric NACA 0012 airfoil is included as a test case. It can be loaded into the XFoil library like this:

```pycon
>>>  from xfoil.test import naca0012
>>>  xf.airfoil = naca0012

 Number of input coordinate points: 160
 Counterclockwise ordering
 Max thickness =     0.120008  at x =   0.308
 Max camber    =     0.000000  at x =   0.033

  LE  x,y  =  -0.00000   0.00000  |   Chord =   1.00000
  TE  x,y  =   1.00000   0.00000  |

 Current airfoil nodes set from buffer airfoil nodes ( 160 )
```

Once the airfoil has been loaded successfully it can be analyzed. Let's analyze it for a range of angles of attack, at a
Reynolds number of one million and a Mach number of 0. Let's limit the maximum number of iterations to 40 (the default 
is 20) as well. For the range of angles of attack, we will go from -20 degrees to 20 degrees with steps of 0.5 degrees:

```pycon
>>>  xf.conditions = (1e6, 0)
>>>  xf.max_iter = 40
>>>  a, cl, cd, cm = xf.aseq(-20, 20, 0.5)
```

The XFOIL library should produce a lot of output, which should be familiar to those who have used the original XFOIL 
application before. The final result are lists of angles of attack, `a`, and the corresponding lift coefficients, `cl`, 
drag coefficients, `cd`, and moment coefficients, `cm`. We can now, for example, plot the lift curve for this airfoil:

```pycon
>>>  import matplotlib.pyplot as plt
>>>  plt.plot(a, cl)
>>>  plt.show()
```

This should produce the following figure:

![NACA 0012 Lift Curve](https://github.com/daniel-de-vries/xfoil-python/blob/master/naca0012-lift-curve.png)
 