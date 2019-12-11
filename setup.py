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
import os
import platform
import re
import subprocess
import sys

from setuptools import setup
from setuptools.extension import Extension
from setuptools.command.build_ext import build_ext

__version__ = re.findall(
    r"""__version__ = ["']+([0-9\.]*)["']+""",
    open('xfoil/__init__.py').read(),
)[0]

options = {k: 'OFF' for k in ['--opt', '--debug', '--cuda']}
for flag in options.keys():
    if flag in sys.argv:
        options[flag] = 'ON'
        sys.argv.remove(flag)

# Command line flags forwarded to CMake
cmake_cmd_args = []
for f in sys.argv:
    if f.startswith('-D'):
        cmake_cmd_args.append(f)
        sys.argv.remove(f)


class CMakeExtension(Extension):

    def __init__(self, name, cmake_list_dir='.', **kwargs):
        super().__init__(name, sources=[], **kwargs)
        self.cmake_lists_dir = os.path.abspath(cmake_list_dir)


class CMakeBuild(build_ext):

    def build_extensions(self):
        # Ensure that CMake is present and working
        try:
            out = subprocess.check_output(['cmake', '--version'])
        except OSError:
            raise RuntimeError('Cannot find CMake executable')

        for ext in self.extensions:
            extdir = os.path.abspath(os.path.dirname(self.get_ext_fullpath(ext.name)))
            cfg = 'Debug' if options['--debug'] == 'ON' else 'Release'

            cmake_args = [
                '-DCMAKE_BUILD_TYPE=%s' % cfg,
                # Ask CMake to place the resulting library in the directory
                # containing the extension
                '-DCMAKE_LIBRARY_OUTPUT_DIRECTORY_{}={}'.format(cfg.upper(), extdir),
                # Other intermediate static libraries are placed in a
                # temporary build directory instead
                '-DCMAKE_ARCHIVE_OUTPUT_DIRECTORY_{}={}'.format(cfg.upper(), self.build_temp),
                # Hint CMake to use the same Python executable that
                # is launching the build, prevents possible mismatching if
                # multiple versions of Python are installed
                '-DPYTHON_EXECUTABLE={}'.format(sys.executable),
                # Add other project-specific CMake arguments if needed
                # ...
            ]

            # We can handle some platform-specific settings at our discretion
            if platform.system() == 'Windows':
                plat = ('x64' if platform.architecture()[0] == '64bit' else 'Win32')
                cmake_args += [
                    # These options are likely to be needed under Windows
                    '-DCMAKE_WINDOWS_EXPORT_ALL_SYMBOLS=TRUE',
                    '-DCMAKE_RUNTIME_OUTPUT_DIRECTORY_{}={}'.format(cfg.upper(), extdir),
                ]
                # Assuming that Visual Studio and MinGW are supported compilers
                if self.compiler.compiler_type == 'msvc':
                    cmake_args += [
                        '-DCMAKE_GENERATOR_PLATFORM=%s' % plat,
                        ]
                else:
                    cmake_args += [
                        '-G', 'MinGW Makefiles',
                    ]

            cmake_args += cmake_cmd_args

            print(cmake_args)

            if not os.path.exists(self.build_temp):
                os.makedirs(self.build_temp)

            # Config and build the extension
            subprocess.check_call(['cmake', ext.cmake_lists_dir] + cmake_args,
                                  cwd=self.build_temp)
            subprocess.check_call(['cmake', '--build', '.', '--config', cfg],
                                  cwd=self.build_temp)


def readme():
    with open('README.md') as f:
        return f.read()


setup(
    name='xfoil',
    version=__version__,
    description='Stripped down version of XFOIL as compiled python module ',
    long_description=readme(),
    long_description_content_type='text/markdown',
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Natural Language :: English',
        'Operating System :: MacOS :: MacOS X',
        'Operating System :: POSIX :: Linux',
        'Operating System :: Microsoft :: Windows',
        'Programming Language :: Fortran',
        'Programming Language :: Python :: 3 :: Only',
        'Topic :: Scientific/Engineering',
    ],
    keywords='xfoil airfoil aerodynamic analysis',
    url='https://github.com/daniel-de-vries/xfoil-python',
    download_url='https://github.com/daniel-de-vries/xfoil-python/tarball/' + __version__,
    author='Daniël de Vries',
    author_email='contact@daniel-de-vries.com',
    license='GNU General Public License v3 or later (GPLv3+)',
    packages=['xfoil'],
    # package_dir={'': 'src'},
    ext_modules=[CMakeExtension('xfoil.xfoil')],
    cmdclass={'build_ext': CMakeBuild},
    install_requires=['numpy'],
    zip_save=False
)
