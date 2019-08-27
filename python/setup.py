#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 27 14:25:32 2019

@author: oscar

LUEGO ESTO POR CONSOLO DENTRO DEL DIRECTORIO DE SETUP.py
python3 setup.py build_ext --inplace

"""

from distutils.core import setup, Extension
from Cython.Build import cythonize
import numpy

setup(
    ext_modules=cythonize("EXTRACT_NAM12_GRIBS_CYTHON.pyx"),
    include_dirs=[numpy.get_include()]
)