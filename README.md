<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Installation</a>
<ul>
<li><a href="#sec-1-1">1.1. <code>netcdf 4</code> dependency</a></li>
<li><a href="#sec-1-2">1.2. Mac OS X:</a></li>
<li><a href="#sec-1-3">1.3. Unix/Linux</a></li>
<li><a href="#sec-1-4">1.4. Windows</a></li>
</ul>
</li>
</ul>
</div>
</div>
# Installation

Installation from R

    library(devtools)
    install_bitbucket("neurocdf","kkholst")

## `netcdf 4` dependency

## Mac OS X:

Install either via [homebrew](http://brew.sh):

    brew install netcdf --enable-cxx-compat --enable-fortran

or [macports](http://www.macports.org/%E2%80%8E):

    sudo port install netcdf +gcc47

## Unix/Linux

Should be part of your distribution or compile from [source](http://www.unidata.ucar.edu/downloads/netcdf/index.jsp).

## Windows

Install binary `ncdf4` from <http://cirrus.ucsd.edu/~pierce/ncdf/>
