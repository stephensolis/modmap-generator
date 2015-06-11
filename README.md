modmap-generator
================

This is a generator for Molecular Distance Maps, as seen in the following papers:

- [Mapping the Space of Genomic Signatures](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0119815)
- [An investigation into inter- and intragenomic variations of graphic genomic signatures](http://arxiv.org/abs/1503.00162)

Some pre-generated Molecular Distance Maps may be seen [here](https://dl.dropboxusercontent.com/u/34456847/modmaps3D/index.html).

It uses [FCGR](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC330698/) and supports 
the following distances:

- [Euclidean](http://en.wikipedia.org/wiki/Euclidean_distance)
- [Manhattan](http://en.wikipedia.org/wiki/Manhattan_distance)
- [Pearson Correlation](http://mathworld.wolfram.com/CorrelationCoefficient.html)
- [SSIM](https://ece.uwaterloo.ca/~z70wang/research/ssim/)
- [Descriptors](http://arxiv.org/abs/1503.00162)
- [Approximate Information Distance](http://arxiv.org/abs/cs/0111054)

The SSIM distance may be accelerated with CUDA or OpenCL.

Usage
-----

1. Create folders named fasta, work, and output.
2. Put fasta files in the fasta folder.
3. Run the notebook called generation.nb. It will place distance matrices and 
	some metadata in the output folder, and temporary files in the work folder.
4. Use the notebook called analysis.nb to work with the distance matrices. 
	You may choose the groups for the plot by setting `taxaGroups` under 
	"Initialization + Settings".

You may also run the test suite in the tests folder.

Credits
-------

This code is based on work by [Rallis Karamichalis](http://www.csd.uwo.ca/~rkaramic/), from
[here](https://github.com/rallis/intraSupplemental_Material).

Licence ![License](http://img.shields.io/:license-mit-blue.svg)
-------

    The MIT License (MIT)

    Copyright (c) 2015 Stephen

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.