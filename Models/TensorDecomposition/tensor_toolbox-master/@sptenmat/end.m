function e = end(X,k,n)
%END Last index of indexing expression for sptenmat.
%
%   The expression X(end,:) will call END(X,1,2) to determine
%   the value of the first index.
%
%   See also SPTENMAT, SPTENMAT/SUBSREF, END.
%
%MATLAB Tensor Toolbox.
%Copyright 2015, Sandia Corporation.

% This is the MATLAB Tensor Toolbox by T. Kolda, B. Bader, and others.
% http://www.sandia.gov/~tgkolda/TensorToolbox.
% Copyright (2015) Sandia Corporation. Under the terms of Contract
% DE-AC04-94AL85000, there is a non-exclusive license for use of this
% work by or on behalf of the U.S. Government. Export of this data may
% require a license from the United States Government.
% The full license terms can be found in the file LICENSE.txt


if n > 2
  error('Subscript out of range.');
end
e = size(X,k);