# Fortran String to Number

This is an initiative in order improvement string to number conversion of Fortran following the [discussions](https://fortran-lang.discourse.group/t/faster-string-to-double/2208) in the fortran community. Which is still very much a work in progress.

The key elements are:
* Transforming the individual strings into its ASCII code integer equivalent.
* Add up the values respecting their respective power of 10th position.
* For float numbers: determine the positions of the decimal place marker and exponent caracters.

## Intended use cases:

* Interpreting a single string:
```fortran
use str2num_m, only: str2real
real(8) :: r
r = str2real("1.234")
r = str2real(" -0.2331726067853964E-01 ")
```
* Interpreting an array of strings:
```fortran
use str2num_m, only: str2real
real(8), allocatable :: r(:)
...
! Loop approach
do i = 1, n
   rval(i) = str2real ( strs(i) )
enddo
! or array approach
rval(:) = str2real ( strs(:) )
```
* stream-lining through a chain of numbers in one string:
```fortran
use str2num_m, only: str2real_p !> pointer version of the str2real function
character(:),allocatable,target :: strs_seq !> Original string
character(len=:), pointer :: ps !> Working pointer
real(8), allocatable :: r(:)
...
ps => strs_seq(1:)
do i = 1, n
   rval(i) = str2real_p ( ps ) !> the pointer is shifted within the function
enddo
! OR
do i = 1, n
   ps => strs(i)(1:)
   rval(i) =  str2real_p( ps )
enddo
```
(a str2int and str2int_p equivalents are also available in the module, which will return only positive integers)
## Performance

This library comes with a test for correctness and a benchmark for execution time. The benchmark only contains one format, the speedup will be different for shorter/other formats.
All tests are a comparison between formatted `read` and `str2real`

Latest test show this implemantation is about 20 times faster than the standard read. (compiler and hardward dependent)

### test correctness:

`fpm test test`

### benchmark execution time:

`fpm test bench --flag "-O3 -march=native"`

`fpm test main_test --flag "-O3 -march=native"`
## Footnote

Authors of the original project [Carl Burkert](https://github.com/Carltoffel)
and collaborators: [Jacob Williams](https://github.com/jacobwilliams) and [Beliavsky](https://github.com/Beliavsky)

Further help from [Tran Quoc Viet](https://fortran-lang.discourse.group/u/tqviet)
