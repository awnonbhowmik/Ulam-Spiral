!Coded with F90/F95 in CodeBlocks
program ulam_spiral
    implicit none
    integer::i,j,n,x,y,dimn
    integer,allocatable::a(:)
    character(1),allocatable::spiral(:,:)
    character(2)::sstr
    character(10)::fmt

    write(*,*)"Enter the dimension (odd number):"
    read(*,*)dimn
    if(mod(dimn,2)==0)then
        write(*,*)"Odd number required as input"
        stop
    end if

    allocate(a(dimn*dimn))
    allocate(spiral(dimn,dimn))
    do i=1,dimn*dimn
        a(i)=i
    end do
    spiral(dimn,dimn)=" "

    n=1
    x=dimn/2+1
    y=x
    if(isprime(a(n))) spiral(x,y)="*"
    n=n+1

    do i=1,dimn-1,2
        do j=1,i
            x=x+1
            if(isprime(a(n))) spiral(x,y)="*"
            n=n+1
        end do

        do j=1,i
            y=y-1
            if(isprime(a(n))) spiral(x,y)="*"
            n=n+1
        end do

        do j=1,i+1
            x=x-1
            if(isprime(a(n))) spiral(x,y)="*"
            n=n+1
        end do

        do j=1,i+1
            y=y+1
            if(isprime(a(n))) spiral(x,y)="*"
            n=n+1
        end do
    end do

    do j = 1, dimn-1
        x = x + 1
        if(isprime(a(n))) spiral(x, y) = "*"
        n = n + 1
    end do

    write(sstr, "(i0)") dimn
    fmt = "(" // sstr // "(a,1x))"
    do i = 1, dimn
        write(*,fmt) spiral(:, i)
    end do

contains

  function isprime(number)
  logical :: isprime
  integer, intent(in) :: number
  integer :: i

  if(number == 2) then
    isprime = .true.
  else if(number < 2 .or. mod(number,2) == 0) then
    isprime = .false.
  else
    isprime = .true.
    do i = 3, int(sqrt(real(number))), 2
      if(mod(number,i) == 0) then
        isprime = .false.
        exit
      end if
    end do
  end if
end function

end program
