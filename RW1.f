      !A small demnstration of multiplicative variables of uniformly
      !distributed random variables have a log-normal distribution.
      !Here, we use the random number generator which uses a integer
      !multiplier method. 

      !Author: Ayan Bhattacharjee
      
      implicit double precision (a-h,o-z)
      parameter(mpoint=128,mvar=3,nstep=10*mpoint)
      parameter(a=1.0d0,b=10.0d0,c=1.0d0,d=2.0d0)
      dimension dist(mvar,0:mpoint)
      dimension distf(0:mpoint)
      dimension distw(-mpoint:mpoint)
      parameter (nconf=2**20)


!_____Initial Seeds____!
      iran1=465776153
      iran2=98765451
      iran3=76241905
      ipos0=0
      iL=-mpoint-1
      iLB=-mpoint+1
      iR=mpoint+1
      iRB=mpoint-1
     

      dist(:,:)=0.0d0
      distf(:)=0.0d0
      distw(:)=0.0d0

      do iconf=1,nconf
 12   rp=ranf3(iran3)
      if(rp.ge.1.0d0)go to 12
      ipos0=mpoint*(2.0d0*rp-1.0d0)
      ipos=ipos0
      do i=1,nstep
c     do j=1,mvar
 11   r=ranf1(iran2)
      if(r.ge.1.0d0)go to 11
      if(r.le.0.5d0)then
      irwalk=1
      else
      irwalk=-1
      endif
      ipos=ipos+irwalk
      if(ipos.eq.iL)ipos=iLB
      if(ipos.eq.iR)ipos=iRB
      distw(ipos)=distw(ipos)+1.0d0
c     write(*,*)i,ipos
      enddo
      write(*,*)iconf,ipos0,ipos
      enddo



!      ir=r*mpoint
!      dist(j,ir)=dist(j,ir)+1.0d0
!      enddo
! 21   r1=a+(b-a)*ranf1(iran1)
!      if(r1.ge.1.0d0*b)go to 21
! 22   r2=a+(b-a)*ranf2(iran2)
!      if(r2.ge.1.0d0*b)go to 22
! 23   r3=a+(b-a)*ranf3(iran3)
!      if(r3.ge.1.0d0*b)go to 23
!      fr=r1/r2!/r3
!      frlow=a/b!/b
!      frhi=b/a!/a
!      rangefr=frhi-frlow
!      ifr=fr*mpoint/rangefr
!      distf(ifr)=distf(ifr)+1.0d0
c     print*,i,r1,r2,r3
!      enddo

      do i=-mpoint,mpoint
!      x=dfloat(i)/dfloat(mpoint)
!      write(1111,112)x,(dist(j,i)/dfloat(nstep),j=1,mvar)
!      y=frlow+rangefr*i/dfloat(mpoint)
!      write(2222,111)y,distf(i)/dfloat(nstep)
      write(3333,*)i,distw(i)/dfloat(nstep)/dfloat(nconf)
      enddo
! 112  format(4e15.6)
! 111  format(2e15.6)

      end


      !These are standard multiplicitive Pseudo Random Number Generators with
      !odd seed, having periodicity of > 2^28
*-----RANDOM NUMBER GENERATOR 1 ------------------

      double precision function ranf1(iran1)
      iran1=iran1*1566083941
      if(iran1.lt.0)iran1=iran1+2147483647+1
      iran1=iran1*1566083941
      if(iran1.lt.0)iran1=iran1+2147483647+1
      ranf1=iran1*4.6566128752458D-10
      return
      end

*-----RANDOM NUMBER GENERATOR 2 ------------------

      double precision function ranf2(iran2)
      iran2=iran2*1664525
      if(iran2.lt.0)iran2=iran2+2147483647+1
      iran2=iran2*1664525
      if(iran2.lt.0)iran2=iran2+2147483647+1
      ranf2=iran2*4.6566128752458D-10
      return
      end

*-----RANDOM NUMBER GENERATOR 3 ------------------

      double precision function ranf3(iran3)
      iran3=iran3*16807
      if(iran3.lt.0)iran3=iran3+2147483647+1
      iran3=iran3*16807
      if(iran3.lt.0)iran3=iran3+2147483647+1
      ranf3=iran3*4.6566128752458D-10
      return
      end

