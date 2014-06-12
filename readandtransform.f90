module qsort_c_module

	implicit none
	public :: QsortC
	private :: Partition

	contains

	recursive subroutine QsortC(A)
	real, intent(in out), dimension(:) :: A
	integer :: iq

  if(size(A) > 1) then
     call Partition(A, iq)
     call QsortC(A(:iq-1))
     call QsortC(A(iq:))
  endif
end subroutine QsortC

subroutine Partition(A, marker)
  real, intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  real :: temp
  real :: x      ! pivot point
  x = A(1)
  i= 0
  j= size(A) + 1

  do
     j = j-1
     do
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do

end subroutine Partition

end module qsort_c_module

program readandtransform
	use qsort_c_module
	implicit none
	
	character*400									:: ladate,url,filename2,output
	integer											:: intdummy,lookat,count1,i,j,j0
	integer											:: count2,count4=1
	integer											:: delimiter1,delimiter2,delimiter0
	integer											:: delimiter3,delimiter4	
	integer											:: current_index
	integer											:: crit=1,Ncrit,Nurls
	real											:: yr,mm,dd
	character*400									:: buffer,filename
	character (len = 400),dimension(:),allocatable 	:: urls,urlsuniques,dates,rest,crit_of_interest,values
	integer,dimension(:),allocatable				:: derivatives
	real,dimension(:),allocatable					:: leursscores,leursscores_unsorted
	integer											:: datediff
	integer											:: val2=0.0,val1=0.0,deri,count3=0
	real											:: alpha=1.0,beta=1.0,score,somme=0.0,moy=0.0
	real											:: first,second,third,fourth,fifth
	integer											:: fi,si,ti,ri,hi
	
	count1=0
	
	open(1,file='datasets_data.csv', status='old',err=98)
	open(9,file='legende.dat')
	
	do
		read(1,'(A)',end=42) buffer
		count1=count1+1
	end do
	rewind(1)
42 	write(*,*) "datasets_data.csv contient",count1,"lignes"

	close(1)
	
	allocate(urls(count1),dates(count1),rest(count1),values(count1),derivatives(count1))
	open(2,file='datasets_data.csv',status='old',err=98)
	do i=1,count1
		read(2,'(A)') buffer

		delimiter1=index(buffer,',')
		delimiter2=index(buffer(delimiter1+1:),',')
		
		delimiter0=delimiter1
		delimiter1=delimiter2+delimiter1
		
		delimiter2=index(buffer(delimiter1+1:),',')		
 		urls(i)=buffer(delimiter1+1:delimiter2+delimiter1-1)
 		dates(i)=buffer(:delimiter0-1)
 		rest(i)=buffer(delimiter2+delimiter1+1:)
 		
		buffer=rest(i)
		buffer(len_trim(buffer)+1:len_trim(buffer)+1)=","
		Ncrit=1
		j0=0
		do j=1,len_trim(buffer)
			if (buffer(j:j) == ",") then
				if (Ncrit==crit) then
					delimiter4=j
					delimiter3=j0
				end if
				Ncrit=Ncrit+1
				j0=j
			end if
		end do
		values(i)=buffer(delimiter3+1:delimiter4-1)
	end do
	close(2)
	
	count2=1
	do i=2,count1
		if (urls(i) .ne. urls(i-1)) then
			count2=count2+1
		end if
	end do

	write(9,*) count2,"nombre de urls uniques"
	
	allocate(urlsuniques(count2),leursscores(count2),leursscores_unsorted(count2))
	j=1
	urlsuniques(1)=urls(1)
	do i=2,count1
		if (urls(i) .ne. urls(i-1)) then
			j=j+1
			urlsuniques(j)=urls(i)
		end if
	end do
	Nurls=j

	write(9,*) "Les urls uniques sont :"
	do i=1,count2
		write(9,*) i-1,urlsuniques(i)
	end do

	
	! URL 1
	! 1) Extraire l'abscisse
!~ 	write(filename,*) urlsuniques(1)

		buffer=urlsuniques(j)		
		open(10,file='output.dat')
			write(10,*) "#############"
			write(10,*) "#",urls(1)
			do i=1,count1
				buffer=dates(i)
				if (i>1) then
					if (urls(i) .ne. urls(i-1)) then
						write(10,*)
						write(10,*)
						write(10,*) "#",urls(i)
					end if
				end if
				read(values(i),*) val2
								
				if (urls(i) .eq. urls(i-1)) then
					read(values(i-1),*) val1
					count3=count3+1
				else
					deri = 0
					count3=0
					somme=0.0
				end if
				
				if (count3 .eq. 0) then
					somme=val2
				else
					somme=somme+val2
				end if
				if (val2.eq.0) then
					val2=1
					somme=1.0
				end if				
				moy=somme/count3
				deri=(val2-val1)/moy

				score=alpha*(val2/moy)+beta*deri
				if (score < -1.0) score=-1.0
!~ 				write(*,*) trim(dates(i))," ",trim(values(i))," ",deri,score
				write(10,*) trim(dates(i))," ",trim(values(i))," ",deri,score
				
				if (i .ne. count1) then
					if (urls(i) .ne. urls(i+1)) then
						leursscores(count4)=score
						leursscores_unsorted(count4)=score
						count4=count4+1
					end if
				end if
			end do
		close(10)
		
		call QsortC(leursscores)
		first=leursscores(count4-1)
		second=leursscores(count4-2)
		third=leursscores(count4-3)
		fourth=leursscores(count4-4)
		fifth=leursscores(count4-5)
		
		do i=1,count4-1
			if (leursscores_unsorted(i) == first) fi = i
			if (leursscores_unsorted(i) == second) si = i
			if (leursscores_unsorted(i) == third) ti = i
			if (leursscores_unsorted(i) == fourth) ri = i
			if (leursscores_unsorted(i) == fifth) hi = i
		end do
		
		open(20,file='classement.dat')
			write(20,*) trim(urlsuniques(fi))
			write(20,*) trim(urlsuniques(si))
			write(20,*) trim(urlsuniques(ti))
			write(20,*) trim(urlsuniques(ri))
			write(20,*) trim(urlsuniques(hi))
		close(20)

	close(9)
	stop
98	write(*,*) "Fichier introuvable !"	
	stop
100 write(*,*) "Le critère que vous demandez n'existe pas !"
	stop
end program readandtransform
